rm(list=ls())
##################Tema 1#############################################

requiredPackages <- c("arsenal", "car","chemometrics","corrplot", "gapminder","dplyr","DescTools", "foreign", "e1071", "expss", "GGally", "ggplot2", "haven", 
                      "knitr","plotly", "moments","psych","remotes", "summarytools","ggridges","table1", "tableone", "tidyverse", "SmartEDA", "scales")

sesion1 <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

sesion1(requiredPackages)
######################################################
##LOAD DATA
#Data Lending Club -https://www.kaggle.com/wordsforthewise/lending-club. Factores que determinan el Default en los créditos. Modelo de riesgo
Datalc<-read.csv("https://raw.githubusercontent.com/millerjanny/Custom_UNIR/main/Data_LendingClub.csv")
Datalc$Default=recode_factor(Datalc$Default, `1` = "Default", `0` = "Non-default")
######################################################
######Medidas de localizaci?n###
# Resumen de todas las variables
summary(Datalc$dti_n)

# Sample median

median(Datalc$dti_n)

# First quartile

(Q1=quantile(Datalc$dti_n,probs = 0.25))

# Second quartile (= Median)

quantile(Datalc$dti_n,probs = 0.50)

# Third quartile

(Q3=quantile(Datalc$dti_n,probs = 0.75))

# quantiles

quantile(Datalc$dti_n,probs=c(0.25,0.5,0.75))

# Interquartile range

(IQR_dti = Q3 - Q1)

# Lower and upper limits

Q1-1.5*IQR_dti
Q3+1.5*IQR_dti
boxplot(Datalc$dti_n)

# Boxplots 

boxplot(Datalc$int_rate)
Q1 = quantile(Datalc$int_rate,probs = 0.25)
Q2 = quantile(Datalc$int_rate,probs = 0.50)
Q3 = quantile(Datalc$int_rate,probs = 0.75)
Q1-1.5*(Q3-Q1)
Q3+1.5*(Q3-Q1)

#Boxplot con ggplot()
ggplot(Datalc, aes(x = "", y = int_rate)) + 
  geom_boxplot()


ggplot(Datalc, aes(y = int_rate)) + 
  geom_boxplot(fill = 2,           # Color caja
               alpha = 0.5,        # Transparencia
               color = 1,          # Color del borde
               outlier.colour = 2) # Color atípicos

#Quitar Outliers

Q1 <- quantile(Datalc$int_rate, .25)
Q3 <- quantile(Datalc$int_rate, .75)
IQR <- IQR(Datalc$int_rate)
no_outliers_int_rate <- subset(Datalc, Datalc$int_rate> (Q1 - 1.5*IQR) & Datalc$int_rate< (Q3 + 1.5*IQR))
dim(Datalc) 
dim(no_outliers_int_rate)
boxplot(Datalc$int_rate)
boxplot(no_outliers_int_rate$int_rate)

#otra manera
boxplot(Datalc$int_rate)
out<-boxplot(Datalc$int_rate)$out
no_outliers_int_rate_1<-Datalc[-which(Datalc$int_rate %in% out),]

####identificar y ajustar atípicos al P5 y P95############
Datalc$int_rate_p5_95<- squish(Datalc$int_rate, quantile(Datalc$int_rate, c(.05, .95)))
summary(Datalc$int_rate_p5_95)
quantile(Datalc$int_rate,probs=c(0.05,0.95))
boxplot(Datalc$int_rate,Datalc$int_rate_p5_95)

#limitando a un num sd##########################
mean=mean(Datalc$int_rate)
std=sd(Datalc$int_rate)
Datalc$int_rate_3sd<-squish(Datalc$int_rate,c(mean-(3*std),mean+(3*std)))

##############
# Box-plot discriminando por variable categórica agregando la media 
options(warn = -1)
Datalc%>%
  ggplot(aes(Default,int_rate, fill=Default)) +
  geom_boxplot() +
  stat_summary(fun="mean")+
  theme(legend.position = "none")

#####Medidas de forma
skewness(Datalc$dti_n)
skewness(Datalc$annual_inc) 
skewness(Datalc$int_rate)
e1071::kurtosis(Datalc$annual_inc,type=1)
#https://search.r-project.org/CRAN/refmans/datawizard/html/skewness.html

#####Medidas de forma por variable cualitativa
#library(moments)
Datalc %>% 
  group_by(Default) %>%
  summarise(Skew = skewness(annual_inc), Kurtosis = kurtosis(annual_inc))