rm(list=ls())
##################Tema 1#############################################

requiredPackages <- c("arsenal", "car","chemometrics","corrplot","datarium","gapminder","dplyr","DescTools", "foreign", "e1071", "expss", "GGally", "ggplot2", "haven", 
                      "knitr","plotly", "psych","remotes", "summarytools","ggridges","table1", "tableone", "tidyverse", "SmartEDA", "scales", "caret", 
                      "imputeMissings", "mice","robustbase", "rstatix")

sesion1 <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

sesion1(requiredPackages)
######################################################
#####Ejemplo 1##########################
#####data=mcars##########################
data(mtcars)
#https://rstudio-pubs-static.s3.amazonaws.com/61800_faea93548c6b49cc91cd0c5ef5059894.html
#mpg:Miles/US Gallon
#hp:Gross horsepower
#wt:Weight (lb/1000)
attach(mtcars)
print(mtcars[1:5,])
cars.lm=lm(mpg~hp+wt)
print(summary(cars.lm))
plot(cars.lm$residuals)
abline(h=0)
#####Ejemplo 2##########################
#####data=marketing##########################
#Data are the advertising budget in thousands of dollars along 
#with the sales (in thousands of units). 
#The advertising experiment has been repeated 200 times
data("marketing", package = "datarium")
head(marketing, 4)
#descriptivos#
describe(marketing)
summary(marketing)
#contar nans por variable
na_count <-sapply(marketing, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
print(na_count)
#matriz de dispersión y correlación
options(warn = -1)
library(plotly)
library(GGally)
p <- ggpairs(marketing, title="correlación por pares") 
ggplotly(p)

#matriz correlaciones
(corr <- round(cor(marketing, method='pearson'), 2))

#otra manera de calcular las correlaciones
#library(rstatix)
cor_mat(marketing,method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)
#Corr p-values
cor_pmat(marketing,method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95)
#mapa de calor correlaciones
#library(GGally)
ggcorr(marketing, method = c("everything", "pearson")
       ,label = TRUE, label_size = 3, label_color = "black",
       label_alpha = TRUE) #https://briatte.github.io/ggcorr/, https://search.r-project.org/CRAN/refmans/GGally/html/ggcorr.html
############################
##########################
#modelo de regresión múltiple
model=lm(formula=sales~youtube+facebook+newspaper, marketing)
summary(model)
#Gráfico de residuales
ggplot(data.frame(x = seq(model$residuals), y = model$residuals)) +
  geom_point(aes(x, y)) +
  labs(x = "Index", y = "Residuals", 
       title = paste("Residuals of", format(model$call)))+
  geom_hline(yintercept=0)
#####Ejemplo 3##########################
#####data=simulated##########################
#rnorm(30, mean=10, sd=2) 
#rlnorm(30, log(10), log(2))
x=1:30
y=5*x
y[2]=11
y[3]=17
y[4]=0
y[9]=47
y[13]=73
y[21]=110
datos<-data.frame(x,y)
attach(datos)
plot(x,y)
resultado=ltsReg(y ~ x, data=datos)
print(resultado)

datos$yestimada=resultado$fitted.values

plot(x,y)
abline(ltsReg(y ~ x, data=datos))

#otra manera de hacer el gráfico
ggplot(datos, aes(x,y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = datos, aes(x,yestimada))+
  ggtitle("Regresión robusta simple") 
  

