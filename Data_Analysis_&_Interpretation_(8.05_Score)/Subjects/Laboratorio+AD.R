rm(list=ls())
##################Laboratorio#############################################

requiredPackages <- c("arsenal", "car", "corrplot", "dplyr", "DescTools", "foreign", "e1071", "expss", "GGally", "ggplot2", "haven", 
                      "knitr","mosaic", "plotly", "table1", "tableone", "tidyverse", "SmartEDA","nortest","ggpubr", "questionr")

laboratorio <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

laboratorio(requiredPackages)

#-------------------------------------------------------------------#
#---------------------LOAD DATA-------------------------------------#
#-------------------------------------------------------------------#
#Experimento de microahorro
Datasav<-read.csv("https://raw.githubusercontent.com/millerjanny/UAM2022/main/SavingData.csv")


#PROMBEF_n: promedio de ahorro previo a la intervenci?n
#PROMAFT_n; promedio de ahorro posterior a la intervenci?n
#TRATAMIENTO_N: Tipo de intervenci?n
#af_inver: Si fue afectado por ola invernal o no
#asisten: n?mero de m?dulos a los que asiste. Programa de EF
#EDAD: edad en a?os cabeza de hogar
#GENERO: F o M
#TIPOPOBLACION: Tipo de vulnerabilidad. Desplazados o no
#CATEGORIA1: Tipo de ahorrador
#ZONAREG: Regi?n geogr?fica del pa?s
#NUM_NUC: N?mero de personas que conforman el n?cleo del hogar
#Puntaje: Puntaje de vulnerabilidad. Mayor puntaje m?s vulnerable.

#-------------------------------------------------------------------#
#---------------------Exploraci?n inicial y preprocesamiento--------#
#-------------------------------------------------------------------#
View(Datasav)
head(Datasav)
glimpse(Datasav)

names(Datasav)
names(Datasav)[1] <- "PROMBEF_n"
names(Datasav)

dim(Datasav)
#---valores faltantes--------------
sapply(Datasav, function(x) sum(is.na(x)))
df <- Datasav[!(is.na(Datasav$PROMBEF_n)), ]
sapply(df, function(x) sum(is.na(x)))
#-----recodificar el tratamiento----
table(df$TRATAMIENTO_N)
df$trata_new <- recode_factor(df$TRATAMIENTO_N, 'EDUCACION + INCENTIVO'='Tratados', 
                              'EDUCACION'='Tratados', 'INCENTIVO'='Tratados',
                              'CONTROL'='No_Tratados')
#-----recodificar el desplazados---
table(df$TIPOPOBLACION)
df$TIPOPOBLACION=recode_factor(df$TIPOPOBLACION, 'SISBEN NIVEL1'='NO DESPLAZADOS')
#-----creaci?n crecimiento del ahorro----
df$c_Ahorro = (df$PROMAFT_n - df$PROMBEF_n)
df$c_positivo=case_when(df$c_Ahorro > 0 ~ 'crec positivo',
                                     df$c_Ahorro<= 0 ~ 'No crec positivo')
#-----validar---
#1. Proporci?n de los que incrementan el ahorro supera el 50%
#2. Crecimiento del ahorro es diferente en hombres y mujeres
#3. Es mayor el crecimiento del ahorro en desplazados que en no desplazados
#4. La proporci?n de los que presentan un crecimiento del ahorro es diferente por g?nero
#5. La intervenci?n de EF tuvo un efecto positivo en el nivel de ahorro 
#5.1. Media del crecimiento del ahorro es postivo (df$c_Ahorro)
#6 Hay mayor crecimiento de ahorro entre tratados frente a los no tratados
#7 Hay mayor nivel de ahorro  posterior a la intervención (PROMAFT_n) en los individuos 
#no afectados por la ola invernal, en comparación con los afectados por ola invernal.


#1. Proporci?n de los que incrementan el ahorro supera el 50%

barplot(prop.table(table(df$c_positivo)),
        beside=TRUE,legend.text=TRUE, ylim=c(0,1),ylab="Proportions")

#library(mosaic)
prop.test(~c_positivo, data=df, p=0.5, success='crec positivo', alternative = 'greater')


#2. Crecimiento del ahorro es diferente en hombres y mujeres

ggplot(data = df, aes(x =GENERO, y= c_Ahorro)) +
  geom_boxplot(alpha = 0) +
  coord_cartesian(ylim = c(-50000, 50000))+
  stat_summary(fun.y="mean")
  ggtitle("Crec. Ahorro por género")

var.test(c_Ahorro ~ GENERO, data = df)
t.test(c_Ahorro ~ GENERO, data = df, alternative='two.sided', var.equal = FALSE)


#3. Es mayor el crecimiento del ahorro en desplazados que en no desplazados

table(df$TIPOPOBLACION)
df$TIPOPOBLACION <- relevel(as.factor(df$TIPOPOBLACION),"DESPLAZADOS")
levels(df$TIPOPOBLACION)

options(warn = -1)
ggplot(data = df, aes(x =TIPOPOBLACION, y= c_Ahorro)) +
  geom_boxplot(alpha = 0) +
  coord_cartesian(ylim = c(-50000, 50000))+
  stat_summary(fun="mean")+
  ggtitle("Crec. Ahorro por desplazamiento")
  

var.test(c_Ahorro ~ TIPOPOBLACION, data = df)
t.test(c_Ahorro ~ TIPOPOBLACION, data = df, alternative='greater', var.equal = FALSE)

#4. La proporci?n de los que presentan un crecimiento del ahorro es diferente por g?nero
library(scales)

prop.table(table(df$GENERO, df$c_positivo), margin=1)

P <- prop.table(table(df$GENERO, df$c_positivo), margin=1)
ggplot(as.data.frame(P), aes(x = Var2, y = Freq, fill = Var1)) +
  scale_y_continuous(labels=percent_format())+
  geom_bar(stat="identity", position = "dodge")+
  ylab("Percent")+
  xlab("Crec. Ahorro")+
  labs(fill = "Género")

prop.test(c_positivo~GENERO, data=df, success='crec positivo', alternative = 'two.sided')

#5. La intervenci?n de EF tuvo un efecto positivo en el nivel de ahorro 
#Muestras relacionadas

dt1 <- data.table( id = rep("BEF", length(df$PROMBEF_n) ),
                   dist = df$PROMBEF_n )
dt2 <- data.table( id = rep("AFT", length(df$PROMAFT_n) ),
                   dist = df$PROMAFT_n )
dt <- rbindlist( list( dt1, dt2 ) )
ggplot( dt, aes( x = dist, fill = id ) ) +
  geom_histogram(alpha = .3, bins=150)


ggplot(dt, aes(x=dist, fill=id))+
  geom_density(alpha=.5)

ggplot(data = dt, aes(x =id, y= dist)) +
  geom_boxplot(alpha = 0) +
  coord_cartesian(ylim = c(0, 50000))+
  stat_summary(fun.y="mean")
  ggtitle("Ahorro After vs Before")

t.test(df$PROMAFT_n, df$PROMBEF_n, paired = TRUE, alternative = "greater")

#Prueba de normalidad
qqnorm(df$c_Ahorro)
#qqline(df$c_Ahorro, col = "steelblue", lwd = 2)

ks.test(df$c_Ahorro, 'pnorm') 
ad.test(df$c_Ahorro)
#Alternativa no param?trica
wilcox.test(df$PROMAFT_n, df$PROMBEF_n, data = df, exact = FALSE,
            alternative = "greater", paired = TRUE)
#5.1. Media del crecimiento del ahorro es postivo (df$c_Ahorro)

ggplot(data = df, aes(x=factor(0),y= c_Ahorro)) +
  geom_boxplot(alpha = 0) +
  coord_cartesian(ylim = c(-5000, 50000))+
  stat_summary(fun.y="mean")
ggtitle("C. de ahorro")

t.test(df$c_Ahorro, mu=0, alternative='greater')

#6 Hay mayor crecimiento de ahorro en tratados frente a los no tratados
#(Ayuda #2 o #3)
#Gráfico
ggplot(data = df, aes(x =trata_new, y= c_Ahorro)) +
  geom_boxplot(alpha = 0) +
  coord_cartesian(ylim = c(-50000, 50000))+
  stat_summary(fun.y="mean") 
#Test
var.test(c_Ahorro ~ trata_new, data = df) # 
t.test(c_Ahorro ~ trata_new, data = df, alternative='greater', var.equal = FALSE)
table(df$trata_new) 

#7 Hay mayor nivel de ahorro  posterior a la intervención (PROMAFT_n) en los individuos 
#no afectados por la ola invernal, en comparación con los afectados por ola invernal.
table(df$Af_inver) 
df$Af_inver <- relevel(as.factor(df$Af_inver),"NO AFECTADO")
levels(df$Af_inver)

ggplot(data = df, aes(x=Af_inver,y= PROMAFT_n)) +
  geom_boxplot(alpha = 0) +
  coord_cartesian(ylim = c(0, 50000))+
  stat_summary(fun.y="mean")
ggtitle("Promedio After")

var.test(PROMAFT_n ~ Af_inver, data = df)
t.test(PROMAFT_n ~ Af_inver, data = df, alternative='greater', var.equal = FALSE)
