rm(list=ls())
##################SESIÓN 3#############################################

requiredPackages7 <- c('DescTools', 'dlookr','dplyr', 
                       'EnvStats', 'mosaic', 'rcompanion', 'pastecs',
                       'samplingbook', 'TeachingDemos')

sesion7 <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

sesion7(requiredPackages7)

##################################
#################TLC##############
#https://opendatascience.com/central-limit-theorem-r
#Con una Bin(n=4,p=0.05)

n <- 4     # Number of trials (population size)
p<-0.05
s <- 2000  # Number of simulations
m <- c(20, 100, 500, 1000)
EX <- n*p
VarX <- n*p*(1-p)
Z_score <- matrix(NA, nrow = s, ncol = length(m))
for (i in 1:s){
  for (j in 1:length(m)){ # loop over sample size
    samp <- rbinom(n = m[j], size = n, prob = p)
    sample_mean <- mean(samp) # sample mean
    # Calculate Z score for mean of each sample size
    Z_score[i,j] <- (sample_mean-EX)/sqrt(VarX/m[j]) 
  }
}

par(mfrow=c(2,2)) 
for (j in 1:4){
  hist(Z_score[,j], xlim=c(-5,5), 
       freq=FALSE, ylim=c(0, 0.5),
       ylab="Probability", xlab="", 
       main=paste("Sample Size =", m[j]))
  # Density curve
  x <- seq(-4, 4, by=0.01)
  y <- dnorm(x)
  lines(x, y, col="blue")}
###################################

###################################
######Intervalos de confianza######
###################################
Datalc<-read.csv("https://raw.githubusercontent.com/millerjanny/UAM2022/main/Data_LendingClub.csv")

####para la media con sigma conocida####
#library(BSDA)
library(pastecs)
format(stat.desc(Datalc$int_rate), scientific = FALSE,digits = 3)
library(TeachingDemos) 
z.test(Datalc$int_rate, sd=sd(Datalc$int_rate))$conf.int  

####para la media con sigma desconocida####
t.test(Datalc$int_rate,conf.level=0.95)$conf.int 

library(DescTools)
MeanCI(Datalc$int_rate,conf.level=0.95)

library(rcompanion)
groupwiseMean(int_rate ~ 1, 
              data=Datalc,
              conf=0.95, 
              digits=5)

groupwiseMean(int_rate ~ Default, 
              data=Datalc,
              conf=0.95, 
              digits=5)
#normality
ggpubr::ggqqplot(Datalc$int_rate)
hist(Datalc$int_rate)
stats::shapiro.test(Datalc$int_rate)#size menor a 5000
dlookr::normality(Datalc, int_rate, sample = 5000)
options(warn=-1)#0
dgof::ks.test(Datalc$int_rate, 'pnorm')
#alternativa no param�trica
wilcox.test(Datalc$int_rate, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)
####Para la proporción####
library(dplyr)
library(mosaic)
Datalc$Default=recode_factor(Datalc$Default, `1` = "Default", `0` = "Non-default")
prop.table(table(Datalc$Default))
prop.test(~Default, data=Datalc, success='Default', conf.level=0.95)$conf.int
####para la varianza####
library(EnvStats)
var(Datalc$int_rate)
varTest(Datalc$int_rate,conf.level=0.95)$conf.int 

####para la diferencia de proporciones####

prop.table(table(Datalc$term, Datalc$Default), margin=1)
prop.test(~Default|term, data=Datalc, success='Default', conf.level=0.95)

####para la diferencia de medias###
#igualdad de varianzas int_rate por categoría Default

by(Datalc$int_rate,Datalc$Default,var)
var.test(int_rate~ Default, data = Datalc)$conf.int

by(Datalc$int_rate,Datalc$Default, mean)

boxplot(int_rate ~ factor(Datalc$Default), data = Datalc)

t.test(int_rate ~ Default, data = Datalc, var.equal = FALSE)$conf.int
#############################

####Ejemplo para la media con sigma desconocida####
#h_s=Tiempo de estudio en horas
h_s=c(2.4,2.2,2.5,3,3.2,3.3,3,3.4,3.2,3.3)
t.test(h_s, conf.level=0.95)$conf.int 

######Tamaño de muestra######
#############################
library(samplingbook)
####media#######
#Ejemplo de la clase:tamaño muestral  mínimo necesario (IC 95%) para estimar la media de la estatura 
#de mujeres policías con una precisión de 1cm 
error=1
confianza=0.95
N=1000
s=4.30
sample.size.mean(error,s,N,confianza)

####Proporción#######
#utils::data()
library(samplingbook)
data(election)
#DataFrame con el número de ciudadanos con derecho a voto y los resultados de las elecciones en 2002 y 2005 para el Bundestag alemán, la primera cámara del parlamento alemán.
#SPD_02:percentage for the Social Democrats SPD in 2002
# tamaño de muestra mínimo para estimar la P de votantes social demócratas hoy
confianza=0.95
error=0.05
N=300
(P=mean(election$SPD_02))
#P=0.5
sample.size.prop(e=error, P=P, N=N,level=confianza)

