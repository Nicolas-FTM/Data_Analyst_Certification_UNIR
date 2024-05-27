rm(list=ls())
##################Tema 1#############################################

requiredPackages <- c("arsenal", "car","chemometrics","corrplot","datarium","gapminder","dplyr","DescTools", "foreign", "e1071", "expss", "GGally", "ggplot2", "haven", 
                      "knitr","plotly", "psych","remotes", "summarytools","ggridges","table1", "tableone", "tidyverse", "SmartEDA", "scales", "caret", 
                      "imputeMissings", "mice")

sesion1 <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

sesion1(requiredPackages)
######################################################
#####Ejemplo 1##########################
#El paquete gapminder contiene un fichero de datos de población, esperanza de vida y renta per cápita de los países del mundo entre 1952 y 2007.
#La fundación Gapminder es una organización sin fines de lucro con sede en Suecia que promueve el desarrollo global mediante el uso de estadísticas.
library(gapminder)
# Descripción de variables
# country: factor with 142 levels	
# continent: factor with 5 levels	
# year: 1952-2007	
# lifeExp: life expectancy at birth
# pop: total population
# gdpPercap: per-capita GDP

#Gráfico de dispersión
gap=data.frame(gapminder)
ggplot(gap, aes(y=lifeExp, x=gdpPercap)) + 
  geom_point()+
  geom_smooth(method=lm)

#Gráfico de dispersión
gap=data.frame(gapminder)
ggplot(gap, aes(y=lifeExp, x=log(gdpPercap))) + 
  geom_point()+
  geom_smooth(method=lm)

#Correlación
cor(gap$lifeExp, gap$gdpPercap)
cor(gap$lifeExp, log(gap$gdpPercap))
cor(gap$lifeExp, log(gap$gdpPercap), method = "spearman")
cor(gap$lifeExp, log(gap$gdpPercap), method = "kendall")

#Modelo de regresi�n simple
model=lm(formula=lifeExp~log(gdpPercap), data=gap)
#resumen del modelo
summary(model)
#######Gráfico de residuos###################
ggplot(data.frame(x = seq(model$residuals), y = model$residuals)) +
  geom_point(aes(x, y)) +
  labs(x = "Index", y = "Residuals", 
       title = paste("Residuals of", format(model$call)))
#######Gráfico de residuos###################
ggplot(data.frame(x = log(gap$gdpPercap), y = model$residuals)) +
  geom_point(aes(x, y)) +
  labs(x = "log(gap$gdpPercap)", y = "Residuals", 
       title = paste("Residuals of", format(model$call)))

##################################################################
#####Ejemplo 2##########################

data("marketing", package = "datarium")
head(marketing, 4)
#pairplot
library(ggplot2)
library(GGally)
ggpairs(marketing)
#Gráfico de dispersión
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth(method=lm)
#correlación
cor(marketing$sales, marketing$youtube)
#Modelo de regresión simple
model1=lm(formula=sales~youtube, marketing)
#resumen del modelo
summary(model1)
#######Gráfico de residuos###################
ggplot(data.frame(x = seq(model1$residuals), y = model1$residuals)) +
  geom_point(aes(x, y)) +
  labs(x = "Index", y = "Residuals", 
       title = paste("Residuals of", format(model1$call)))
#######Gráfico de residuos###################
ggplot(data.frame(x = marketing$youtube, y = model1$residuals)) +
  geom_point(aes(x, y)) +
  labs(x = "youtube", y = "Residuals", 
       title = paste("Residuals of", format(model1$call)))


######################################################
#####Tablas de frecuencia de variables categóricas
##LOAD DATA
#Data Lending Club -https://www.kaggle.com/wordsforthewise/lending-club. Factores que determinan el Default en los créditos. Modelo de riesgo
Datalc<-read.csv("https://raw.githubusercontent.com/millerjanny/Custom_UNIR/main/Data_LendingClub.csv")
Datalc$Default=recode_factor(Datalc$Default, `1` = "Default", `0` = "Non-default")
# Exploración inicial

# Frecuencias tablas cualitativas

table(Datalc$purpose, Datalc$Default)

#Totales filas y columnas
#margin.table():Para una tabla de contingencia en forma de matriz, calcule la suma de las entradas de la tabla para un índice dado.

margin.table(table(Datalc$purpose, Datalc$Default), margin = 2)
margin.table(table(Datalc$purpose, Datalc$Default), margin = 1)
addmargins(table(Datalc$purpose, Datalc$Default))

## relativo total

prop.table(table(Datalc$purpose, Datalc$Default))

## relativo fila

prop.table(table(Datalc$purpose, Datalc$Default), margin=1)

## relativo columna

prop.table(table(Datalc$purpose, Datalc$Default), margin=2)

#Resumen de todas las variables cualitativas
#https://www.rdocumentation.org/packages/expss/versions/0.10.7/topics/tables

(cuali_summary<-Datalc %>% tab_cols(Default) %>% tab_cells(emp_length,home_ownership_n,purpose, total()) %>% tab_stat_rpct() %>% 
    tab_pivot%>% tab_caption("Resumen de variables cualitativas"))
##############
#algunos gráficos
ggplot(Datalc, aes(x = term)) + 
  geom_bar(position = "dodge")  +  #position = "dodge", to have a side-by-side (i.e. not stacked) barchart
  theme_bw()

ggplot(Datalc, aes(x = term, fill = Default)) + 
  geom_bar(position = "dodge")  +  #position = "dodge", to have a side-by-side (i.e. not stacked) barchart
  theme_bw()

Datalc %>% 
  count(term = factor(term), Default = factor(Default)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = term, y = pct, fill = Default, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)


ggplot(Datalc, aes(x= Default,  group=term)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Default") +
  facet_grid(~term) +
  scale_y_continuous(labels = scales::percent)
