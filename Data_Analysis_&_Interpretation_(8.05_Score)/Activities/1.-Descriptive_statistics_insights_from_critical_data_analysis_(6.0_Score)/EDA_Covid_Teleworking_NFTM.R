#' @Autor
#' Nicolas Felipe Trujillo Montero, CUA Data Analyst, Analisis e Interpretacion de los datos
#'
#' @Titulo
#' ANALISIS DESCRIPTIVO ACERCA DEL TELETRABAJO EN EL ANO 2020

# Libreria para leer Dataframes
library(readr)

# Libreria para manejo de Dataframes
library(dplyr)

# Libreria para manejo de ggpairs
# install.packages("GGally")
library(GGally)

# Libreria para el plot
# install.packages("ggplot2")
library(ggplot2)

# Libreria para el uso de la kurtosis
# install.packages("moments")
library(moments)

# Libreria para usar la moda
# install.packages("DescTools")               
library("DescTools")                         

# Cargamos en memoria el dataframe original
x2020_rws <- read_csv("data/2020_rws.csv",locale=locale(encoding="latin1"))

# Eliminamos filas repetidas y filas con valores NaN
# x2020_rws <- na.omit( x2020_rws[!duplicated(x2020_rws), ] )
dim(x2020_rws)

# Los valores NA no aportan nada por lo que los borramos
x2020_rws[which(is.na(x2020_rws)),]
x2020_rws <- na.omit( x2020_rws[!duplicated(x2020_rws), ] )

# Vamos a observar todas las variables 
names(x2020_rws)
vars <- names(x2020_rws)[c(1,2,3,4,6,10,13,20,29)]

# Filtramos por columnas 
x2020_rws <- x2020_rws[, vars]

#################################################################
# Vamos a analizar el primer problema propuesto:
# Analisis de la poblacion para ver el interes por seguir 
# trabajando en remoto
#################################################################

perc_hours_work_2020 <- x2020_rws[,vars[7]]
unique(perc_hours_work_2020)

# Primero analizamos lo que trabajaron cada persona (Ordenado de 100% a 0%)

ggpairs(perc_hours_work_2020) 

# Vamos a ver como esta el sesgo de los datos (ordenado de 0% a 100%)
# Para utilizar la funcion de sesgo, necesitamos discretizar 
# y poner los valores numéricos

convert_percent <-function(elem){
  if (elem == "Less than 10% of my time") {return (0)} 
  else if (elem == "10%") {return (1)} 
  else if (elem == "20%") {return (2)} 
  else if (elem == "30%") {return (3)} 
  else if (elem == "40%") {return (4)} 
  else if (elem == "50% - I spent about half of my time remote working") {return (5)} 
  else if (elem == "60%") {return (6)} 
  else if (elem == "70%") {return (7)} 
  else if (elem == "80%") {return (8)} 
  else if (elem == "90%") {return (9)} 
  else if (elem == "100% - I spent all of my time remote working") {return (10)} 
}

vec_aux <- apply(perc_hours_work_2020, MARGIN=1, FUN=convert_percent)

# Los valores a continuacion verifican el sesgo hacia la izquierda

skewness(vec_aux)
mean(vec_aux)
median(vec_aux)
Mode(vec_aux)[1]

# Por último, vamos a realizar lo mismo para la variable que muestra
# el tiempo que le hubiera gustado a las personas trabajar el año pasado
# y la variable que dice que si el Covid-19 se acabara cuanto tiempo les
# gustaría seguir trabajando vía telemática. 

wished_perc_hours_work_2020 <- x2020_rws[,vars[8]]

ggpairs(wished_perc_hours_work_2020) 

noCovid_perc_hours_work_2020 <- x2020_rws[,vars[9]]

ggpairs(noCovid_perc_hours_work_2020) 

#################################################################
# Vamos a analizar el segundo problema propuesto:
# Analisis de la poblacion para ver el sesgo en el trabajo de 
# mujeres y hombres en remoto
#################################################################

man_df <- x2020_rws %>% filter( x2020_rws$`What is your gender?` == "Male" )

woman_df <- x2020_rws %>% filter( x2020_rws$`What is your gender?` == "Female" )

ggplot(x2020_rws, aes(x = x2020_rws$`Thinking about your current job, how much of your time did you spend remote working last year?`,
                      fill = x2020_rws$`What is your gender?`)) + 
                      geom_bar() +
                      guides(fill = guide_legend(title = "Género")) +
                      labs(x='Porcentaje de trabajo de Mayor a Menor', y='Frecuencia')
