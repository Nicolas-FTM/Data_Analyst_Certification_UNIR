# cargar paquete readr
library(readr)
library(dplyr)

# Leemos dataframe
data <- read.csv("datosCsv.csv", sep = ";", header = TRUE)

# Vemos datos
View(data)

# Analizamos distro
esp <- data %>% filter(data$Nacionalidad == "Espanola") %>% select(Periodo, Total)
ext <- data %>% filter(data$Nacionalidad == "Extranjera") %>% select(Periodo, Total)
esp$Total <- as.numeric(gsub(",", ".", gsub("\\.", "", esp$Total)))
ext$Total <- as.numeric(gsub(",", ".", gsub("\\.", "", ext$Total)))

# Histograma
hist(esp$Total, main = "Histograma de frecuencias de Espanoles", ylab = "Frecuencia de Nacimiento", xlab = "Porcentaje de Nacimiento")
hist(ext$Total, main = "Histograma de frecuencias de Extranjeros", ylab = "Frecuencia de Nacimiento", xlab = "Porcentaje de Nacimiento")

# Funciones con respecto al tiempo
plot(esp$Periodo, esp$Total, xlab = "Años", ylab = "Porcentaje de Nacimiento", main = "Gráfico de Nacimiento para Españoles")
plot(ext$Periodo, ext$Total, xlab = "Años", ylab = "Porcentaje de Nacimiento", main = "Gráfico de Nacimiento para Extranjeros")

# Spearman's Rank Correlation
corr <- cor.test(x=esp$Total, y=esp$Total, method = 'spearman')

# Chi Square Test
chisq <- chisq.test(esp$Total,esp$Total, correct = FALSE)
