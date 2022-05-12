# Importar dataset en. CSV

data <- read.csv("D:/GIT/attacks/data.csv", sep=";")
View(data)

# Descargar e instalar la extension GIT para subir el dataset https://git-lfs.github.com/
# Insertar el código en el Termina:  > git lfs track "*.csv" > git add data.csv

# Filtro de Variables según POSIXct, factor y Ch 


data$StartT <- as.POSIXct(data$Starttime, origin="1970-01-01")
data$Starttime <- NULL #eliminar columna
data$Lasttime <- NULL #eliminar columna

data$Category <- as.factor(data$Attackcategory)
data$Subcategory <- as.factor(data$Attacksubcategory)
data$Protocols <- as.factor(data$Protocol)

data$Attackcategory <- NULL #eliminar columna
data$Attacksubcategory <- NULL #eliminar columna
data$Protocol <- NULL #eliminar columna
data$newcategory <- NULL #eliminar columna



# Analizaremos los Levels para nuestro caso IPs, Protocolos, Categoria de Amenazas


##Segmentamos las columnas
#Verificar duplicados y unificar a una sola variable en TIPO DE AMENAZA EN CATEGORIA

levels(data$Category)
data[data$Category == "",]  #PARA SELECCIONAR LAS FILAS VACÍAS

data %>% group_by(Category) %>% summarise( n=n()) #PARA AGRUPAR Y VERIFICAR LA CANTIDAD  Y REPETIDOS

data$necategory <- as.character(data$Category) # CAMBIO DE VARIABLE POR UN CARATER PARA UNIFICAR

unique(data$necategory)

data$necategory <- str_replace(data$necategory,"Backdoors", "Backdoor" ) # REMPLAZAR UNA VARIABLE POR OTRA
data$necategory <- str_replace(data$necategory,".", NA )
data$necategory <- str_replace(data$necategory," Fuzzers", " Fuzzers " )
data$necategory <- str_replace(data$necategory,"  Shellcode  ", "Shellcode" )
data$necategory <- str_replace(data$necategory," Shellcode ", "Shellcode" )
data$necategory <- str_replace(data$necategory," Fuzzers ", "Fuzzers" )
data$necategory <- str_replace(data$necategory,"Fuzzers ", "Fuzzers" )
data$necategory <- str_replace(data$necategory,"Fuzzers ", "Fuzzers" )
data$necategory <- str_replace(data$necategory," Reconnaissance ", "Reconnaissance" )


data$necategory <- str_replace(data$necategory, "[.]", "NA")
data$necategory <- str_replace(data$necategory, "[NA]", NA_character_) # 

data %>% group_by(necategory) %>% summarise( n=n()) #NÚMERO DE ATAQUES POR CATEGORIA
unique(data$necategory) #VERIFICAR DUPLICIDAD 

#Verificar duplicados y unificar a una sola variable en IPs

levels(data$SourceIP)
data[data$SourceIP == "",]  #PARA SELECCIONAR LAS FILAS VACÍAS

data %>% group_by(SourceIP) %>% summarise( n=n()) #PARA AGRUPAR Y VERIFICAR LA CANTIDAD  Y REPETIDOS
unique(data$SourceIP)

#Verificar duplicados y unificar a una sola variable en los PROTOCOLOS VULNERABLES

levels(Protocols)
data[data$Protocols == "",]  #PARA SELECCIONAR LAS FILAS VACÍAS

data %>% group_by(Protocols) %>% summarise( n=n()) #PARA AGRUPAR Y VERIFICAR LA CANTIDAD  Y REPETIDOS

data$neprotocols <- as.character(data$Protocols) # CAMBIO DE VARIABLE POR UN CARATER PARA UNIFICAR

unique(data$Protocols)

#Verificar duplicados y unificar a una sola variable en los PUERTOS VULNERABLES

levels(SourcePort)
data[data$SourcePort == "",]  #PARA SELECCIONAR LAS FILAS VACÍAS

data %>% group_by(SourcePort) %>% summarise( n=n()) #PARA AGRUPAR Y VERIFICAR LA CANTIDAD  Y REPETIDOS

data$neprotocols <- as.character(data$Protocols) # CAMBIO DE VARIABLE POR UN CARATER PARA UNIFICAR

unique(data$SourcePort)


## Verificar TIPO DE ATAQUES VS ORIGEN DE IP

compare <- data %>% group_by(necategory, SourceIP) %>% summarise( n=n())

arrange(compare, desc(n))


# Instalar Packages

install.packages("stringr")
library(stringr)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

?select
?filter
?arrange
?mutate
?summarise
?group_by



# FILTRO DE VARIABLES POR CRITERIOS DE: POSIXct, Factor y Chr, 

data$InicioT <- as.POSIXct(data$SartT, origin="1970-01-01")
data$Categoria <- as.factor(data$ncategory)
data$Subcategoria <- as.factor(data$subcategory)
data$Protcolo <- as.factor(data$Protocol)
data$OrigenIP <- as.character(data$SourceIP)
data$OrigenPuerto <- as.character(data$SourcePort)
data$DestinoIP <- as.character(data$DestinationIP)
data$DestinoPuerto <- as.character(data$DestinationPort)
data$Nombredeataque <- as.factor(data$NameAttack)
data$Referencias <- as.factor(data$Referencia)


## GRAFICA POR VARIABLE PARA IDENTIFICAR EL SCORE RESPECTO A OTRAS UTILIZANDO PLOT

plot(data$OrigenIP)
plot(data$DestinationIP)
plot(data$Category)
plot(data$Subcategoria)
plot(data$Protcolo)
plot(data$IPSource)
plot(data$OrigenPuerto)
plot(data$DestinoIP)
plot(data$DestinoPuerto)
plot(data$Nombredeataque)
plot(data$Referencias)




















