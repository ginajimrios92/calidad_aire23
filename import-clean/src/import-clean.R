#
# Author: Gina Jiménez
# ------------------------------------------------------------------------------------
# Calidad del aire, Enero 2023

library("pacman")
p_load(tidyverse, foreign, janitor, here, zoo, lubridate, matrixStats)

#### Funciones ####
mas_cero <- function(x){
  x <- ifelse(x>0, 1, 0)
}

### Base Índice de calidad del aire ####
#Primero 2013 a 2018
dir <- "/Users/georginajimenez92/Documents/inputs-datos/0123_calidadaire/indice-calidad/"
names <- dir(dir)

data <- data.frame()
col_names <- c("fecha", "hora", "noro_ozono", "noro_azu", "noro_nitro", "noro_carb",
               "noro_pm10", "nore_ozono", "nore_azu", "nore_nitro", "nore_carb",
               "nore_pm10", "cen_ozono", "cen_azu", "cen_nitro", "cen_carb",
               "cen_pm10", "suro_ozono", "suro_azu", "suro_nitro", "suro_carb",
               "suro_pm10", "sure_ozono", "sure_azu", "sure_nitro", "sure_carb", "sure_pm10")

for (i in 1:6) {
  tempo <- read.csv(paste0(dir, names[i]), skip=9, header=F)
  names(tempo) <- col_names
  tempo <- tempo[, 1:27]
  data <- bind_rows(data, tempo)
} 

rm(tempo)

### Luego 2019 a 2022 
col_names <- c("fecha", "hora", "noro_ozono", "noro_azu", "noro_nitro", "noro_carb",
               "noro_pm10", "noro_pm25", "nore_ozono", "nore_azu", "nore_nitro", "nore_carb",
               "nore_pm10", "nore_pm25", "cen_ozono", "cen_azu", "cen_nitro", "cen_carb",
               "cen_pm10", "cen_pm25", "suro_ozono", "suro_azu", "suro_nitro", "suro_carb", 
               "suro_pm10", "suro_pm25", "sure_ozono", "sure_azu", "sure_nitro", "sure_carb", 
               "sure_pm10", "sure_pm25")

for (i in 7:10) {
  tempo <- read.csv(paste0(dir, names[9]), skip=9, header=F)
  names(tempo) <- col_names
  tempo <- tempo[, 1:32]
  data <- bind_rows(data, tempo)
} 

rm(tempo)

data <- data[!is.na(data$hora),]
data[is.na(data)] <- 0


#### Todo junto y unido #### 
data <- data%>%
        mutate(fecha=as.Date(fecha, format="%d/%m/%Y"),
               noro_repm10=ifelse(noro_pm10>150, 1, 0),
               nore_repm10=ifelse(nore_pm10>150, 1, 0),
               cen_repm10=ifelse(cen_pm10>150, 1, 0),
               suro_repm10=ifelse(suro_pm10>150, 1, 0),
               sure_repm10=ifelse(sure_pm10>150, 1, 0),
               noro_reozono=ifelse(noro_ozono>160, 1, 0),
               nore_reozono=ifelse(nore_ozono>160, 1, 0),
               cen_reozono=ifelse(cen_ozono>160, 1, 0),
               suro_reozono=ifelse(suro_ozono>160, 1, 0),
               sure_reozono=ifelse(sure_ozono>160, 1, 0),
               noro_renitro=ifelse(noro_nitro>120, 1, 0),
               nore_renitro=ifelse(nore_nitro>120, 1, 0),
               cen_renitro=ifelse(cen_nitro>120, 1, 0),
               suro_renitro=ifelse(suro_nitro>120, 1, 0),
               sure_renitro=ifelse(sure_nitro>120, 1, 0),
               noro_reazu=ifelse(noro_azu>125, 1, 0),
               nore_reazu=ifelse(nore_azu>125, 1, 0),
               cen_reazu=ifelse(cen_azu>125, 1, 0),
               suro_reazu=ifelse(suro_azu>125, 1, 0),
               sure_reazu=ifelse(sure_azu>125, 1, 0),
               noro_repm25=ifelse(noro_pm25>75, 1, 0),
               nore_repm25=ifelse(nore_pm25>75, 1, 0),
               cen_repm25=ifelse(cen_pm25>75, 1, 0),
               suro_repm25=ifelse(suro_pm25>75, 1, 0),
               sure_repm25=ifelse(sure_pm25>75, 1, 0))

data <- mutate(data, repm10 = rowSums(data[33:37]),
                     reozono = rowSums(data[38:42]),
                     renitro = rowSums(data[43:47]),
                     reazu = rowSums(data[49:52]),
                     repm25 = rowSums(data[53:57]))%>%
        group_by(fecha)%>%
        summarize(repm10=sum(repm10, na.rm=T),
                  reozono=sum(reozono, na.rm=T),
                  renitro=sum(renitro, na.rm=T),
                  reazu=sum(reazu, na.rm=T),
                  repm25=sum(repm25, na.rm=T))%>%
        mutate_at(vars(c(repm10:repm25)), mas_cero)%>%
        mutate(quart=as.yearqtr(fecha))

saveRDS(data, here("import-clean/output/base_calidad.rds"))


#### Base promedio de dióxido de azufre ####
dir <- "/Users/georginajimenez92/Documents/inputs-datos/0123_calidadaire/promedios/azufre/"
names <- dir(dir)

data_so2 <- data.frame()
for (i in 1:10) {
  tempo <- read.csv(paste0(dir, names[i]), skip=8, header=T)
  tempo <- mutate(tempo, date=as.Date(date, format="%d/%m/%Y"))
  data_so2 <- bind_rows(data_so2, tempo)
} 

data_so2 <- mutate(data_so2, fecha=as.Date(date, format="%d/%m/%Y"))%>%
            group_by(fecha)%>%
            summarize(so2=mean(value, na.rm=T))

dir <- "/Users/georginajimenez92/Documents/inputs-datos/0123_calidadaire/promedios/particulas/"
names <- dir(dir)


data_pm <- data.frame()
for (i in 1:10) {
  tempo <- read.csv(paste0(dir, names[i]), skip=8, header=T)
  data_pm <- bind_rows(data_pm, tempo)
} 

data_pm <- mutate(data_pm, fecha=as.Date(date, format="%d/%m/%Y"))%>%
           group_by(fecha, id_parameter)%>%
           summarize(value=mean(value, na.rm=T))%>%
           pivot_wider(names_from=id_parameter, values_from=value)%>%
           clean_names()

data <- left_join(data_so2, data_pm)

saveRDS(data, here("import-clean/output/base_promedios.rds"))

### Base Máximos en Row ####
dir <- "/Users/georginajimenez92/Documents/inputs-datos/0123_calidadaire/maximos/"
names <- dir(dir)

data_so2 <- data.frame()


for (i in 11:15) {
  tempo <- read.csv(paste0(dir, names[i]), skip=1, header=T)%>%
           clean_names()%>%
           select(-x)%>%
           mutate_at(vars(2:length(.)), as.numeric)%>%
           mutate(fecha=as.Date(fecha, format="%d-%m-%Y"))
  tempo$max_so2 = rowMaxs(as.matrix(tempo[,c(2:length(tempo))]), na.rm=T)
  tempo <- select(tempo, fecha, max_so2)
  data_so2 <- bind_rows(data_so2, tempo)
} 

data_pm10 <- data.frame()

for (i in 1:5) {
  tempo <- read.csv(paste0(dir, names[i]), skip=1, header=T)%>%
    clean_names()%>%
    select(-x)%>%
    mutate_at(vars(2:length(.)), as.numeric)%>%
    mutate(fecha=as.Date(fecha, format="%d-%m-%Y"))
  tempo$max_pm10 = rowMaxs(as.matrix(tempo[,c(2:length(tempo))]), na.rm=T)
  tempo <- select(tempo, fecha, max_pm10)
  data_pm10  <- bind_rows(data_pm10 , tempo)
} 

data_pm25 <- data.frame()

for (i in 6:10) {
  tempo <- read.csv(paste0(dir, names[i]), skip=1, header=T)%>%
    clean_names()%>%
    select(-x)%>%
    mutate_at(vars(2:length(.)), as.numeric)%>%
    mutate(fecha=as.Date(fecha, format="%d-%m-%Y"))
  tempo$max_pm25 = rowMaxs(as.matrix(tempo[,c(2:length(tempo))]), na.rm=T)
  tempo <- select(tempo, fecha, max_pm25)
  data_pm25  <- bind_rows(data_pm25 , tempo)
} 

data <- left_join(data_pm10, data_pm25)%>%
        left_join(., data_so2)%>%
        pivot_longer(2:4, names_to="contaminante", values_to="total")%>%
        mutate(contaminante=gsub("max_", "", contaminante),
               contaminante=str_to_title(contaminante))

rm(data_pm10, data_pm25, data_so2)

saveRDS(data, here("import-clean/output/base2023.rds"))

  
