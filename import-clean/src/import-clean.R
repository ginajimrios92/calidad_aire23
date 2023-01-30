#
# Author: Gina Jiménez
# ------------------------------------------------------------------------------------
# Calidad del aire, Enero 2023

library("pacman")
p_load(tidyverse, foreign, janitor, here, zoo)


#### Directorios####
dir <- "/Users/georginajimenez92/Documents/inputs-datos/0123_calidadaire/indice-calidad/"
names <- dir(dir)
col_names <- c("fecha", "hora", "noro_ozono", "noro_azu", "noro_nitro", "noro_carb",
               "noro_pm10", "nore_ozono", "nore_azu", "nore_nitro", "nore_carb",
               "nore_pm10", "cen_ozono", "cen_azu", "cen_nitro", "cen_carb",
               "cen_pm10", "suro_ozono", "suro_azu", "suro_nitro", "suro_carb",
               "suro_pm10", "sure_ozono", "sure_azu", "sure_nitro", "sure_carb", "sure_pm10")

#### Funciones ####
mas_cero <- function(x){
  x <- ifelse(x>0, 1, 0)
}

### Empezamos ####

data <- data.frame()

for (i in 1:length(names)) {
  tempo <- read.csv(paste0(dir, names[i]), skip=9, header=F)
  names(tempo) <- col_names
  tempo <- tempo[2:8760, 1:27]
  data <- bind_rows(data, tempo)
} 

rm(tempo)


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
               sure_reazu=ifelse(sure_azu>125, 1, 0))

data <- mutate(data, repm10 = rowSums(data[28:32]),
                     reozono = rowSums(data[33:37]),
                     renitro = rowSums(data[38:42]),
                     reazu = rowSums(data[43:47]))%>%
        group_by(fecha)%>%
        summarize(repm10=sum(repm10, na.rm=T),
                  reozono=sum(reozono, na.rm=T),
                  renitro=sum(renitro, na.rm=T),
                  reazu=sum(reazu, na.rm=T))%>%
        mutate_at(vars(c(repm10:reazu)), mas_cero)%>%
        mutate(quart=as.yearqtr(fecha))

saveRDS(data, "/Users/georginajimenez92/Documents/GitHub/calidad_aire/import-clean/output/base_calidad.rds")