#
# Author: Georgina Jimenez
# -----------------------------------------------------------
# calidad_aire/graphs/src/graficas.R

#### Paquetería ####
rm(list=ls())
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, ggplot2, ggalt, zoo, lubridate, svglite)
options(scipen=999)

files <- list(data_calidad = here("import-clean/output/base_calidad.rds"),
              data_promedios = here("import-clean/output/base_promedios.rds"),
              tema = here("graphs/src/theme.R"))
#### Tema  ####
source(files$tema)

#### Gráficas sobre en los que se rebasó el límite ideal 
data <- readRDS(files$data_calidad)

 data%>%
 mutate(yearmon=yearmon(fecha))%>%
 group_by(yearmon)%>%
 summarize(repm10=sum(repm10, na.rm=T))%>%
 ungroup()%>%
 ggplot(aes(x=yearmon, y=repm10))+
 geom_line()+
 geom_point()+
 tema+
  scale_x_continuous(breaks=2013:2022)+
 labs(title="Número de días en los que el PM10 rebasó el límite recomendado",
     subtitle="Por año", caption=caption, y="Número de días", x="Año")
 save("Rebaso_pm10")


 data%>%
   mutate(year=year(fecha))%>%
   group_by(year)%>%
   summarize(reozono=sum(reozono, na.rm=T))%>%
   ungroup()%>%
   ggplot(aes(x=year, y=reozono))+
   geom_line()+
   geom_point()+
   tema+
   scale_x_continuous(breaks=2013:2022)+
   labs(title="Número de días en los que el ozono superó el límite recomendado",
       subtitle="Por año", caption=caption, y="Número de días", x="Año")
 save("Rebaso_ozono")
 
 
 data%>%
   mutate(year=year(fecha))%>%
   group_by(year)%>%
   summarize(reazu=sum(reazu, na.rm=T))%>%
   ungroup()%>%
   ggplot(aes(x=year, y=reazu))+
   geom_line()+
   geom_point()+
   tema+
   scale_x_continuous(breaks=2013:2022)+
   labs(title="Número de días en los que el dióxido de azufre superó el límite recomendado",
        subtitle="Por año", caption=caption, y="Número de días", x="Año")
 save("Rebaso_azufre")
 
 
 data%>%
   mutate(year=year(fecha))%>%
   filter(year>2018)%>%
   group_by(year)%>%
   summarize(repm25=sum(repm25, na.rm=T))%>%
   ungroup()%>%
   ggplot(aes(x=year, y=repm25))+
   geom_line()+
   geom_point()+
   tema+
   scale_x_continuous(breaks=2019:2022)+
   labs(title="Número de días en los que el PM2.5 superó el límite recomendado",
        subtitle="Por año", caption=caption, y="Número de días", x="Año")
 save("Rebaso_pm25")
 
 
 #### Gráficas sobre promedios ####
 data <- readRDS(files$data_promedios)
 
 #Azufre
 data%>%
 mutate(yearmon=as.yearmon(fecha))%>%
 group_by(yearmon)%>%
 summarize(so2=mean(so2, na.rm=T))%>%
 ungroup()%>%
 ggplot(aes(x=yearmon, y=so2))+
 geom_line()+
 geom_point()+
 tema+
 scale_x_continuous(breaks=2013:2022)+
labs(title="Promedio de dióxido de azufre en el aire",
     subtitle="Por año", caption=caption, y="", x="Año")
 save("Mean_azufre-yearmon")
 
 
 data%>%
   mutate(year=year(fecha))%>%
   group_by(year)%>%
   summarize(so2=median(so2, na.rm=T))%>%
   ungroup()%>%
   ggplot(aes(x=year, y=so2))+
   geom_line()+
   geom_point()+
   tema+
   scale_x_continuous(breaks=2013:2022)+
   labs(title="Mediana de dióxido de azufre en el aire",
        subtitle="Por año", caption=caption, y="", x="Año")
 save("Median_azufre")
 
 
 #PM10
 data%>%
   mutate(year=year(fecha))%>%
   group_by(year)%>%
   summarize(pm10=mean(pm10, na.rm=T))%>%
   ungroup()%>%
   ggplot(aes(x=year, y=pm10))+
   geom_line()+
   geom_point()+
   tema+
   scale_x_continuous(breaks=2013:2022)+
   labs(title="Promedio de PM10 en el aire",
        subtitle="Por año", caption=caption, y="", x="Año")
 save("Mean_PM10")
 
 
 data%>%
   mutate(year=year(fecha))%>%
   group_by(year)%>%
   summarize(pm10=median(pm10, na.rm=T))%>%
   ungroup()%>%
   ggplot(aes(x=year, y=pm10))+
   geom_line()+
   geom_point()+
   tema+
   scale_x_continuous(breaks=2013:2022)+
   labs(title="Mediana de PM10 en el aire",
        subtitle="Por año", caption=caption, y="", x="Año")
 save("Median_PM10")
 
 
 #PM2.5
 data%>%
   mutate(year=year(fecha))%>%
   group_by(year)%>%
   summarize(pm2_5=mean(pm2_5, na.rm=T))%>%
   ungroup()%>%
   ggplot(aes(x=year, y=pm2_5))+
   geom_line()+
   geom_point()+
   tema+
   scale_x_continuous(breaks=2013:2022)+
   labs(title="Promedio de PM2.5 en el aire",
        subtitle="Por año", caption=caption, y="", x="Año")
 save("Mean_PM25")
 
 
 data%>%
   mutate(year=year(fecha))%>%
   group_by(year)%>%
   summarize(pm2_5=median(pm2_5, na.rm=T))%>%
   ungroup()%>%
   ggplot(aes(x=year, y=pm2_5))+
   geom_line()+
   geom_point()+
   tema+
   scale_x_continuous(breaks=2013:2022)+
   labs(title="Mediana de PM10 en el aire",
        subtitle="Por año", caption=caption, y="", x="Año")
 save("Median_PM25")
