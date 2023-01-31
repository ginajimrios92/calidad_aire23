#
# Author: Georgina Jimenez
# -----------------------------------------------------------
# calidad_aire/graphs/src/graficas.R

#### Paquetería ####
rm(list=ls())
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, ggplot2, ggalt, zoo, lubridate)
options(scipen=999)

files <- list(data = here("import-clean/output/base_calidad.rds"),
              tema = here("graphs/src/theme.R"))

data <- readRDS(files$data)

#### Tema  ####
source(files$tema)

#### Gráficas sobre días de calidad del aire
data%>%
mutate(yearmon=as.yearmon(fecha))%>%
group_by(yearmon)%>%
summarize()
group_by(quart)%>%
summarize(repm10=sum(repm10, na.rm=T))%>%
ungroup()%>%
ggplot(aes(x=quart, y=repm10))+
geom_line()+
geom_point()+
tema+
ylim(c(-1, 2))+
labs(title="Número de días en los que el PM10 superó \n el límite recomendado",
     subtitle="Por trimestre del año")


data%>%
  group_by(quart)%>%
  summarize(reozono=sum(reozono, na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(x=quart, y=reozono))+
  geom_line()+
  geom_point()+
  tema+
  labs(title="Número de días en los que el ozono superó \n el límite recomendado",
       subtitle="Por trimestre del año")


data%>%
  group_by(quart)%>%
  summarize(reazu=sum(reazu, na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(x=quart, y=reazu))+
  geom_line()+
  geom_point()+
  tema+
  labs(title="Número de días en los que el dióxido azufre superó \n el límite recomendado",
       subtitle="Por trimestre del año")
  

data%>%
  group_by(quart)%>%
  summarize(renitro=sum(renitro, na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(x=quart, y=renitro))+
  geom_line()+
  geom_point()+
  tema+
  labs(title="Número de días en los que el nitrógeno superó el límite recomendado",
       subtitle="Por trimestre del año")


data%>%
  group_by(quart)%>%
  summarize(repm25=sum(repm25, na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(x=quart, y=repm25))+
  geom_line()+
  geom_point()+
  tema+
  labs(title="Número de días en los que el nitrógeno superó el límite recomendado",
       subtitle="Por trimestre del año")


#### Gráficas por año
data%>%
mutate(month=month(fecha),
       year=year(fecha))%>%
group_by(year, month)%>%
summarize(repm10=sum(repm10, na.rm=T))%>%
ggplot(aes(x=month, y=repm10, color=as.factor(year), group=year))+
  geom_line()+
  geom_point()+
  tema+
  labs(title="Número de días en los que el PM10 superó el límite recomendado",
       subtitle="Por mes de cada año año")


data%>%
  mutate(month=month(fecha),
         year=year(fecha))%>%
  group_by(year, month)%>%
  summarize(reozono=sum(reozono, na.rm=T))%>%
  ggplot(aes(x=month, y=reozono, color=as.factor(year), group=year))+
  geom_line()+
  geom_point()+
  tema+
  labs(title="Número de días en los que el ozono superó el límite recomendado",
       subtitle="Por mes de cada año año")


data%>%
  mutate(month=month(fecha),
         year=year(fecha))%>%
  group_by(year, month)%>%
  summarize(reazu=sum(reazu, na.rm=T))%>%
  ggplot(aes(x=month, y=reazu, color=as.factor(year), group=year))+
  geom_line()+
  geom_point()+
  tema+
  labs(title="Número de días en los que el dióxido de azufre superó el límite recomendado",
       subtitle="Por mes de cada año año")

data%>%
  mutate(month=month(fecha),
         year=year(fecha))%>%
  group_by(year, month)%>%
  summarize(renitro=sum(renitro, na.rm=T))%>%
  ggplot(aes(x=month, y=renitro, color=as.factor(year), group=year))+
  geom_line()+
  geom_point()+
  tema+
  labs(title="Número de días en los que el dióxido de nitrógeno superó el límite recomendado",
       subtitle="Por mes de cada año año")
