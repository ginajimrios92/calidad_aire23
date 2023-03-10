#
# Author: Gina Jiménez
# --------------------------------------------------
# diarreas/descriptives/src/theme.r
#

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, extrafont)
extrafont::loadfonts(quiet=T)

Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)

tema <- theme_bw() + 
        theme(text = element_text(family = "Trebuchet MS", color = "black"),
              plot.title = element_text(face = "bold", size = 20),
              legend.background = element_rect(fill = "white", size = 4, colour = "white"),
              legend.position = "top",
              axis.ticks = element_line(colour = "grey70", size = 0.2),
              panel.grid.major = element_line(colour = "grey70", size = 0.2),
              panel.grid.minor = element_blank()
  )

pal <-  c("#61304B","#857C8D","#94BFBE", "#acf7c1")

save <- function(name){
  ggsave(paste0(here("graphs/out/"), name, ".png"), width = 12, height = 8)
  ggsave(paste0(here("graphs/out/"), name, ".svg"), width = 12, height = 8)
}


caption <- "Source: Air Quality Mexico City http://www.aire.cdmx.gob.mx/"


# done.
