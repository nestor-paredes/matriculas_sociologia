#################################
#Paquetes recurrentes
#################################

#install.packages("")
#library()
library(installr)
library(readr)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(GGally)
library(corrplot)
library(Hmisc)
library(egg)
library(tidyr)
library(viridis)
library(clipr)
library(patchwork)
library(FactoMineR)
library(factoextra)
library(Factoshiny)

###############################
#'Estoy utilizando los datos 23-24, 48 programas
#'27 variables.  


#Cargando mi base de datos 
base <- read_csv("datos/espacio_sociologia.csv")
  base2 <- as_tibble(base) #Transformando en tibble 
  rm(base)
  
base3 <- base2 %>% 
  select(-nombre_instituciòn, -esc_fac) #Quitando variables no necesarias 
  rm(base2)

espacio <- base3 %>%
  mutate(across(where(is.double), scale)) #Normalizando o estandarizando las variables numéricas 
  rm(base3)
  espacio
  View(espacio)

#Buscando correlaciones 
  
espacio %>% 
  select(lug_ofer:tit_t) %>% #Opción sencilla
  pairs() 

espacio %>% 
  select(lug_ofer:tit_t) %>% 
  cor() %>%
  ggpairs()

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#Esta opción me gusta, ofrece coeficientes de correlación en los contrastes
espacio %>% 
  select(lug_ofer:tit_t) %>%  
  chart.Correlation()

#Matriz de correlaciones y mapas de calor 

espacio %>% 
  select(lug_ofer:tit_t) %>% #Buena  
  cor() %>% 
  corrplot(method = "color")

espacio %>% 
  select(lug_ofer:tit_t) %>% #Poco intuitiva 
  cor() %>% 
  corrplot(method = "circle") 

#'Esta otra opción es interesante, realiza también un dendograma y construye 
#'clústeres (as-jer), ya se pueden ver dos grupos de variables, las primeras 
#'relacionadas con el primer ingreso, las solicitudes, lugares ofertados y matrículas; 
#'el otro grupo, egresados y titulados. Podemos ver una falta de asociación entre 
#'ambos grupos, particularmente entre el egreso y las solicitudes de primer ingreso... 

espacio %>% 
  select(lug_ofer:tit_t) %>% #Está buena
  cor() %>%
  heatmap()