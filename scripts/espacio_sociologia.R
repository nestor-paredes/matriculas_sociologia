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
library(ggrepel)
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

install.packages("ggrepel")

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

###############################
#Buscando correlaciones 
  
espacio %>% 
  select(lug_ofer:tit_t) %>% #Opción sencilla
  pairs() 

espacio %>% 
  select(lug_ofer:tit_t) %>% #Es necesario hacer una matriz de correlación previa 
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

###############################
#PCA

#Un PCA, indicando las variables categóricas como complementarias
espacio_pca <- PCA(espacio,ncp = 4, quali.sup=1:9)

#'Explorando los eigenvalues, los dos primeros capturan el 92% de la inercia/varianza 
espacio_pca$eig

#'Explorando las variables, en la Dim 1 la que que más contribuye es Primer ingreso hombres, 
#'seguida de Primer ingreso total. En la Dim 2 Egresados total
espacio_pca$var

#'Explorando a los individuos. La UAM Azc y la UAM Xoc, seguidas por la UNAM FCPyS escolarizada
#'son los programas que mas contribuyen a la Dim1; por su parte, Uhumanista_LA, UABJO_IIS y UDG_CUCSyH
#'son los programas que más contribuyen a la Dim 2. 
espacio_pca$ind$contrib



#Creando variables sintéticas
espacio$comp1 <- espacio_pca$ind$coord[,1] #Primer ingreso 
espacio$comp2 <- espacio_pca$ind$coord[,2] #Egresados 

#Graficando variables sintéticas 
plot1 <-ggplot(espacio, aes(x = comp1, y = comp2)) +
  geom_point() +
  theme_bw()+
  ggtitle("Primer ingreso/Egresadxs")+
  geom_text(aes(label = id), hjust = 0)
plot1
