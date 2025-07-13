#################################
#Paquetes recurrentes
#################################

#install.packages("")
#library()
library(installr)
library(readr)
library(dplyr)
library(tidyverse)
library(paletteer)
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

#install.packages("showtext")
#install.packages("sysfonts")
library(showtext)
library(sysfonts)
font_add_google(name = "Roboto", family = "roboto")
showtext_auto()



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
espacio_pca$ind$contrib[,1]

#Graficando variables relevantes 
ggplot(espacio, aes(x = sol_prim_ing_t, y = egre_t, label = id)) +
  geom_point() +
  geom_text_repel(size = 3) +
  geom_smooth(method = "loess", color = "green") +
  theme_bw()+
  ggtitle("Primer ingreso total / Egreso total")

ggplot(espacio, aes(x = sol_prim_ing_m, y = egre_t, label = id)) +
  geom_point() +
  geom_text_repel(size = 3) +
  geom_smooth(method = "loess", color = "red") +
  theme_bw()+
  ggtitle("Primer ingreso mujeres / Egreso total")

ggplot(espacio, aes(x = sol_prim_ing_h, y = egre_t, label = id)) +
  geom_point() +
  geom_text_repel(size = 3) +
  geom_smooth(method = "loess", color = "blue") +
  theme_bw()+
  ggtitle("Primer ingreso hombres / Egreso total")

#Creando variables sintéticas y sumándolas a la base 
espacio$comp1 <- espacio_pca$ind$coord[,1] #Primer ingreso 
espacio$comp2 <- espacio_pca$ind$coord[,2] #Egresados 

#Graficando variables sintéticas 1
#No legible 
ggplot(espacio, aes(x = comp1, y = comp2)) +
  geom_point() +
  theme_bw()+
  ggtitle("Primer ingreso/Egresadxs")+
  geom_text(aes(label = id), hjust = 0)

#Graficando variables sintéticas 2
#Utilizando ggrepel

p_titulo <- "Componente 1/Componente 2"
p_subtitulo <- "Primer ingreso y Egreso"
p_caption <- "Elaboración propia con datos de Anuarios Estadísticos, ANUIES, 2023-2024"

etiqueta_x <- "Contribución al componente 1: Ingreso"
etiqueta_y <- "Contribución al componente 2: Egreso"

#Graficando los componentes 1 y 2

grafica <- ggplot(espacio, aes(x = comp1, y = comp2, 
                               label = id))
grafica + theme_bw()+
  geom_point() +
  geom_text_repel(size = 3) +
  labs(x = etiqueta_x, 
       y = etiqueta_y, 
       title = p_titulo, 
       subtitle = p_subtitulo, 
       caption = p_caption
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", family = "roboto"),
    plot.subtitle = element_text(size = 13, family = "roboto"),
    axis.title.x = element_text (size = 10, face = "bold", family = "roboto"),
    axis.title.y = element_text (size = 10, face = "bold", family = "roboto"), 
    panel.border = element_rect(size = 1.2),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

#Graficando los componentes 1 y 2 por tipo de financiamiento

grafica <- ggplot(espacio, aes(x = comp1, y = comp2, 
                               label = id, 
                               color = financiamiento))
grafica + theme_bw()+
  geom_point() +
  geom_text_repel(size = 3, face ="bold") +
  scale_colour_paletteer_d("MexBrewer::Alacena") +
    labs(x = etiqueta_x, 
       y = etiqueta_y, 
       title = p_titulo, 
       subtitle = p_subtitulo, 
       caption = p_caption
       ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", family = "roboto"),
    plot.subtitle = element_text(size = 13, family = "roboto"),
    axis.title.x = element_text (size = 10, face = "bold", family = "roboto"),
    axis.title.y = element_text (size = 10, face = "bold", family = "roboto"), 
    panel.border = element_rect(size = 1.2),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
        )

#Graficando los componentes 1 y 2 facet por tipo de financiamiento 

grafica <- ggplot(espacio, aes(x = comp1, y = comp2, 
                               label = id))
grafica + theme_bw()+
  geom_point() +
  geom_text_repel(size = 3) +
  labs(x = etiqueta_x, 
       y = etiqueta_y, 
       title = p_titulo, 
       subtitle = p_subtitulo, 
       caption = p_caption
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", family = "roboto"),
    plot.subtitle = element_text(size = 13, family = "roboto"),
    axis.title.x = element_text (size = 10, face = "bold", family = "roboto"),
    axis.title.y = element_text (size = 10, face = "bold", family = "roboto"), 
    panel.border = element_rect(size = 1.2),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  facet_wrap(~ financiamiento)