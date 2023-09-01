
##Borrar datos del entorno
rm(list=ls())



#Directorio
setwd("C:/Users/claudio.pacheco/Documents")
##Crear folders de almacenamiento
dir.create("conapo",showWarnings = FALSE)

#Paquetería
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,gganimate, showtext, gifski, ggpol)
font_add_google("Poppins", "pop")
showtext_auto()



#Función para descargar y unzip
download_and_unzip <- function(url, dir) {
  # Crear directorio si no existe
  if (!dir.exists(dir)) {
    dir.create(dir, showWarnings = FALSE)
  }
  
  # Crear el archivo destino
  destfile <- paste0(dir, "/", basename(url))
  
  # Descarga
  download.file(url, destfile)
  
  # Unzip
  unzip(destfile, exdir = dir)
  
}

#Descargar datos
url<-"https://conapo.segob.gob.mx/work/models/CONAPO/pry23/DB/ConDem50a19_ProyPob20a70.zip"
download_and_unzip(url,"conapo")

#leer datos
pob<-openxlsx::read.xlsx("conapo/ConDem50a19_ProyPob20a70/0_Pob_Mitad_1950_2070.xlsx")%>%
#Nombres a minúsculas
  janitor::clean_names()%>%
  select(!renglon)%>%
  #Omitir Nacional
  filter(entidad=="República Mexicana")%>%
  
  ##Valores negativos para hombres
  mutate(poblacion=ifelse(sexo=="Hombres",
                          poblacion*-1,
                          poblacion))%>%

  ###Se crean grupos de edad
  mutate(agegroup= case_when ( 
    edad >=0 & edad <5 ~  "00-04",
    edad >4 & edad <10 ~ "05-09",
    edad >9 & edad <15 ~  "10-14",
    edad >14 & edad <20 ~  "15-19",
    edad >19 & edad <25 ~  "20-24",
    edad >24 & edad <30 ~  "25-29",
    edad >29 & edad <35 ~  "30-34",
    edad >34 & edad <40 ~  "35-39",
    edad >39 & edad <45 ~  "40-44",
    edad >44 & edad <50 ~  "45-49",
    edad >49 & edad <55 ~  "50-54",
    edad >54 & edad <60 ~  "55-59",
    edad >59 & edad <65 ~  "60-64",
    edad >64 & edad <70 ~  "65-69",
    edad >69 & edad <75 ~  "70-74",
    edad >74 & edad <80 ~  "75-79",
    edad >79 & edad <85 ~  "80-84",
    edad >=85 ~ "85+"))



  ##Se crea el gráfico estático
      
      pob%>%
      ggplot(aes(
        x = agegroup,
        y = poblacion/1000000,

        fill = sexo)) +
      geom_bar(stat = "identity")+
coord_flip()+
         facet_share(
              ~ sexo,
              dir = 'h',
              scales = 'free',
              reverse_num = TRUE)+ 
              
      scale_fill_manual(
        values = c("#2ca25f", "#fdae6b")) +
        #Y con separador de miles

      ##Tema del gráfico
      theme(
        plot.background=element_rect(fill = "#edf8b1"),
        axis.title.y = element_blank(),
           axis.title.x = element_text(
                                size = 16,
                                face = 'bold'
                                ),
        axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16), 
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),

        strip.background = element_blank(),
            strip.text.x = element_blank(),

        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.key.size = unit(0.80, "cm"),
        legend.text = element_text(
          size = 20,
          face = "bold"
        ),
            legend.position = c(0.95, 0.5), plot.title = element_text(
          size = 35,
          hjust = 0.5,
          face = "bold"
        ),
        plot.subtitle = element_text(
          size = 30,
          hjust = 0.5,
          face = "bold"
        ),
        plot.caption = element_text(
          size = 20,
          hjust = 0),
        text=element_text("pop"))+
      labs(
        title = "México. Población 1950 - 2070\n\n{closest_state}",
        subtitle = "Población por sexo y grupo de edad",
        y = "\n\nPoblación (millones de personas)",
        caption = "\n\nFuente: @claudiodanielpc con información de 
CONAPO. Conciliación Demográfica 1950 a 2019 y Proyecciones de la población de México 2020 a 2070."
      )->p

  ##Animación
    p <- p +
      transition_states(ano,
                        transition_length = 1,
                        state_length = 2,
                        wrap = FALSE
                        ) +
                        
      enter_fade() +
      exit_fade() +
    ease_aes('cubic-in-out')



    
    
    animate(
      p,
      fps = 24,
      duration = 25,
      width = 3000,
      height = 1500,
      end_pause = 30,
      renderer = gifski_renderer("pirampobmx.gif")

      
    )

