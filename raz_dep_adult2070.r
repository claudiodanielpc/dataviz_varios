
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
  #Calcular población de 65 años y más y de 15 a 64
    mutate(pob_65mas=case_when(edad>=65~poblacion,
                               TRUE~0),
           pob_15a64=case_when(edad>=15 & edad<=64~poblacion,
                               TRUE~0))%>%
    ungroup()%>%
    #Sumar población
    group_by(ano)%>%
    summarise(
              pob_65mas=sum(pob_65mas),
              pob_15a64=sum(pob_15a64))%>%
    ungroup()%>%
    #Calcular razón de dependencia
    mutate(razon=pob_65mas/pob_15a64*100)

#Generar gráfico estático sobre razón de dependencia adulta
graf<-pob%>%
  #Filtro
  ggplot()+
  geom_line(aes(ano,razon,group=1),color="#b38e5d",linewidth=3)+
labs(

    title="México. Razón de dependencia adulta, 1950-2070",
    x="Año",
    y="Razón de dependencia",
    caption="Nota:
Razón de dependencia adulta: Cantidad de personas de 65 y más años de edad por cada 100 personas de entre 15 y 64 años.
    
Fuente: @claudiodanielpc con información de CONAPO. Conciliación Demográfica 1950 a 2019 y Proyecciones de la población de México 2020 a 2070."
  )+
  
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=18),
        #Fuente y color de texto
        text=element_text("pop",
                          size=20, color="#bdbdbd"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color="#bdbdbd", face = "bold"),
        #Fondo verde
        plot.background = element_rect(fill = "#285c4d")
        
        )


graf<-graf+
  #Código para animación
  transition_reveal(ano)+view_follow()
  



#Detalles de la animación
graf%>%
  animate(fps = 20,
          nframes = 100,
          duration=18,
          end_pause = 100,
          width = 1800, height = 800)
          

#Salvar GIF
anim_save("raz_dep.gif")

