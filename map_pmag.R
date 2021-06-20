#Mapa de establecimientos de alojamiento temporal

##Paquetería necesaria
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, leaflet)

##Se crea directorio de almacenamiento
dir.create("ue/", showWarnings = FALSE)


##Liga primer archivo
download.file(url = "https://www.inegi.org.mx/contenidos/masiva/denue/denue_00_72_1_csv.zip", 
              destfile = "ue/denue_00_72_1_csv.zip")

##Liga segundo archivo
download.file(url = "https://www.inegi.org.mx/contenidos/masiva/denue/denue_00_72_2_csv.zip",            
              destfile = "ue/denue_00_72_2_csv.zip")


##Extraer archivos
unzip("ue/denue_00_72_1_csv.zip",
      exdir = "ue")

unzip("ue/denue_00_72_2_csv.zip",
      exdir = "ue")


###Leer archivos

ue1<-read.csv("ue/conjunto_de_datos/denue_inegi_72_1.csv",
              encoding="latin",header=TRUE,check.names=FALSE)


ue2<-read.csv("ue/conjunto_de_datos/denue_inegi_72_2.csv",
              encoding="latin",header=TRUE,check.names=FALSE)


##Pegar y filtrar únicamente los establecimientos de hospedaje
ue<-rbind(ue1,ue2)

ue<-ue%>%
  filter(codigo_act==721111 | 
           codigo_act==721112 |
           codigo_act==721113 |
           codigo_act==721190)%>%
  ##Construir clave de municipio
  ##Agregar cero a entidad
  mutate(cve_ent=ifelse(nchar(cve_ent)==1,
                        paste0("0",cve_ent),cve_ent))%>%
  ##Agregar ceros a municipio
  mutate(cve_mun=ifelse(nchar(cve_mun)==1,
                        paste0("00",cve_mun),
                        ifelse(nchar(cve_mun)==2,
                               paste0("0",cve_mun),
                               cve_mun)))%>%
  ##Crear clave geoestadística  
  mutate(cvegeo=paste0(cve_ent,cve_mun))%>%
  ##Filtro de pueblos mágicos
  filter(cvegeo=="14086")
  
  
  ##Remover los archivos
  rm(ue1,ue2)
  


#Directorio de trabajo

setwd("C:/Users/ALIENWARE/Documents/turismo/turismo/pueblos")

##Título y fuente del gráfico
tit<-htmltools::HTML("<b style='font-size:23px; font-family:century gothic'>",
                     paste0(unique(ue$municipio),", ",
                                  unique(ue$entidad)),"<br>
                     Establecimientos de servicios<br>
                     de alojamiento temporal, 2021</b>")

fuente<-htmltools::HTML("<b style='font-size:15px; 
                        font-family:century gothic'>
                        Nota: Se considera la rama de actividad 7211 Hoteles, moteles y similares.<br>
                        Fuente: Elaboración propia con datos de INEGI. 
Directorio Estadístico Nacional de Unidades\nEconómicas (DENUE)</b>")

##Mapa
mapa<-leaflet(ue) %>%
  addTiles() %>%
#leaflet.extras::addHeatmap(lng=~longitud, lat=~latitud,
 #                          blur = 20, max = 0.05, 
  #                         radius = 15)%>%
  addCircleMarkers(lng=~longitud, lat=~latitud, 
                  weight = 3, color = "#de2d26")%>%
  addControl(tit, 
             position = "topleft", 
             className="map-title")%>%
  addControl(fuente, 
             position = "bottomleft", 
             className="info legend")%>%
  addControl(paste0(ue%>%summarise(n())," Establecimientos"), 
             position = "topright", 
             className="info legend")
mapa

##Guardar mapa
mapview::mapshot(mapa,
                 file=paste0("mapa",
                                  unique(ue$municipio),".png"))