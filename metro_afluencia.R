##Gráfica de afluencia de pasajeros en el Metro CDMX

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("D:")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, lubridate,scales)

#Datos
movi = read.csv("https://datos.cdmx.gob.mx/dataset/f2046fd5-51b5-4876-b008-bd65d95f9a02/resource/0e8ffe58-28bb-4dde-afcd-e5f5b4de4ccb/download/afluencia_metro.csv", 
                   header=TRUE, row.names=NULL, 
                   stringsAsFactors=FALSE)%>%
  #Renombrar nombre de variables a minúsculas
  janitor::clean_names()%>%
  mutate(afluencia=na_if(afluencia,0))



##Heatmap
movi%>%
  arrange(fecha)%>%
  mutate(fecha=as.Date.character(fecha)
  )%>%
  group_by(linea) %>% 
  complete(fecha) %>% 
  ungroup()%>%
  filter(ano>=2020)%>%
  mutate(linea = factor(linea, levels=rev(c("Linea 1", "Linea 2", 
                                                              "Linea 3", "Linea 4", "Linea 5", "Linea 6", "Linea 7", "Linea 8",
                                                              "Linea 9","Linea A","Linea B","Linea 12"))))%>%


ggplot(., 
       aes(x = fecha, y = linea, fill = afluencia)) + 
  geom_tile()+
  #Jornada Nacional de Sana Distancia
  geom_vline(xintercept = ymd("2020-03-23"), linetype="dashed", 
             color = "black", size=1.5)+
  
  geom_vline(xintercept = ymd("2020-05-30"), linetype="dashed", 
             color = "black", size=1.5)+
  annotate(geom = "text", x = ymd("2020-01-10"), y = 13.5, 
           label = "Movilidad previa\na COVID-19",
           hjust = 0, vjust=1, fontface="bold", color="#31a354",
           size=3)+
  
  annotate(geom = "text", x = ymd("2020-03-27"), y = 13.5, 
           label = "Jornada Nacional\n de Sana Distancia",
           hjust = 0, vjust=1, fontface="bold", color="#636363",
           size=3)+
  ##Semáforo epidemiológico de CDMX pasa de rojo a naranja
  geom_vline(xintercept = ymd("2020-06-29"), linetype="dashed", 
             color = "black", size=1.5)+
  annotate(geom = "text", x = ymd("2020-06-02"), y = 13.5, 
           label = "Semáforo\n rojo",
           hjust = 0, vjust=1, fontface="bold", color="red",
           size=3)+
  ##Semáforo epidemiológico de CDMX pasa de naranja a rojo
  geom_vline(xintercept = ymd("2020-12-21"), linetype="dashed", 
             color = "black", size=1.5)+
  geom_vline(xintercept = ymd("2020-12-24"), linetype="dashed", 
             color = "#3182bd", size=1.5)+
  annotate(geom = "text", x = ymd("2020-08-06"), y = 13.5, 
           label = "Semáforo naranja",
           hjust = 0, vjust=1, fontface="bold", color="#d95f0e",
           size=6)+
  annotate(geom = "text", x = ymd("2020-12-31"), y = 13.5, 
           label = "Semáforo rojo",
           hjust = 0, vjust=1, fontface="bold", color="red",
           size=3)+
  annotate(geom = "text", x = ymd("2020-12-24"), y = 13, 
           label = "Vacunación contra COVID-19, primera y segunda dosis y refuerzo",
           hjust = 0, vjust=1, fontface="bold", color="#3182bd",
           size=6)+
  ##Semáforo epidemiológico de CDMX pasa de rojo a naranja
  geom_vline(xintercept = ymd("2021-02-15"), linetype="dashed", 
             color = "black", size=1.5)+
  
  annotate(geom = "text", x = ymd("2021-08-01"), y = 1, 
           label = "Cierre de Línea 12",
           hjust = 0, vjust=1, fontface="bold", color="#756bb1",
           size=6)+
  theme_minimal()+
  ##Títulos, subtítulos y fuente
  labs(title = "Afluencia diaria de pasajeros en el Sistema de Transporte Colectivo (Metro)",

       y = "Líneas",
       caption = "Notas: 
La Jornada Nacional de Sana Distancia fue el nombre que utilizó el Gobierno de México para definir el confinamiento.
Por otra parte, El semáforo epidemiológico es un sistema de monitoreo para la regulación del uso del espacio público de acuerdo con el riesgo de contagio de COVID-19.
Los valores en blanco son datos no disponibles o en cero dentro de la base de datos.
Fuente: @claudiodanielpc con datos de la Agencia Digital de Innovación Pública de la Ciudad de México (ADIP) y de la Secretaría de Salud del Gobierno de México.",
    fill = "Afluencia \n(personas)")+
  ##Formato del eje de la fecha
  scale_x_date("Fecha",expand = c(0, 0),
               date_labels = "%b %d %Y", date_breaks = "30 days")+
  ##Colores del heatmap
  scale_fill_distiller(
    limits = c(1,150000),
    palette = "YlOrBr",direction=1,labels=comma,na.value = "white")+
  ##Temita
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour="black"),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.title.align = 0.5,
        legend.text=element_text(colour="black",size=14,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(colour="black", size=13,angle=90),
        axis.text.y=element_text(colour="black",size=13),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        strip.text.x = element_text(size=20, color="black",
                                    face="bold"),
        plot.title=element_text(colour="black",hjust=0,size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        text=element_text("Century Gothic",size=20))


##Salvar el gráfico
ggsave("metro_afluencia.png",height = 10,width = 17, units="in",dpi=300)