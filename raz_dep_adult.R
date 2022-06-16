#Relación de dependencia


##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
#Esto se debe de cambiar en cada computadora
setwd("D:/dataviz")

# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, gganimate, extrafont)


#URl====
url<-"http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/ind_dem_proyecciones.csv"


#Lectura de datos====
pob<-read.csv(url)%>%
  #Nombres en minúsculas
  janitor::clean_names()%>%
  #Dejar datos nacionales
  filter(cve_geo==0)%>%
  #Seleccionar variables de interés
  select(ano,raz_dep_adu,raz_dep_inf)%>%
  #Wide a long
  pivot_longer(cols=!ano,
    names_to="razon", values_to="data")%>%
  #Cambiar variable string
  mutate(razon=case_when(razon=="raz_dep_adu" ~ "Razón de dependencia adulta",
                         TRUE ~ "Razón de dependencia infantil"
                         
                         )
    
    
  )


#Generar gráfico estático sobre razón de dependencia adulta
graf<-pob%>%
  #Filtro
  filter(razon=="Razón de dependencia adulta")%>%
  ggplot()+
  geom_line(aes(ano, data, group=1), size=3, color="#b38e5d")+
  labs(
    title="México. Razón de dependencia adulta, 1950-2050",
    x="Año",
    y="Razón de dependencia",
    caption="Nota:
Razón de dependencia adulta: Cantidad de personas de 65 y más años de edad por cada 100 personas de entre 15 y 64 años.
    
Fuente: @claudiodanielpc con información de CONAPO."
  )+
  
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=18),
        #Fuente y color de texto
        text=element_text("Tahoma",
                          size=20, color="#bdbdbd"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color="#bdbdbd", face = "bold"),
        #Fondo negro
        plot.background = element_rect(fill = "#285c4d")
        
        )

#Mostrar gráfico estático
graf


#Animación

graf<-graf+
  #Código para animación
  transition_reveal(ano)+view_follow()
  



#Detalles de la animación
graf%>%
  animate(fps = 20,
          nframes = 100,
          duration=18,
          end_pause = 100,
          width = 1200, height = 800)


#Salvar GIF
anim_save("raz_dep.gif")

