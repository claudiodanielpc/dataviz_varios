##Estimación de soluciones habitacionales para el rezago habitacional

#Se borra todo lo que se encuentra en el entorno

rm(list=ls())

# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, geofacet)


#Directorio de trabajo
setwd("D:/")

#Descarga de archivos
url<-"https://www.inegi.org.mx/contenidos/programas/natalidad/datosabiertos/2020/conjunto_de_datos_natalidad_2020_csv.zip"

##Creación de directorio temporal

td<- tempdir()

# Descarga del archivo temporal


tf = tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)


unzip(tf, files="conjunto_de_datos/conjunto_de_datos_natalidad_2020.csv", exdir=td, 
      overwrite=TRUE)


path=file.path(td,"conjunto_de_datos/conjunto_de_datos_natalidad_2020.csv")
unlink(td)


nat<-read.csv(path)%>%
  #Crear grupos de edad
  mutate(gpo=case_when(
    edad_madn<15 ~ "Menor de 15",
    edad_madn>=15 & edad_madn<=19 ~ "15 a 19",
    edad_madn>=20 & edad_madn<=24 ~ "20 a 24",
    edad_madn>=25 & edad_madn<=29 ~ "25 a 29",
    edad_madn>=30 & edad_madn<=34 ~ "30 a 34",
    edad_madn>=35 & edad_madn<=39 ~ "35 a 39",
    edad_madn>=40 & edad_madn<=44 ~ "40 a 44",
    edad_madn>=45 & edad_madn<=49 ~ "45 a 49",
    edad_madn>=50 & edad_madn<99 ~ "50 y más",
    edad_madn==99 ~ "No especificado"),
    ##Variable entidad
    entidad= case_when(ent_ocurr==1 ~ "Aguascalientes",
                       ent_ocurr==2 ~ "Baja California",
                       ent_ocurr==3 ~ "Baja California Sur",
                       ent_ocurr==4 ~ "Campeche",
                       ent_ocurr==5 ~ "Coahuila",
                       ent_ocurr==6 ~ "Colima",
                       ent_ocurr==7 ~ "Chiapas",
                       ent_ocurr==8 ~ "Chihuahua",
                       ent_ocurr==9 ~ "Ciudad de México",
                       ent_ocurr==10 ~"Durango",
                       ent_ocurr==11 ~ "Guanajuato",
                       ent_ocurr==12 ~ "Guerrero",
                       ent_ocurr==13 ~ "Hidalgo",
                       ent_ocurr==14 ~ "Jalisco",
                       ent_ocurr==15 ~ "México",
                       ent_ocurr==16 ~ "Michoacán",
                       ent_ocurr==17 ~ "Morelos",
                       ent_ocurr==18 ~ "Nayarit",
                       ent_ocurr==19 ~ "Nuevo León",
                       ent_ocurr==20 ~ "Oaxaca",
                       ent_ocurr==21 ~ "Puebla",
                       ent_ocurr==22 ~ "Querétaro",
                       ent_ocurr==23 ~ "Quintana Roo",
                       ent_ocurr==24 ~ "San Luis Potosí",
                       ent_ocurr==25 ~ "Sinaloa",
                       ent_ocurr==26 ~ "Sonora",
                       ent_ocurr==27 ~ "Tabasco",
                       ent_ocurr==28 ~ "Tamaulipas",
                       ent_ocurr==29 ~ "Tlaxcala",
                       ent_ocurr==30 ~ "Veracruz",
                       ent_ocurr==31 ~ "Yucatán",
                       ent_ocurr==32 ~ "Zacatecas",
                         TRUE ~ "Extranjero"
    ))%>%
    group_by(ent_ocurr,entidad,gpo)%>%
  #Crear porcentaje
tally%>%
  mutate(pct=(100*n)/sum(n))%>%
  ungroup()

#Generar datos para subtítulo
#Nacimientos totales
general<-nat%>%
  summarise(total=sum(n))%>%
  as.integer()


#Nacimientos en el extranjero
extr<-nat%>%
  filter(
    entidad=="Extranjero"
  )%>%
  summarise(total=sum(n))%>%
  as.integer()


#Gráfica


nat%>%
  mutate(gpo = fct_relevel(gpo, 
                            "Menor de 15", "15 a 19", "20 a 24", 
                            "25 a 29", "30 a 34", "35 a 39","40 a 44",
                           "45 a 49", "50 y más", "No especificado"))%>%
  ggplot(.,aes(gpo,pct))+
  geom_bar(stat="identity",width=.8, fill="#bcbddc")+
  
  #Forma de mapa
  facet_geo(~ ent_ocurr, 
            grid = mx_state_grid2, 
            label = "name",
            scales = "free_y")+
  theme_minimal() +
  labs(
    title = "México. Nacimientos registrados por entidad de ocurrencia y edad de la madre al momento
del nacimiento, 2020",
    subtitle = paste0("Totales: ",format(general,big.mark = ","),"\n",
"En el extranjero: ",format(extr,big.mark = ",")),
    caption = "
Fuente: @claudiodanielpc con datos de INEGI. Estadísticas de Natalidad.",
    y="Porcentaje")+

  
  theme(plot.title = element_text(hjust = 0,
                                  size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, 
                                     size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size=9),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 10, face="bold", color="#8c510a"),
        
        #Fuente del gráfico
        text=element_text("Century Gothic"))
  

##Salvar gráfico
ggsave("nac2020.png", width = 16.5, height = 11.5, bg = "white")
  




