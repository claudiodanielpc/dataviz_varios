#ENDISEG 2021


##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
#Esto se debe de cambiar en cada computadora
#Crear carpeta general de trabajo en unidad D
dir.create("D:/datos/endiseg", showWarnings = F)
setwd("D:/datos/endiseg")

# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,srvyr, extrafont)



#Funciones====
##PALETA DE COLORES

#Viene de: https://github.com/joelleforestier/PridePalettes/blob/master/R/pride_palette.R
pride_palette <- function(palette) {
  
  # Pride Flag
  pride <- c("#e40303", # red
             "#ff8c00", # orange
             "#ffed00", # yellow
             "#008026", # green
             "#004dff", # blue
             "#750787") # purple
  
  # Philly People of Color Pride Flag
  philly_poc_pride <- c("#000000", # black
                        "#785017", # brown
                        "#e40303", # red
                        "#ff8c00", # orange
                        "#ffed00", # yellow
                        "#008026", # green
                        "#004dff", # blue
                        "#750787") # purple
  
  # Gilbert Baker Pride Flag
  gilbert_baker_pride <- c("#f564ae", # pink
                           "#e40303", # red
                           "#ff8c00", # orange
                           "#ffed00", # yellow
                           "#008026", # green
                           "#52ced9", # light blue
                           "#391294", # dark purple
                           "#750787") # red-purple
  
  # Bisexual Pride Flag
  bisexual_pride <- c("#D70270", # pink
                      "#734F96", # purple
                      "#0038A8") # blue
  
  # Pansexual Pride Flag
  pansexual_pride <- c("#ff218c", # pink
                       "#ffd800", # yellow
                       "#21b1ff") # blue
  
  # Asexual Pride Flag
  asexual_pride <- c("#000000", # black
                     "#a3a3a3", # grey
                     "#ffffff", # white
                     "#800080") # purple
  
  # Trans Pride Flag
  trans_pride <- c("#5bcdfa", # blue
                   "#f5a9b8", # pink
                   "#ffffff") # white
  
  # Gender Fluid Pride Flag
  genderfluid_pride <- c("#ff75a3", # pink
                         "#ffffff", # white
                         "#bd18d6", # purple
                         "#000000", # black
                         "#333fbd") # blue
  
  # Genderqueer Pride Flag
  genderqueer_pride <- c("#B77FDD", # purple
                         "#ffffff", # white
                         "#48821E") # green
  
  # Polysexual Pride Flag
  polysexual_pride <- c("#F61CB9", # pink
                        "#07D569", # green
                        "#1C92F6") # blue
  
  # Agender Pride Flag
  agender_pride <- c("#000000", # black
                     "#a3a3a3", # grey
                     "#ffffff", # white
                     "#b8f483") # green
  
  # Aromantic Pride Flag
  aromantic_pride <- c("#3da542", # green
                       "#a7d379", # light green
                       "#ffffff", # white
                       "#a3a3a3", # grey
                       "#000000") # black
  
  # Nonbinary Pride Flag
  nonbinary_pride <- c("#fff530", # yellow
                       "#ffffff", # white
                       "#9d59d1", # purple
                       "#000000") # black
  
  if (palette == "pride") {
    colors <- pride
  } else if (palette == "philly_poc_pride") {
    colors <- philly_poc_pride
  } else if (palette == "gilbert_baker_pride") {
    colors <- gilbert_baker_pride
  } else if (palette == "bisexual_pride") {
    colors <- bisexual_pride
  } else if (palette == "pansexual_pride") {
    colors <- pansexual_pride
  } else if (palette == "asexual_pride") {
    colors <- asexual_pride
  } else if (palette == "trans_pride") {
    colors <- trans_pride
  } else if (palette == "genderfluid_pride") {
    colors <- genderfluid_pride
  } else if (palette == "genderqueer_pride") {
    colors <- genderqueer_pride
  } else if (palette == "polysexual_pride") {
    colors <- polysexual_pride
  } else if (palette == "agender_pride") {
    colors <- agender_pride
  } else if (palette == "aromantic_pride") {
    colors <- aromantic_pride
  } else if (palette == "nonbinary_pride") {
    colors <- nonbinary_pride
  }
  
  return(colors)
  
}


##DESCARGA
#Crear función de descarga estableciendo el directorio de extracción de archivos
descarga<-function(url,directorio){
  temp <- tempfile()
  download.file(url, destfile = temp)
  unzip(temp, 
        exdir = directorio,
        overwrite=TRUE)
  unlink(temp)
}



#ENDISEG 2021
endiseg<-"https://www.inegi.org.mx/contenidos/programas/endiseg/2021/microdatos/endiseg_2021_bd_csv.zip"


descarga(endiseg,getwd())


#Datos generales
dato<-read.csv("TMODULO.csv")%>%
  janitor::clean_names()


##Tratamiento de datos====
dato%>%
  #Declarar factor de expansión
  as_survey(weights=factor)%>%
  #Filtro por variables de interés
  filter(p8_1a==2 & p11_1_4==1)%>%
#Agrupar
  group_by(tipo
          )%>%
  summarise(pct=survey_prop(vartype = c("cv")
  ))%>%
  ungroup()%>%
  mutate(
         pct=pct*100,
         
         tipo=case_when(
           p11_2_4_1  %in% 1:3 ~ "Ámbito familiar",
           p11_2_4_1 %in% 4:5 ~ "Escuela o trabajo",
           p11_2_4_1==6 ~ "Amigo, vecino o conocido",
           p11_2_4_1==7 ~ "Personal de salud",
           TRUE ~ "Otro"),         
         
         tipo = fct_reorder(tipo, pct)         
         
         )%>%
  #Gráfico
  ggplot(., aes(x=tipo, y=pct)) +
  geom_col(aes(fill=tipo))+
  geom_text(aes(label=format(round(pct,1))),
            hjust=0,
            color="black",
            size=6.5,fontface="bold")+
  scale_fill_manual(values = pride_palette("pride"))+
  coord_flip()+
  theme_minimal()+
  labs(title = "Personas gays u homosexuales que han sido humilladas, 
avergonzadas o le han hablado con groserías por ámbito",
       subtitle = "(Porcentaje)",
       x="",
       caption = "
Fuente: @claudiodanielpc con datos de INEGI. Encuesta Nacional sobre Diversidad Sexual y de Género (ENDISEG) 2021.")+
  theme(plot.title = element_text(hjust = 0, 
                                  size=30,
                                  face="bold"),
        plot.subtitle = element_text(hjust = 0,
                                     size=25, 
                                     face="italic"),
        plot.caption = element_text(hjust = 0,
                                    size=18),
        legend.position = "none",
        text=element_text("Century Gothic",
                          size=20),
        axis.text.x=element_text(size=15)
        
  )



##Salvar gráfico
ggsave("endiseg1.png", width = 20, height = 15, bg = "white")