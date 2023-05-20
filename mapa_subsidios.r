##Paquetería necesaria
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,sf,showtext)


showtext_auto()
font_add_google("Montserrat","Montserrat")


urls<-c("https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/Me01R5t-RO6Lf57R8wDfAQ/content/subsidios_2022.csv?a=true",
"https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/LpsBICHsSsqr3yHHAwcYYQ/content/subsidios_2021.csv?a=true",
"https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/ALRiRn-IQeeswSiwaJo1Rg/content/subsidios_2020.csv?a=true",
"https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/7K7KYeUrSKmBMoFj8bIcvw/content/subsidios_2019.csv?a=true",
"https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/u-AUbZZ5TomsHpZb-D8Hlw/content/subsidios_2018.csv?a=true",
"https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/wdkMIo7HS4WO5kzIwmPWMA/content/subsidios_2017.csv?a=true",
"https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/8_UYSFcXQzCu75VaIHFm-g/content/subsidios_2016.csv?a=true",
"https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/1m4ZeknTTjGOs9AV0OTbYQ/content/subsidios_2015.csv?a=true",
"https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/Hzh35rixQG2O75E6n28xVQ/content/subsidios_2014.csv?a=true",
"https://sistemas.sedatu.gob.mx/repositorio/proxy/alfresco-noauth/api/internal/shared/node/tvwNfHFQSWGqABW2lbQbkQ/content/subsidios_2013.csv?a=true"
)

#Establecer directorio de trabajo
setwd("D:/")
#Crear carpeta para guardar los datos
dir.create("subsidios")

#Establecer directorio de trabajo
setwd("D:/subsidios/")


##Descarga de datos
for(i in 1:length(urls)){
  download.file(urls[i],paste0("subsidios_",i,".csv"))
}

#Leer archivos

for (i in 1:length(urls)){
  assign(paste0("subsidios_",i),read.csv(paste0("subsidios_",i,".csv"),fileEncoding="latin1"))
}


#Descarga de margo geoestadístico
#Si existe el archivo no se descarga
#options(timeout=6000)
#if(!file.exists("mg_2020_integrado/conjunto_de_datos/00mun.shp")) download.file("https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/mg_2020_integrado.zip",
#destfile="mg_2020_integrado.zip")


#Descomprimir archivo
#unzip("mg_2020_integrado.zip",exdir="mg_2020_integrado")
#if(file.exists("mg_2020_integrado.zip")) file.remove("mg_2020_integrado.zip")

#Unir archivos
subsidios<-rbind(subsidios_1,subsidios_2,subsidios_3,subsidios_4,subsidios_5,subsidios_6,subsidios_7,subsidios_8,subsidios_9,subsidios_10)%>%

#Crear clave geoestadística
  ##Agregar cero a entidad
  mutate(cve_ent=ifelse(nchar(cve_ent)==1,
                        paste0("0",cve_ent),
                    cve_ent))%>%
  group_by(cve_ent,año)%>%
    summarise(acciones=sum(acciones))%>%
    ungroup()%>%
    #Crear variable de administración
    mutate(admon=case_when(
        año>=2013 & año<=2018~"2013-2018",
        año>=2019~"2019-2022"))


#Subsidios por administración y municipio

datosmapa<-subsidios%>%
group_by(cve_ent,admon)%>%
summarise(subsidios=sum(acciones))%>%
ungroup()%>%
group_by(admon)%>%
mutate(pct=subsidios/sum(subsidios)*100)%>%
ungroup()%>%
#Eliminar NA
filter(!is.na(cve_ent))


#Capa de entidades
estados<-st_read("mg_2020_integrado/conjunto_de_datos/00ent.shp")%>%
janitor::clean_names()%>%
left_join(datosmapa)

#Crear mapa
estados%>%
ggplot()+
geom_sf(aes(fill=pct),color="white")+
    scale_fill_distiller("%", guide=guide_legend(reverse=TRUE),
                         palette = "YlOrBr", direction = 1, limits=c(min(datosmapa$pct),
                         max(datosmapa$pct)))+
 theme_void()+
labs(title="Subsidios CONAVI por administración y entidad federativa",
     subtitle="% respecto del total por administración\n",
     caption="Fuente: @claudiodanielpc con datos de SEDATU. Sistema Nacional de Información e Indicadores de Vivienda (SNIIV).",
     fill="Subsidios")+
             theme(plot.title = element_text(hjust = 0, size=30,face="bold"),
                   
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position="right",
        #Fuente y tamaño
        text=element_text("Montserrat",size=15),
        strip.text = element_text(
          size = 20, color = "black", 
          face = "bold.italic")
        
        )+

facet_wrap(~admon,
           ncol=2)

ggsave("mapasubsidios.png", height = 10,
       width = 15, units="in", dpi=100, bg="white")

