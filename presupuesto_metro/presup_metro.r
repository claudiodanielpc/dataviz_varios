##Prespuesto del Metro 2018-2022

#Se borra todo lo que se encuentra en el entorno

rm(list=ls())

# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,gt, showtext)


font_add_google("Montserrat", "Montserrat")

showtext_auto()
#Fuente general
fuente<-"Montserrat"

#Leer archivo del INPC
inpc<-read.csv("D:/github/dataviz_varios/presupuesto_metro/INP_INP20230108203127.csv")%>%
#Concatenar 20 al principio y extraer el año de la variable fecha
mutate(ciclo=as.numeric(paste0("20",substr(fecha,5,7))))%>%
#Obtener inpc promedio anual 
group_by(ciclo)%>%
summarise(inpc_promedio=mean(indice))%>%
ungroup()%>%
#Cambiar base inpc 2022=100
mutate(inpc_cambio_base=round(inpc_promedio*100/inpc_promedio[ciclo==2022],2))

#URLs de datos
url2018<-"https://datos.cdmx.gob.mx/dataset/3a6cba97-adb1-4f35-9df1-984cc3657aca/resource/345caf5a-ad85-45f3-8930-4452a72ef73e/download/egresos_2018.csv"
url2019<-"https://datos.cdmx.gob.mx/dataset/3a6cba97-adb1-4f35-9df1-984cc3657aca/resource/105b28e2-28c9-45db-951d-2641a4ac114a/download/egresos_2019.csv"
url2020<-"https://datos.cdmx.gob.mx/dataset/3a6cba97-adb1-4f35-9df1-984cc3657aca/resource/6d26726d-0993-4c2e-a902-cef0edb1dc81/download/egresos_2020.csv"
url2021<-"https://datos.cdmx.gob.mx/dataset/3a6cba97-adb1-4f35-9df1-984cc3657aca/resource/4097b0e0-6dd8-4c21-a274-d7b2775db3ed/download/egresos_2021.csv"
url2022<-"https://datos.cdmx.gob.mx/dataset/3a6cba97-adb1-4f35-9df1-984cc3657aca/resource/5c6fb123-5a2f-4ac3-b033-48e12f59973c/download/egresos_2022_t2.csv"

#Leer urls
egresos2018<-read.csv(url2018, encoding = "latin-1")
egresos2019<-read.csv(url2019, encoding = "latin-1")
egresos2020<-read.csv(url2020, encoding = "latin-1")
egresos2021<-read.csv(url2021, encoding = "latin-1")
egresos2022<-read.csv(url2022,encoding = "latin-1")


#Crear catálogo de partida específica
catalogo<-egresos2022%>%select(concepto,desc_concepto)%>%distinct()

#Unir ambas bases de datos
egresos<-rbind(egresos2018,egresos2019,egresos2020,egresos2021,egresos2022)%>%

#filtrar por UR y aprobado
filter(desc_unidad_responsable=="Sistema de Transporte Colectivo Metro" &
 periodo=="Aprobado")

 #Agrupar y sumar
egresos%>%
group_by(ciclo, concepto)%>%
summarise(total=sum(monto_aprobado))%>%
ungroup()%>%
#Pegar inpc
left_join(inpc)%>%
#Deflactar usando inpc_cambio_base
mutate(total_deflactado=total/inpc_cambio_base*100)%>%
#Cifras en millones de pesos
mutate(total_deflactado=round(total_deflactado/1000000,1))%>%
#pivot wider con años como columnas y concepto como filas
pivot_wider(id_cols = c("concepto"),
            names_from = ciclo,
            values_from = total_deflactado)%>%
left_join(catalogo)%>%
#Eliminar partida específica duplicada usando distinct
distinct(concepto, .keep_all = TRUE)%>%
  janitor::adorn_totals()%>%
mutate(desc_concepto=case_when(
desc_concepto=="-"~ "Total",
TRUE ~desc_concepto

))%>%

 
#Crear variaciones
mutate(variacion_2018_2019=round((`2019`/`2018`-1)*100,1),
       variacion_2019_2020=round((`2020`/`2019`-1)*100,1),
       variacion_2020_2021=round((`2021`/`2020`-1)*100,1),
       variacion_2021_2022=round((`2022`/`2021`-1)*100,1),

       #Crear tasa de crecimiento promedio anual de 2022 vs 2018
       tcma=round(((`2022`/`2018`)^(1/4)-1)*100,1))%>%
       #Reemplazar NA por 0 en cada columna
  mutate_all(funs(replace(.,is.na(.),0)))%>%

       #Formato de miles
  mutate_all(funs(format(.,big.mark = ",")))%>%

       #Eliminar columna de concepto y poner descripción al principio
       select(!concepto)%>%
         select(desc_concepto, everything())%>%
                #Tabla
  gt(rowname_col = "row", groupname_col = "group") %>%
  #Título y subtítulo
  tab_header(
    title = md("**Presupuesto del Sistema de Transporte Colectivo Metro, 2018-2022**"),
    subtitle = md("*(Millones de pesos, base 2022)*")) %>%
#Columnas  
  cols_label(

    desc_concepto = md("**Descripción**"),
    `2018` = md("**2018**"),
    `2019` = md("**2019**"),
    `2020` = md("**2020**"),
    `2021` = md("**2021**"),
    `2022` = md("**2022**"),
    variacion_2018_2019 = md("**Variación % 2018-2019**"),
    variacion_2019_2020 = md("**Variación % 2019-2020**"),
    variacion_2020_2021 = md("**Variación % 2020-2021**"),
    variacion_2021_2022 = md("**Variación % 2021-2022**"),
    tcma = md("**Tasa de crecimiento promedio anual 2018-2022**")
  )%>%
   #Fuente y alineación  
  opt_table_font(
    font =google_font("Montserrat") 
  )%>%
  opt_align_table_header(align = "left")%>%
  cols_align(
    align = "center"
  )%>%
  #Colores
  tab_options(
    table.border.top.color = "white"
  )%>%
  tab_style(
    style=list(
    cell_fill(color ="#756bb1"),
    cell_text(align = "center", color = "white", 
              weight = "bold")
    
    ),
    locations = cells_column_labels(columns = everything())    
    #Añadir fuente de los datos
    )%>%
#Pintar última fila
 tab_style(
    style = list(cell_fill(color = "#bdbdbd"),
    cell_text(weight="bold",color="black")),
    locations = cells_body(columns = everything(), rows = length(desc_concepto))
  )%>%


    tab_footnote(
    footnote = "Fuente: @claudiodanielpc con datos del Gobierno de la Ciudad de México.",

    
    )->presupuesto


gtsave(presupuesto,"D:/github/dataviz_varios/presupuesto_metro/tabla.png", vwidth = 1600, vheight = 1000)
