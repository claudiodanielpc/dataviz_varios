import pandas as pd
import plotnine
from plotnine import ggplot, geom_line, aes, theme

#Descarga y lectura de datos
url = "http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv"
df = pd.read_csv(url, encoding = "latin")

#Limpieza de datos
df.columns = df.columns.str.lower()

##Filtro del país a visualizar
df = df[df["entidad"] == "Ciudad de México"]

df = df.groupby(["año","sexo"], as_index=False)[['poblacion']].sum()

df["poblacion"]=df["poblacion"]/1000000


#Gráfico
p=(ggplot(df)
    + aes(x="año", y="poblacion")
    + geom_line(aes(color="sexo"), size=2) +
plotnine.theme_grey(base_family = "Century Gothic")
+plotnine.scale_color_manual(values = ("#31a354",
                                      "#e6550d"))
+ plotnine.labs(x = "Año", y = "Población (millones de personas)",
                color="Género",
       title = "Ciudad de México. Población por género, 1970-2050",
       caption = "Fuente: @claudiodanielpc con datos del Consejo Nacional de Población (CONAPO).Proyecciones de la Población de México y de las Entidades Federativas.")
   +theme(plot_title=plotnine.element_text(size=25, hjust = 1, face = "bold"),
          axis_text=plotnine.element_text(size=12),
            axis_text_x = plotnine.element_text(angle=90),
          legend_text_legend=plotnine.element_text(size=12),
          legend_title= plotnine.element_text(size=15),
          axis_title=plotnine.element_text(size=15))

 )
#Salvar gráfico
p.save("myplot.png", dpi=300, width =20 ,height = 10)