import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib
from matplotlib import style

df = pd.read_csv("C:/Users/ALIENWARE/Downloads/IDEFC_NM_mar2021.csv",
                 encoding="latin")
#Limpieza de datos
df.columns = df.columns.str.lower()



df = pd.melt(df, id_vars = ['año', "tipo de delito"],
             value_vars = ['enero', 'febrero',
                          'marzo', "abril",
                          "mayo", "junio",
                          "julio","agosto",
                          "septiembre", "octubre",
                          "noviembre", "diciembre"])


##Filtro del país a visualizar
df = df[(df["año"] <2021) &
        (df["tipo de delito"] == "Homicidio")]

##Transformar a numérico
df["value"] = pd.to_numeric(df["value"])


df = df.groupby(["año","variable"], as_index=False)[['value']].sum()


#Ordenar meses
ordered= ['enero','febrero','marzo','abril',
          "mayo", "junio", "julio", "agosto", "septiembre",
          "octubre", "noviembre", "diciembre"]

df['variable'] = pd.Categorical(df['variable'], categories=ordered, ordered=True)

#Plot

fig, ax = plt.subplots(figsize = ( 12 , 9 ))
matplotlib.style.use('fivethirtyeight')
sns.lineplot(x='variable', y='value', hue='año',
             style="año", ci=None, palette="viridis",
             data=df)
sns.despine()
sns.set(font="Century Gothic")
plt.title("Homicidios mensuales, 2015-2020",
          fontweight = 'bold', ha='right',
          va = 'bottom', size=15)
ax.set_ylabel('Homicidios')
ax.yaxis.set_major_formatter(matplotlib.ticker.StrMethodFormatter(
    '{x:,.0f}'))
ax.set_xlabel("Meses\nFuente: @claudiodanielpc con datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública")
ax.legend(bbox_to_anchor=(1, 1), loc=2)
plt.xticks(rotation=90, size=8)
plt.savefig("homic.png", format="png", dpi=300, transparent=False)
plt.show()