import folium
import pandas as pd
from selenium import webdriver

url="https://archivo.datos.cdmx.gob.mx/victimas_completa_2019_2020.csv"
#Lectura de datos
df = pd.read_csv(url)


##Limpieza de base de datos
# Columnas a minúsculas
df.columns = df.columns.str.lower()


##Filtos de datos
df = df[df["año_hecho"] == 2020]
df = df[df["categoria"] == "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA"]
df = df[df["alcaldiahechos"] == "CUAUHTEMOC"]

#Mapa en Folium
m=folium.Map(
    location=[19.428,-99.13680],
    tiles='Stamen Terrain',
             zoom_start=13)
df.apply(lambda row:folium.CircleMarker(location=[row["latitud"], row["longitud"]],
                                              radius=10,
                                        fill_color="red").add_to(m), axis=1)
#Salvar mapa en html
m.save("mapa.png")

##Salvar mapa en png con webshot
#Abrir explorador y archivo generado
browser = webdriver.Chrome(executable_path="C:\\Program Files (x86)\\chromedriver.exe")
browser.get('C:\\Users\\ALIENWARE\\PycharmProjects\\practicas\\venv\\mapa.html')
#Salvar screenshot
browser.save_screenshot('maparobos.png')
#Cerrar navegador
browser.quit()