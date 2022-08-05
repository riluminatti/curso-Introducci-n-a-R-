## ACTIVIDAD FINAL - OPCION 1: VIZ Y MAPAS ##

#En este archivo, te proponemos que trabajes con la tabla de datos del 
#[Sistema Nacional de Estadísticas sobre Ejecución de la Pena - SNEEP del 
#Ministerio de Justicia y Derechos Humanos de la Nación]
#(http://datos.jus.gob.ar/dataset/sneep)

#Vamos a usar los paquetes que hemos visto en el curso para importar, 
#procesar y visualizar datos con gráficos y mapas.

#A partir de la información analizada, también deberás responder algunas 
#preguntas que encontrarás en este código.

# ATENCION #

#- Si vas a hacer el trabajo con un compañerx (equipos de 2 personas máximo), 
#tendrán que resolver las DOS propuestas de actividad 
#(Opcion 1: Viz y Mapas + Opcion 2: Análisis de Texto). 
#Si elegis trabajar de forma individual, deberás resolver una sola Opción.

#- Si te frenas en algun ejercicio, no dudes en buscar el error en internet 
#(stackoverflow, etc.), repasar los ejercicios y el material de cada clase.

#- Si la situación se complica, podés compartirlo en el Foro o a mi buzón de 
#correo con la captura de pantalla y el error que devuelve la consola.

#- Formato de entrega: R Markdown o R Script. Si elegís el segundo formato, 
#podés trabajar sobre este mismo archivo, guardar los cambios y subirlo 
#al "Espacio de entrega - Actividad Final Obligatoria". 
#No es necesario adjuntar las tablas de datos.  
#Si trabajas en equipo, una persona puede subir el archivo en 
#representación de la dupla de trabajo. Por favor, recordá poner tu nombre 
#y apellido y el de tu compañerx si corresponde.

#- Agregar #comentarios con el paso a paso de cada instrucción de código.

#- Antes de enviar el trabajo, revisa que el código se ejecute sin arrojar 
#ningun error.

#- FECHA LIMITE DE ENTREGA: LUNES 12 DE JULIO.

#- Por último, ¡muchos exitos en esta actividad! 
#De seguro, lo podrás resolver :)

# 1. ¡Comencemos! Activemos los paquetes que necesitamos.



# 2. Descargamos el dataset del censo 2019 del Sistema Nacional de 
#Estadisticas sobre Ejecucion de la pena. Acá el [link]
#(http://datos.jus.gob.ar/dataset/6c03af36-6a1d-4306-b2a8-dd39ad73afb3/resource/af0a64da-6d06-45cf-a86c-de00f09221d8/download/sneep-2019.csv) 
#o podes importar la tabla sneep-2019 desde la carpeta de la Actividad.
library(readr)
      censo_2019_ejcucion_pena<-read.csv("http://datos.jus.gob.ar/dataset/6c03af36-6a1d-4306-b2a8-dd39ad73afb3/resource/af0a64da-6d06-45cf-a86c-de00f09221d8/download/sneep-2019.csv")
# Analizaremos los resultados del censo del 2019



# 3. Exploremos los datos

View(censo_2019_ejcucion_pena)# para mirar el dataset
Names(censo_2019_ejcucion_pena)# miro el nombre de las columnas
head(censo_2019_ejcucion_pena)# primeros 6 registros
summary(censo_2019_ejcucion_pena)# resumen



# 4. Transformemos las columnas fecha de condena y fecha de detención 
#en formato fecha. La función ``ymd()`` nos ayudara.

library(lubridate)
library(dplyr)


censo_2019_ejecucion_pena_formato_fecha<-mutate(censo_2019_ejcucion_pena,fecha_detencion=ymd(fecha_detencion), fecha_condenado=ymd(fecha_condenado))
# 5. Analicemos de qué nacionalidad son lxs internxs extranjeros

#Procesemos la informacion y armemos una tabla con el recuento
#segun nacionalidad extranjera

censo_2019_ejecucion_pena_extrajeros<-censo_2019_ejcucion_pena%>%filter(nacionalidad_descripcion!="Argentina")%>%select(nacionalidad_descripcion)%>%table()%>%as.data.frame()






#Opcional: Podemos imputar un nombre a la categoría que esta vacía, 
#ej: "Sin datos"
datos$columna[4] <- "S/D"

# 6. Grafiquemos. Recorda colocar todos los detalles de la visualizacion
#(titulo, subitulo, fuente, etiquetas, tema, rellenos de colores, etc)

censo_2019_ejecucion_pena_extrajeros<-rename(censo_2019_ejecucion_pena_extrajeros,Nacionalidad=.,Cantidad_de_delitos=Freq)
library(ggplot2)
ggplot(censo_2019_ejecucion_pena_extrajeros)+ 
  geom_col(aes(x=Nacionalidad,y=Cantidad_de_delitos, fill=Nacionalidad))+ 
  ggtitle("Cantidad de Delitos por Nacionalidad Correspondientes al Censo de Unidades Penitenciarias 2019")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
  


 
                                                        






#PREGUNTA: ¿Cuales son las 3 nacionalidades con mas internxs?
#RESPONDER AQUI: 

#Paraguaya, Peruana y Boliviana.

# 7. Analicemos la evolucion de lxs internos a lo largo del tiempo
#segun anio y nivel de instruccion
summary(censo_2019_ejecucion_pena_formato_fecha)

# 7.a. Transformemos la información que necesitamos.

#IMPORTANTE: correr estas lineas para extraer el año de detencion
censo_2019_ejecucion_pena_formato_fecha$anio <- year(censo_2019_ejecucion_pena_formato_fecha$fecha_detencion)
class(censo_2019_ejecucion_pena_formato_fecha$anio)
# censo_2019_ejecucion_pena_formato_fecha$anio <- as.Date.character(censo_2019_ejecucion_pena_formato_fecha$anio, format = "%Y")



#Procesemos la informacion que necesitamos

censo_2019_ejcucion_pena_instruccion<-censo_2019_ejecucion_pena_formato_fecha%>%group_by(anio)%>%select(nivel_instruccion_descripcion)%>%table()%>%as.data.frame()%>%filter()



#Linea opcional: imputemos categorias vacías
datos$nivel_instruccion_descripcion[c(1:24)] <- "S/D"

# 7.b. Visualicemos un grafico de linea faceteado por nivel de instruccion.
ggplot(censo_2019_ejcucion_pena_instruccion)+
  geom_line(aes(x=as.numeric(as.character(anio)), y=Freq,color=nivel_instruccion_descripcion,group=nivel_instruccion_descripcion))+
  facet_wrap(~nivel_instruccion_descripcion,nrow = 2)+
  ggtitle("Cantidad de Internos según nivel de instrucción por año correspondientes al Censo de Unidades Penitenciarias 2019")+
  ylab("Cantidad de Internos")+
  xlab("Año")+
  theme_minimal()









#PREGUNTA: ¿En cuáles niveles de instrucción se observó un crecimiento exponencial
#(mayor a 2500 detenidxs) a lo largo del tiempo?
#RESPONDER AQUI: 
#Primario completo, Primario Incompleto, Secundario Completo, Secundario Incompleto
# 8. Analicemos la distribución de internxs según edad
censo_2019_ejecucion_edad<-censo_2019_ejecucion_pena_formato_fecha%>%select(edad)%>%table()%>%as.data.frame()

#Podemos usar la función table() para consultar un paneo general y después ver
#si debemos filtrar alguna información para mejorar la visualizacion


datos_filtrados_edad<-censo_2019_ejecucion_pena_formato_fecha%>%filter(edad!=0)
#Procesamos la información y graficamos un histograma con los elementos necesarios


ggplot(datos_filtrados_edad)+
  geom_histogram(aes(x=as.numeric(as.character(edad))),fill="lightblue")+
  ggtitle("Internos según la Edad")+
  ylab("Cantidad")+
  xlab("edad")+
  theme_minimal()






#Grafiquemos un diagrama de densidad con opacidad

ggplot(datos_filtrados_edad)+
  geom_density(aes(x=as.numeric(as.character(edad))),fill="lightblue",alpha=0.5)+
  ggtitle("Internos según la Edad")+
  ylab("Cantidad")+
  xlab("edad")+
  theme_minimal()









#PREGUNTA: ¿En qué rango de edad se registra la mayor frecuencia de internxs?
#RESPONDER AQUI: 
#25
#PREGUNTA: ¿Cuál es la media de edad que se registra para lxs internxs?
mean(datos_filtrados_edad$edad)
#RESPONDER AQUI: 
#34.49015
#Calculamos la media de edad



# 9. Analicemos si hay una relacion entre la edad y la duracion de condena en 
#años en lxs internxs de sexo transexual.
internxs_trans<-censo_2019_ejecucion_pena_formato_fecha%>%filter(sexo_descripcion=="Transexual")%>%select(edad,duracion_condena_anios,calificacion_conducta_descripcion)

#Procesemos la data y grafiquemos un diagrama de dispersion (scatter plot)
#que muestre la relación entre edad y años de condena de lxs internxs trans y
#nos indique para cada observacion la calificacion de su conducta.
ggplot(internxs_trans)+
  geom_point(aes(x= duracion_condena_anios, y=edad,color=calificacion_conducta_descripcion,shape= calificacion_conducta_descripcion))+
  ggtitle("Población Transexual Carcelaria según censo 2019: años de condena según la edad y su conducta")+
  ylab("Edad")+
  xlab("Duración de la condena en años")+
 theme_minimal()








#PREGUNTA: ¿Hay alguna relación entre la edad, la duración y la conducta en
#lxs internos trans? Conta tu opinion en base al grafico diseñado
#RESPONDE AQUI: 
#la calaificación mala y pésima se observa en los residentes de condenas de 5 años o más y de menos de 40 años.


# 10. Descarguemos el mapa de provincias geojson para seguir analizando otras
#variables. La URL esta aqui: https://github.com/idera/Polymaps-Argentina/raw/master/provincias.json

library(sf)
provincias<-st_read("https://github.com/idera/Polymaps-Argentina/raw/master/provincias.json")

provincias <- provincias[-c(18),] # Eliminamos el doble registro de Entre Rios


# 11. Realicemos un mapa de densidad con ggplot tomando en cuenta la cantidad 
#de internxs por provincia del establecimiento penitenciario filtrado por
#delitos contra la administración pública

# 11.a. Procesemos la data que necesitamos. Recorda que vamos a hacer un join
#espacial para graficar!

internos_delitos_adminpublica<-censo_2019_ejecucion_pena_formato_fecha%>%filter(delito1_descripcion=="Delitos c/ la administracion pública")%>%select(provincia_indec_id)%>%table()%>%as.data.frame()
internos_delitos_adminpublica<-rename(internos_delitos_adminpublica,prov=.,cantidad=Freq)
internos_delitos_adminpublica$prov<-as.numeric(as.character(internos_delitos_adminpublica$prov))


# 11.b. Joiniemos datos geográficos con los datos generados en el punto anterior.

datos_juntos<-full_join(internos_delitos_adminpublica,provincias,by="prov")
head(datos_juntos)





# 11. c. Grafiquemos con ggplot.


ggplot(datos_juntos,aes(geometry=geometry,fill=cantidad))+
  geom_sf()+
  ggtitle("Internos con Condena por Delitos a la Administración Pública")+theme_minimal()





#PREGUNTA: ¿Cuáles son las 3 provincias que registraron mayor cantidad de internxs
#con delitos contra la Administración Pública?
#RESPONDE AQUI: 
#Buenos Aires, Córdoba y Mendoza
# 12. Vamos a darle interactividad con leaflet pero esta vez usaremos
#todxs lxs internxs en general.

# 12. a. Procesemos la informacion que necesitamos usar.

#Procesemos la data que necesitamos.


internos_delitos<-censo_2019_ejecucion_pena_formato_fecha%>%select(provincia_indec_id)%>%table()%>%as.data.frame()
internos_delitos<-rename(internos_delitos,prov=.,cantidad=Freq)
internos_delitos$prov<-as.numeric(as.character(internos_delitos$prov))



#Joiniemos datos geográficos con los datos de internxs por provincia. 


datos_juntos_nuevo<-full_join(internos_delitos,provincias,by="prov")
head(datos_juntos_nuevo)

ggplot(datos_juntos_nuevo,aes(geometry=geometry,fill=cantidad))+
  geom_sf()+
  ggtitle("Internos con Condena por provincia")+theme_minimal()




# 12. b. Grafiquemos un mapa en leaflet.

#paleta de colores
library(rgdal)
library(leaflet)
pal_internos <- colorFactor("Reds", domain = datos_juntos_nuevo$cantidad)

#agregar etiquetas

#intenté hacer el mapa interactivo pero no me salió, no encuentro el archivo .sph

etiquetas_provincias <- sprintf(
  "<strong>%s</strong><br/>%s Espacios",
  datos_juntos_nuevo$provincia, datos_juntos_nuevo$cantidad) %>% 
  lapply(htmltools::HTML)


#Mapeamos
leaflet(datos_juntos_nuevo) %>%
  addTiles() %>%
  addPolygons(weight = 2, opacity = 1, smoothFactor = 1, fillOpacity = 0.9,
              color = pal_internos(datos_juntos_nuevo$cantidad),label = etiquetas_dist,
              highlightOptions = highlightOptions(color = pal_internos(pal_internos(datos_juntos_nuevo$cantidad)),
                                                  weight = 10, bringToFront = TRUE, fillOpacity = 0.6)) 


m_internos<-leaflet(datos_juntos_nuevo)%>%
  addProviderTiles("Stamen.Toner",group = "cantidad")%>%
  addMarkers(label = ~cantidad) %>% 
  setView(lng = -64.453492, lat = -39, zoom = 4)



#graficar mapa

m_internos





# 13. OPCIONAL. Si te animas, agreguemos un filtro por sexo al mapa de leaflet.

# 13.b. Primero, procesemos la info que necesitamos.

#Procesemos la tabla que necesitamos



                    
#Joineamos



# 13.c. Grafiquemos.

#paleta de colores


#agregar etiquetas




#graficar mapa







## FIN DE LA ACTIVIDAD FINAL - ¡MUCHAS GRACIAS! ##