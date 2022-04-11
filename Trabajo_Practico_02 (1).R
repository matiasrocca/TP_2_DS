rm(list=ls())
setwd() # Definir directorio de trabajo.

#~~ Introducción a Ciencia de Datos: Taller 02 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Ejercicio 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
datos_bicis= read.table("recorridos-realizados-2017.csv", header = TRUE,
                        sep = ",")

## Ejercicio 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dim(datos_bicis) #Tiene 1886192 viajes (unidades de análisis) y 9 variables

## Ejercicio 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
datos_bicis$bici_Fecha_hora_retiro= as.POSIXct(datos_bicis$bici_Fecha_hora_retiro,format="%Y-%m-%d %H:%M:%OS")

## Ejercicio 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
datos_bicis$bici_tiempo_uso_secs= datos_bicis$bici_tiempo_uso*60

## Ejercicio 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
"Teniendo fecha de retiro y tiempo de uso en segundos, es simple crear otra 
variable, dado que ambas se encuentran grabadas en segundos contados desde 1970:"
datos_bicis$bici_Fecha_hora_destino= datos_bicis$bici_Fecha_hora_retiro + datos_bicis$bici_tiempo_uso_secs

## Ejercicio 6 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
datos_bicis$weekday= strftime(datos_bicis$bici_Fecha_hora_retiro, format = "%w")  # Devuelve el día.
datos_bicis$weekday <- as.numeric(datos_bicis$weekday)

## Ejercicio 7 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
datos_bicis$fin_de_semana= ifelse(datos_bicis$weekday %in% c(0, 6), "Fin de semana", "Día de semana")
datos_bicis$fin_de_semana= as.factor(datos_bicis$fin_de_semana)

## Ejercicio 8 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
datos_bicis$hour= strftime(datos_bicis$bici_Fecha_hora_retiro, format = "%H")  
datos_bicis$hour= as.numeric(datos_bicis$hour)
#Nos devuelve la hora de dicha variable (extracción)

## Ejercicio 9 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
datos_bicis$misma_estacion= factor(ifelse(datos_bicis$bici_nombre_estacion_origen == datos_bicis$bici_nombre_estacion_destino , "Misma estación", "Distinta estación"))

## Ejercicio 10 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(ggplot2)
"Para visualizar esto, tenemos que armar un grafico en el que los viajes en min 
se encuentren en el eje y, y la variable fin_de_semana en el eje x:"
(ggplot(data = datos_bicis, mapping= aes(fin_de_semana, bici_tiempo_uso, na.rm= TRUE)) 
 + geom_boxplot() + xlab("Día") + ylab("Tiempo de uso (en min)") 
 + coord_cartesian(ylim = c(0,100)) 
 + ggtitle("Distribución de la duración de los viajes"))

## Ejercicio 11 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
datos_bicis$fecha= as.Date(datos_bicis$bici_Fecha_hora_retiro, tz = "America/Argentina/Buenos_Aires")

## Ejercicio 12 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(dplyr)

hora_sem= group_by(datos_bicis, fin_de_semana, hour)
frecuencia= summarise(hora_sem, cant = n())
frecuencia$promedio= ifelse(frecuencia$fin_de_semana=="Día de semana", frecuencia$cant/5, frecuencia$cant/2)
###Se podría haber hecho todo con pipes, pero nos fue más facil pensarlo por partes
(ggplot(data = frecuencia, mapping= aes(hour, promedio, color=fin_de_semana)) 
  + geom_point() + xlab("Horas") +  ylab("Promedio de extracción") 
  + ggtitle("Distribución de la cantidad de extracciones en un día típico") 
  + geom_line())

## Ejercicio 13 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
datos_bicis %>% group_by(misma_estacion, fin_de_semana) %>% summarise(cant = n())
"RTA: Es más común que se extraiga y devuelva una bici en la misma estación 
en los días de semana (117 462 contra 50 734 casos en los findes)"

## Ejercicio 14 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(reshape2)
ej_14= table(datos_bicis$bici_nombre_estacion_origen, datos_bicis$bici_sexo) %>% 
  as.data.frame() %>% dcast(Var1 ~ Var2) %>% select(-"NO INFORMADO") %>% 
  mutate(total_extracc = MASCULINO + FEMENINO) %>% select(-"MASCULINO") %>% 
  filter(total_extracc>=6000) %>% mutate(prop.mujeres= FEMENINO/total_extracc) %>% arrange(desc(prop.mujeres))
ej_14

## Ejercicio 15 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
est_mas_1000=datos_bicis %>% group_by(bici_nombre_estacion_origen) %>% count() %>% filter(n>=1000) 
ej_15= datos_bicis %>% filter(bici_sexo!="NO INFORMADO",
                       bici_nombre_estacion_origen %in% est_mas_1000$bici_nombre_estacion_origen,
                       !is.na(fin_de_semana),
                       !is.na(misma_estacion))
grafico= ggplot(ej_15, aes(x=bici_tiempo_uso, y= bici_sexo, fill=bici_sexo))+
  geom_boxplot()+
  facet_grid(misma_estacion~fin_de_semana)+
  coord_cartesian(xlim = c(0,100))+
  xlab("Tiempo de uso") +
  ylab("Sexo") +
  ggtitle("Distribución de extracciones")

grafico

## Ejercicio 16 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
ggsave("fig1.jpg", grafico, width = 15, height = 10, units = "cm")

## Ejercicio 17 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
ej_17= datos_bicis %>% group_by(fecha, bici_sexo) %>% 
  summarise(total= length(bici_nombre_estacion_origen)) %>% filter(bici_sexo!="NO INFORMADO")

(ggplot(data = ej_17) + 
    geom_line( mapping = aes(x = fecha, y = total, color= bici_sexo), na.rm = TRUE)
  + xlab("Días del año") 
  + ylab("Cantidad de extracciones (por día)") 
  + ggtitle("Evolución de la cantidad de extracciones")
  + geom_smooth(mapping = aes(x = fecha, y = total, color= bici_sexo)))

## Ejercicio 18 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
"Creamos una variable que posea la cantidad de extracciones por estación y, a su
vez, se encuentren ordenadas de mayor a menor"
ej_18= datos_bicis %>% group_by(bici_nombre_estacion_origen) %>% 
  summarise(total= length(bici_nombre_estacion_origen)) %>% arrange(desc(total))

#Pasamos la cantidad total de extracciones a miles:
ej_18= ej_18 %>% mutate(total=total/1000)

#Le pido que se quede solo con las primeras 20 estaciones:
ej_18= ej_18[0:20,]
ej_18$bici_nombre_estacion_origen= as.factor(ej_18$bici_nombre_estacion_origen)

#Lo ploteamos:

(ggplot(data = ej_18) 
  + geom_col(mapping= aes(reorder(bici_nombre_estacion_origen, total), total)) 
  + xlab("Estación") 
  + ylab("Extracciones") 
  + ggtitle("Distribución de la extracción según estación") 
  + coord_flip())
