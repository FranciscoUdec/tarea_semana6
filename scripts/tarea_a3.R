# Tarea A3 semana 6
# Autor: Francisco Inostroza
# Fecha: 29-09-2026
# Qué hace: Limpieza de la ENE para evaluar la brecha de género en las 
#           horas efectivas de trabajo.

library(dplyr)


# 1. PREGUNTA ECONÓMICA 
# Pregunta: ¿Existe una brecha por sexo en las horas efectivas que trabajan 
#          los ocupados en Chile?

# Ficha de la fuente:
# - Institución: INE Chile
# - Encuesta: Encuesta Nacional de Empleo (ENE)
# - Trimestre: Mayo - Junio - Julio 2026 (mjj)
# - Archivo: ene-2026-06-mjj.csv
# - Enlace: https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2026/csv/ene-2026-06-mjj.csv
# - Fecha descarga: 29-09-2026
# - Unidad de observación: Personas en edad de trabajar en viviendas particulares


# 2. CARGA de datos

ntento <- read.csv("data/raw/ene-2026-06-mjj.csv", nrows = 5)
ncol(intento)
# read.csv() no sirve porque el INE separa columnas con ";" y decimales con ",".
ene <- read.csv2("data/raw/ene-2026-06-mjj.csv")

# Reducción usando starts_with()
sub_ene <- ene |>
  select(sexo, activ, fact_cal, starts_with("efect"))

dim(sub_ene)  # 97.946 filas y 4 columnas


# 3. CASE_WHEN()
sub_ene <- sub_ene |>
  mutate(
    sexo_txt = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      TRUE      ~ "Otro"
    ),
    situacion = case_when(
      activ == 1 ~ "Ocupado",
      activ == 2 ~ "Desocupado",
      activ == 3 ~ "Inactivo",
      TRUE       ~ "Fuera de edad de trabajar"
    )
  )

# Verificación de ausencia de valores NA inesperados
table(sub_ene$sexo_txt, useNA = "ifany")    
table(sub_ene$situacion, useNA = "ifany")  


# 4. FALTANTES
summary(sub_ene)
# La variable efectivas presenta 56.096 NA.

# ¿De quiénes son los NA en efectivas?
table(sub_ene$situacion[is.na(sub_ene$efectivas)], useNA = "ifany")
# Da 36.379 inactivos, 4.412 desocupados y 15.305 fuera de edad.
# Son 56.096 faltantes ESTRUCTURALES: a las personas no ocupadas no se les hace esta pregunta.

nrow(na.omit(sub_ene))
# Elimina 56.096 observaciones. Sesga el análisis porque borra gran parte de la muestrra


# 5. CAZA DE CENTINELAS 
sort(table(sub_ene$efectivas[sub_ene$efectivas > 80]), decreasing = TRUE)

oc <- sub_ene$efectivas[sub_ene$situacion == "Ocupado"]
c(crudo = mean(oc, na.rm = TRUE),
  sin1 = mean(oc[oc != 999], na.rm = TRUE),
  sin2 = mean(oc[!(oc %in% c(888, 999))], na.rm = TRUE))

37.73 - 35.45 
# Los centinelas inflan el promedio en 2,29 horas. 
# Pasar a NA reales:
sub_ene <- sub_ene |>
  mutate(
    efectivas = na_if(efectivas, 888),
    efectivas = na_if(efectivas, 999)
  )

max(sub_ene$efectivas, na.rm = TRUE)
# Da 140 horas 


# 6. GROUP_BY() Y PONDERACIÓN -------------------------------------------------
ocupados <- sub_ene |>
  filter(situacion == "Ocupado")

tabla_genero <- ocupados |>
  group_by(sexo_txt) |>
  summarise(
    n_muestra                     = n(),
    pob_expandida                 = sum(fact_cal),
    media_hrs_semanales           = round(mean(efectivas, na.rm = TRUE), 2),
    media_hrs_semanales_ponderada = round(sum(efectivas * fact_cal, na.rm = TRUE) / 
                                            sum(fact_cal[!is.na(efectivas)]), 2)
  )

tabla_genero


# 7. GUARDAR ARCHIVO ----------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE)
write.csv(sub_ene, "data/processed/ene_a3.csv", row.names = FALSE)


# 8. INTERPRETACIÓN ECONÓMICA Y BITÁCORA
# Los hombres ocupados promedian 38,6 horas efectivas semanales, mientras que 
# las mujeres promedian 33,6 horas, dando como resultado que los hombres trabajan
# 5 horas mas que las mujeres en la semana.

# LIMITACIÓN: La variable "efectivas" no distingue si esta menor cantidad de horas 
# en las mujeres responde a jornadas parciales voluntarias, subempleo, o otra razon.

# BITÁCORA DE LIMPIEZA — complétala. Sin esto, el trabajo no es reproducible.
# 1. Lectura: usé read.csv2() porque el archivo trae separador ";" y
#    decimales con ",".
# 2. Variables conservadas: sexo, activ, fact_cal y efectivas (de 222).
# 3. Recodifiqué: sexo y activ a texto, con case_when().
# 4. Faltantes estructurales encontrados: 56.096 filas en `efectivas`, que son
#    inactivos, desocupados y niños. Decisión: No se usó na.omit() para no borrar al 57% del país.
# 5. Centinelas encontrados: 888 y 999 en `efectivas`.
#    Decisión: Convertidos a NA real con na_if(). Efecto en el promedio: bajó de 37.7 a 35.4 h.
# 6. Outliers restantes (hasta 140 h): decisión: Se mantuvieron (pueden ser turnos extremos).
# 7. Ponderación: usé fact_cal porque permite proyectar la muestra a los habitantes reales de Chile.
# 8. Resultado principal: brecha efectiva = 5 horas a favor de los hombres.