

library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)
library(data.table)
library(writexl)
library(stringi)

#Son muchos! como lo hacemos más limpio? 
#Pacman!

library(pacman)

pacman::p_load(tidyverse, janitor, readxl, openxlsx, data.table,writexl, stringi)




#Cargamos base----

df_ags <- read_xlsx("01_Inputs/Sesion 2/BBJ_AGS_IV_2024.xlsx") %>% 
  clean_names()

head(df_ags)
glimpse(df_ags)

unique(df_ags$beca)

#Paso 1: Operaciones Strings Básicas----

df_ags_nombres_clean <- df_ags %>% 
  mutate(nombre_completo = paste(nombre, apellido_paterno, apellido_materno, sep = " "), #Concatenado básico
         nombre_completo = str_to_title(nombre_completo), # Capitalización en primera letra
         nombre_completo_sin_acentos = stri_trans_general(nombre_completo, "Latin-ASCII")) #Remover acentos

#Revisamos casos especiales
df_ags_nombres_clean$nombre_completo[1:3]

#Recodificamos con corrección (eliminar mayúsculas en "De" y "Los")


df_ags_nombres_clean <- df_ags %>% 
  mutate(nombre_completo = paste(nombre, apellido_paterno, apellido_materno, sep = " "),
         nombre_completo = str_to_title(nombre_completo),
         nombre_completo = str_replace_all(nombre_completo, 
                                           c("De" = "de",
                                             "Los" = "los")),
         nombre_completo_sin_acentos = stri_trans_general(nombre_completo, "Latin-ASCII")) %>% 
  select(-c(nombre, apellido_paterno, apellido_materno, nombre_completo)) %>% 
  select(nombre_completo_sin_acentos, everything())


#Paso 2: Concatenado de claves ----

#Las claves están separadas: Objetivo. Crear una clave unica de localidad con el formato INEGI y de ahi derivar edo y mun
View(df_ags_nombres_clean)

df_claves <- df_ags_nombres_clean %>% 
  mutate(clave_localidad = paste0(cve_edo, cve_mun, cve_loc))

#Vusualizamos los valores de la col creada
unique(df_claves$clave_localidad)

#No parece seguir el formato INEGI
#Objetivo: Aplicar padding a todas las claves para crear la clave de localidad a 9 dígitos como en INEGI

df_claves <- df_ags_nombres_clean %>% 
  mutate(cve_edo = str_pad(cve_edo, side = "left", width = 2, pad = "0"),
         cve_mun = str_pad(cve_mun, side = "left", width = 3, pad = "0"),
         cve_loc = str_pad(cve_loc, side = "left", width = 4, pad = "0"),
         clave_localidad = paste0(cve_edo, cve_mun, cve_loc),
         clave_inegi = substr(clave_localidad, 1,5))

#Visualizamos los valores de la col creada
unique(df_claves$clave_localidad)
unique(df_claves$clave_inegi)




