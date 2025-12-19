library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)

#Cargar base

df <- fread("Inputs/Apoyo_a_comerciantes_de_alimentos_CDMX_2021.csv",
         encoding = "UTF-8")

#Generar base completa con campos para análisis

df_limpia <- df %>% 
  mutate(monto = 3000) %>% 
  filter(status == "CON DISPERSIÓN")

