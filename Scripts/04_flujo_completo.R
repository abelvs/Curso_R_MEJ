############################################
# Juntándolo todo: Flujo de trabajo completo
############################################

library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)


############################################
# TAREA: Procesamiento de Remuneraciones
############################################
# Objetivo:
#   - Cargar la base de remuneraciones
#   - Generar una sola columna con el nombre completo de cada persona
#   - Crear variables de rango salarial (indicador sueldo bruto > 20,000 y rangos generales)
#   - Generar una tabla con el sueldo promedio por alcaldía
#     y formatearla lista para imprimir
############################################

path_archivo <- "Inputs/Remuneraciones_base_alcaldias_CDMX.xlsx"

df_clean <- read_excel(path_archivo) %>% 
  clean_names() %>% 
  mutate(nombre_completo = paste(nombre, apellido_1, apellido_2, sep = " ")) %>% 
  mutate(nombre_clean = str_to_title(nombre_completo)) %>% 
  mutate(sueldo_superior_20 = ifelse(sueldo_tabular_bruto >= 20000, 1, 0)) %>% 
  mutate(rango_salarial = case_when(sueldo_tabular_bruto <10000 ~ "< 10,000",
                                    sueldo_tabular_bruto < 20000 ~ "10,000 a 20,000",
                                    sueldo_tabular_bruto < 50000 ~ "20,000 a 50,000",
                                    sueldo_tabular_bruto < 80000 ~ "50,000 a 80,000",
                                    sueldo_tabular_bruto < 100000 ~ "80,000 a 100,000",
                                    sueldo_tabular_bruto > 100000 ~ "> 100,000")) %>% 
  mutate(alcaldia = str_remove(n_cabeza_sector, "ALCALDIA DE "),
         alcaldia = str_to_title(alcaldia),
         alcaldia = str_replace(alcaldia, " De ", " de ")) %>% 
  select(nombre_clean, sexo, edad, n_puesto, alcaldia, sueldo_tabular_bruto, sueldo_tabular_neto,
         rango_salarial, sueldo_superior_20)

tabla_alcaldias <- df_clean %>% 
  group_by(alcaldia) %>% 
  summarise(sueldo_bruto_promedio = mean(sueldo_tabular_bruto)) %>% 
  mutate(sueldo_bruto_promedio = round(sueldo_bruto_promedio, digits = 0),
         sueldo_bruto_promedio = dollar(sueldo_bruto_promedio))


write.xlsx(df_clean, "Outputs/Salida_pipeline/Remuneraciones_base_alcaldias_CDMX_clean.xlsx")
write.xlsx(tabla_alcaldias, "Outputs/Salida_pipeline/Sueldo_por_alcaldia.xlsx")


















#Ej. Demasiada condensacion----

df_clean <- read_excel(path_archivo) %>% 
  clean_names() %>% 
  mutate(nombre_clean = str_to_title(paste(nombre, apellido_1, apellido_2, sep = " ")),
         sueldo_superior_20 = ifelse(sueldo_tabular_bruto >= 20000, 1, 0),
         rango_salarial = case_when(
         sueldo_tabular_bruto < 10000  ~ "< 10,000",
         sueldo_tabular_bruto < 20000  ~ "10,000 a 20,000",
         sueldo_tabular_bruto < 50000  ~ "20,000 a 50,000",
         sueldo_tabular_bruto < 80000  ~ "50,000 a 80,000",
         sueldo_tabular_bruto < 100000 ~ "80,000 a 100,000",
         sueldo_tabular_bruto > 100000 ~ "> 100,000"),
         alcaldia = str_replace(str_to_title(str_remove(n_cabeza_sector, "ALCALDIA DE ")), " De ", " de ")) %>% 
  select(nombre_clean, sexo, edad, n_puesto, alcaldia, sueldo_tabular_bruto, sueldo_tabular_neto,
         rango_salarial, sueldo_superior_20)
  
  

