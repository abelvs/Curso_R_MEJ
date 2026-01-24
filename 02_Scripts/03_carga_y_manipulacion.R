############################################
# Tidyverse: Cargar y manipular bases
############################################

library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)


############################################
# 1. Cargar base de datos
############################################

df <- read_excel("01_Inputs/Remuneraciones_base_alcaldias_CDMX.xlsx")

############################################
# 2. Exploración inicial
############################################

head(df)
glimpse(df)
summary(df)

unique(df$n_cabeza_sector)
table(df$n_cabeza_sector)


############################################
# 3. filter(): seleccionar filas
############################################

df %>% 
  filter(n_cabeza_sector == "ALCALDIA DE BENITO JUAREZ")

df %>% 
  filter(sexo == "FEMENINO")

# Guardar un objeto filtrado
df_benito_juarez <- df %>% 
  filter(n_cabeza_sector == "ALCALDIA DE BENITO JUAREZ")

df_alcaldias_prd <- df %>% 
  filter(n_cabeza_sector %in% c("ALCALDIA DE CUAUHTEMOC", "ALCALDIA DE TLALPAN"))


############################################
# 3. TRANSFORMACION mutate(): crear nuevas columnas
############################################

# Filtrar por condiciones combinadas
df %>% 
  filter(apellido_1 == "GUTIERREZ")

df %>% 
  filter(apellido_1 == "GUTIERREZ" & apellido_2 == "AGUILAR")

df %>% 
  filter(nombre == "JOSE GIOVANI", apellido_1 == "GUTIERREZ" & apellido_2 == "AGUILAR")


# Crear nombre completo
df_nombres <- df %>% 
  mutate(nombre_completo = paste(nombre, apellido_1, apellido_2, sep = " "))

df_nombres %>% 
  filter(nombre_completo == "JOSE GIOVANI GUTIERREZ AGUILAR")

df_nombres_clean <- df_nombres %>% 
  mutate(nombre_clean = str_to_title(nombre_completo))


# Columna condicional simple
#Servidores con sueldo mayor a 20,000 brutos


df_salarios <- df_nombres_clean %>% 
  mutate(sueldo_superior_20 = ifelse(sueldo_tabular_bruto >= 20000, 1, 0)) 


df_salarios %>% 
  count(sueldo_superior_20)


# Columna condicional múltiple (case_when)

df_rangos_sueldos <- df_salarios %>% 
  mutate(rango_salarial = case_when(sueldo_tabular_bruto < 10000 ~ "< 10,000",
                                    sueldo_tabular_bruto < 20000 ~ "10,000 a 20,000",
                                    sueldo_tabular_bruto < 50000 ~ "20,000 a 50,000",
                                    sueldo_tabular_bruto < 80000 ~ "50,000 a 80,000",
                                    sueldo_tabular_bruto < 100000 ~ "80,000 a 100,000",
                                    sueldo_tabular_bruto > 100000 ~ "> 100,000"))


hist(df$sueldo_tabular_bruto) #Ojo, ¿se entienden los valores?
options(scipen = 999) #Desactiva notación científica
hist(df$sueldo_tabular_bruto)

############################################
# 4. COLAPSADO group_by() + summarise()
############################################


df_rangos_sueldos %>% 
  group_by(n_cabeza_sector)

df_rangos_sueldos %>% 
  group_by(n_cabeza_sector) %>% 
  summarise(sueldo_bruto_promedio = mean(sueldo_tabular_bruto))

df_prom_alcaldia <- df_rangos_sueldos %>% 
  group_by(n_cabeza_sector) %>% 
  summarise(sueldo_bruto_promedio = mean(sueldo_tabular_bruto))


############################################
# 5. Formateo
############################################

#Se puede ver más limpio?
df_prom_alcaldia_clean <- df_prom_alcaldia %>% 
  rename(alcaldia = n_cabeza_sector) %>% 
  mutate(alcaldia = str_remove(alcaldia, "ALCALDIA DE "),
         alcaldia = str_to_title(alcaldia),
         alcaldia = str_replace(alcaldia, " De ", " de ")) %>% 
  mutate(sueldo_bruto_promedio = round(sueldo_bruto_promedio, digits = 0),
         sueldo_bruto_promedio = dollar(sueldo_bruto_promedio)) #Ojo: scales cambia el tipo a STR

  
############################################
# 6. Exportado
############################################

glimpse(df_rangos_sueldos)

#Generamos columnas faltantes y seleccionamos (ordenamos) las de interés
df_clean <- df_rangos_sueldos %>% 
  rename(alcaldia = n_cabeza_sector) %>% 
  mutate(alcaldia = str_remove(alcaldia, "ALCALDIA DE "),
         alcaldia = str_to_title(alcaldia),
         alcaldia = str_replace(alcaldia, " De ", " de ")) %>% 
  select(nombre_clean, sexo, edad, n_puesto, alcaldia, sueldo_tabular_bruto, sueldo_tabular_neto,
         rango_salarial, sueldo_superior_20)


write.xlsx(df_clean, "Outputs/Remuneraciones_base_alcaldias_CDMX_clean.xlsx")
write.xlsx(df_prom_alcaldia_clean, "Outputs/Sueldo_por_alcaldia.xlsx")





