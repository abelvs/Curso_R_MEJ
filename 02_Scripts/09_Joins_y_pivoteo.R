pacman::p_load("dplyr", "data.table", "janitor",
               "readxl", "tidyr","writexl", "stringr", "scales")


#Cargamos bases

df <- read_excel("01_Inputs/Homicidios_2025.xlsx") %>% 
  clean_names 

#Catalogo inegi

municipios <- fread("01_Inputs/AGEEML_20251021629312.csv") %>% 
  clean_names() %>% 
  mutate(clave_inegi = str_pad(cvegeo, side = "left", pad = "0", width = 5)) %>% 
  mutate(pob_total = as.double(pob_total))


#Incorporamos información de municipios ()

base_mun <- df %>% 
  mutate(clave_inegi = str_pad(cve_municipio, side = "left", pad = "0", width = 5)) %>% 
  left_join(municipios, by = "clave_inegi") %>% 
  group_by(clave_inegi) %>% 
  summarise(nom_mun = first(nom_mun),
            pob_total = sum(pob_total, na.rm = T),
            homicidios_totales = sum(total_anual, na.rm = T)) 


base_edo <- base_mun %>% 
  mutate(cve_ent = substr(clave_inegi, 1,2)) %>% 
  group_by(cve_ent) %>% 
  summarise(pob_total = sum(pob_total, na.rm = T),
            homicidios_totales = sum(homicidios_totales, na.rm = T)) %>% 
  mutate(tasa = homicidios_totales/pob_total * 100000)
  
base_edo_modalidad <- df %>% 
  mutate(clave_inegi = str_pad(cve_municipio, side = "left", pad = "0", width = 5)) %>% 
  mutate(cve_edo = substr(clave_inegi, 1,2)) %>% 
  group_by(cve_edo, modalidad) %>% 
  summarise(homicidios_totales = sum(total_anual, na.rm = T)) %>%
  ungroup() %>% 
  pivot_wider(names_from = "modalidad",
              values_from = "homicidios_totales")
  

  