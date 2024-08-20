library(dplyr)
library(targets)
library(kableExtra)
library(arrow)
library(janitor)
library(stringr)
library(ggplot2)
library(readr)
library(arrow)

# Tabla 1 -----------------------------------------------------------------

fonasa <- as_arrow_table(tar_read(fonasa_duplicated_table))

tbl1 <- fonasa %>% 
  count(caso) %>% 
  arrange(caso) %>% 
  collect() %>% 
  mutate(porcentaje = n/sum(n)*100) %>% 
  adorn_totals() %>%
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename(Casos = caso,
         '%' = porcentaje)

rm(fonasa)

write_parquet(tbl1, "presentations/presentacion-rep-demografia/tablas/tbl1.parquet")

# Tabla 2 -----------------------------------------------------------------

suseso <- as_arrow_table(tar_read(suseso_duplicated_table ))

tbl2 <- suseso %>% 
  count(caso) %>% 
  arrange(caso) %>% 
  collect() %>% 
  mutate(porcentaje = n/sum(n)*100) %>% 
  adorn_totals() %>%
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename(Casos = caso,
         '%' = porcentaje)

rm(suseso)

write_parquet(tbl2, "presentations/presentacion-rep-demografia/tablas/tbl2.parquet")

# Tabla 3 -----------------------------------------------------------------

fonasa <- as_arrow_table(tar_read(joined_life_signal_fonasa_table))
suseso <- as_arrow_table(tar_read(joined_life_signal_suseso_table))

aux1 <- fonasa %>% 
  count(senal_fonasa_esc_1) %>% 
  collect() %>% 
  mutate(porcentaje = n/sum(n)*100) %>% 
  adorn_totals() %>%
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('Señal de vida' = senal_fonasa_esc_1,
         FONASA = n,
         '% FONASA' = porcentaje) 

aux2 <- suseso %>% 
  count(senal_suseso_esc_1) %>% 
  collect() %>% 
  mutate(porcentaje = n/sum(n)*100) %>%
  adorn_totals() %>%
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('Señal de vida' = senal_suseso_esc_1,
         SUSESO = n,
         '% SUSESO' = porcentaje)

tbl3 <- aux1 %>% left_join(aux2)

rm(fonasa, suseso)

write_parquet(tbl3, "presentations/presentacion-rep-demografia/tablas/tbl3.parquet")

# Tabla 4 -----------------------------------------------------------------

base <- as_arrow_table(tar_read(demographic_table))

aux1 <- base %>% 
  count(match_fonasa) %>% 
  collect() %>% 
  arrange(-match_fonasa) %>% 
  filter(!is.na(match_fonasa)) %>% 
  rename('Match' = match_fonasa,
         FONASA = n) %>% 
  mutate(porcentaje_match_fonasa = FONASA/sum(FONASA)*100) %>%  
  adorn_totals() %>% 
  mutate(FONASA = formatC(FONASA, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje_match_fonasa = formatC(porcentaje_match_fonasa, format = "f", big.mark = ".", decimal.mark = ",", digits = 1))

aux2 <- base %>% 
  count(match_suseso) %>% 
  collect() %>% 
  arrange(-match_suseso) %>% 
  filter(!is.na(match_suseso)) %>% 
  rename('Match' = match_suseso,
         SUSESO = n) %>% 
  mutate(porcentaje_match_suseso = SUSESO/sum(SUSESO)*100) %>%  
  adorn_totals() %>% 
  mutate(SUSESO = formatC(SUSESO, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje_match_suseso = formatC(porcentaje_match_suseso, format = "f", big.mark = ".", decimal.mark = ",", digits = 1))

tbl4 <- aux1 %>% left_join(aux2) %>% 
  rename('% match SUSESO' = porcentaje_match_suseso,
         '% match FONASA' = porcentaje_match_fonasa) 

rm(base)

write_parquet(tbl4, "presentations/presentacion-rep-demografia/tablas/tbl4.parquet")


# Tabla 5 -----------------------------------------------------------------

base <- as_arrow_table(tar_read(validated_run_table))

tbl5 <- base %>% 
  count(validacion_rut) %>% 
  collect() %>% 
  mutate(porcentaje = n/sum(n)*100) %>% 
  adorn_totals() %>% 
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename(validacion_run = validacion_rut,
         '%' = porcentaje)

rm(base)

write_parquet(tbl5, "presentations/presentacion-rep-demografia/tablas/tbl5.parquet")


# Tabla 6 -----------------------------------------------------------------

base <- as_arrow_table(tar_read(combined_age_table))

tbl6 <- base %>% 
  filter(edad_combinada!=99999) %>% 
  collect() %>%
  select(edad_combinada) %>%
  summary(edad_combinada) %>% 
  as.data.frame() %>% 
  select(Freq) %>% 
  rename('Estadísticas descriptivas de edad'=Freq)

rm(base)

write_parquet(tbl6, "presentations/presentacion-rep-demografia/tablas/tbl6.parquet")

# Tabla 7 -----------------------------------------------------------------

base <- as_arrow_table(tar_read(state_table))

tbl7 <- base %>% 
  count(estado) %>% 
  collect() %>% 
  mutate(porcentaje = n/sum(n)*100,
         estado = case_when(estado == "vivo" ~ "1. Vivo",
                            estado == "muerto" ~ "2. Muerto",
                            estado == "no match" ~ "3. Incierto")) %>% 
  arrange(estado) %>% 
  adorn_totals() %>% 
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('%' = porcentaje,
         estado_rc = estado)

write_parquet(tbl7, "presentations/presentacion-rep-demografia/tablas/tbl7.parquet")

# Tabla 8 -----------------------------------------------------------------

tbl8 <- base %>% 
  count(estado_2) %>% 
  collect() %>% 
  mutate(porcentaje = n/sum(n)*100,
         estado_2 = case_when(estado_2 == 1 ~ "1. Vivo",
                              estado_2 == 2 ~ "2. Muerto",
                              estado_2 == 3 ~ "3. Incierto")) %>% 
  adorn_totals() %>% 
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('%' = porcentaje,
         estado = estado_2)

rm(base)

write_parquet(tbl8, "presentations/presentacion-rep-demografia/tablas/tbl8.parquet")


# Tabla 9 -----------------------------------------------------------------

base <- as_arrow_table(tar_read(residency_status_table))

tbl9 <- base %>% 
  count(condicion_residencia) %>% 
  collect() %>% 
  mutate(porcentaje = n/sum(n)*100,
         condicion_residencia = case_when(condicion_residencia == 1 ~ "1. Activo",
                                          condicion_residencia == 2 ~ "2. Inactivo",
                                          condicion_residencia == 3 ~ "3. Sin señal")) %>% 
  adorn_totals() %>% 
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('%' = porcentaje)

rm(base)

write_parquet(tbl9, "presentations/presentacion-rep-demografia/tablas/tbl9.parquet")


# Tabla 10 ----------------------------------------------------------------

base <- read_parquet(tar_read(homologated_variables_table), as_data_frame = FALSE)

tbl10 <- base %>% 
  count(estado_2, condicion_residencia) %>% 
  collect() %>% 
  arrange(estado_2, condicion_residencia) %>% 
  mutate(porcentaje = n/sum(n)*100,
         estado_2 = case_when(estado_2 == 1 ~ "1. Vivo",
                              estado_2 == 2 ~ "2. Muerto",
                              estado_2 == 3 ~ "3. Incierto"),
         condicion_residencia = case_when(condicion_residencia == 1 ~ "1. Activo",
                                          condicion_residencia == 2 ~ "2. Inactivo",
                                          condicion_residencia == 3 ~ "3. Sin señal")) %>% 
  adorn_totals() %>% 
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('%' = porcentaje,
         estado = estado_2) 

write_parquet(tbl10, "presentations/presentacion-rep-demografia/tablas/tbl10.parquet")

# Tabla 11 ----------------------------------------------------------------

tbl11 <- base %>% 
  filter(estado_2 == 1 & condicion_residencia == 1) %>% 
  count(sexo) %>% 
  collect() %>% 
  arrange(sexo) %>% 
  mutate(porcentaje = n/sum(n)*100,
         sexo = case_when(sexo == 1 ~ "1. Hombre",
                          sexo == 2 ~ "2. Mujer",
                          sexo == 3 ~ "3. Indeterminado",
                          sexo == 4 ~ "4. No binario",
                          sexo == 99 ~ "99. Sin información")) %>% 
  adorn_totals() %>% 
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('%' = porcentaje)

write_parquet(tbl11, "presentations/presentacion-rep-demografia/tablas/tbl11.parquet")

# Tabla 12 ----------------------------------------------------------------

tbl12 <- base %>% 
  filter(estado_2 == 1 & condicion_residencia %in% c(1,3)) %>% 
  count(sexo) %>% 
  collect() %>% 
  arrange(sexo) %>% 
  mutate(porcentaje = n/sum(n)*100,
         sexo = case_when(sexo == 1 ~ "1. Hombre",
                          sexo == 2 ~ "2. Mujer",
                          sexo == 3 ~ "3. Indeterminado",
                          sexo == 4 ~ "4. No binario",
                          sexo == 99 ~ "99. Sin información")) %>% 
  adorn_totals() %>% 
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('%' = porcentaje)

write_parquet(tbl12, "presentations/presentacion-rep-demografia/tablas/tbl12.parquet")

# Tabla 13 ----------------------------------------------------------------

tbl13 <- base %>% 
  filter(estado_2 == 1 & condicion_residencia == 1) %>% 
  count(nacionalidad) %>% 
  collect() %>% 
  arrange(nacionalidad) %>% 
  mutate(porcentaje = n/sum(n)*100,
         nacionalidad = case_when(nacionalidad == 1 ~ "1. Chileno",
                                  nacionalidad == 2 ~ "2. Extranjero",
                                  nacionalidad == 3 ~ "3. Nacionalizado",
                                  nacionalidad == 99 ~ "99. Sin información")) %>% 
  adorn_totals() %>% 
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('%' = porcentaje)

write_parquet(tbl13, "presentations/presentacion-rep-demografia/tablas/tbl13.parquet")

# Tabla 14 ----------------------------------------------------------------

tbl14 <- base %>% 
  filter(estado_2 == 1 & condicion_residencia %in% c(1,3)) %>% 
  count(nacionalidad) %>% 
  collect() %>% 
  arrange(nacionalidad) %>% 
  mutate(porcentaje = n/sum(n)*100,
         nacionalidad = case_when(nacionalidad == 1 ~ "1. Chileno",
                                  nacionalidad == 2 ~ "2. Extranjero",
                                  nacionalidad == 3 ~ "3. Nacionalizado",
                                  nacionalidad == 99 ~ "99. Sin información")) %>% 
  adorn_totals() %>% 
  mutate(n = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
         porcentaje = formatC(porcentaje, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)) %>% 
  rename('%' = porcentaje)

write_parquet(tbl14, "presentations/presentacion-rep-demografia/tablas/tbl14.parquet")

# Tabla 15 ----------------------------------------------------------------

edad_frq <- base %>% 
  filter(estado_2 == 1 & condicion_residencia %in% c(1,3)) %>% 
  count(edad_combinada) %>% 
  collect()

casos <- base %>% 
  filter(estado_2 == 1 & condicion_residencia %in% c(1,3) & edad_combinada!=99999 & edad_combinada > 110) %>% 
  collect()

tbl15 <- base %>% 
  filter(estado_2 == 1 & condicion_residencia == 1) %>% 
  mutate(edad_agrupada = case_when(edad_combinada %in% 0:9 ~ "0 a 9",
                                   edad_combinada %in% 10:19 ~ "10 a 19",
                                   edad_combinada %in% 20:29 ~ "20 a 29",
                                   edad_combinada %in% 30:39 ~ "30 a 39",
                                   edad_combinada %in% 40:49 ~ "40 a 49",
                                   edad_combinada %in% 50:59 ~ "50 a 59",
                                   edad_combinada %in% 60:69 ~ "60 a 69",
                                   edad_combinada %in% 70:79 ~ "70 a 79",
                                   edad_combinada %in% 80:89 ~ "80 a 89",
                                   edad_combinada %in% 90:99 ~ "90 a 99",
                                   edad_combinada %in% 100:110 ~ "100 a 110",
                                   edad_combinada == 99999 ~ "Sin información",
                                   TRUE ~ ">110"),
         edad_num = case_when(edad_combinada %in% 0:9 ~ 1,
                              edad_combinada %in% 10:19 ~ 2,
                              edad_combinada %in% 20:29 ~ 3,
                              edad_combinada %in% 30:39 ~ 4,
                              edad_combinada %in% 40:49 ~ 5,
                              edad_combinada %in% 50:59 ~ 6,
                              edad_combinada %in% 60:69 ~ 7,
                              edad_combinada %in% 70:79 ~ 8,
                              edad_combinada %in% 80:89 ~ 9,
                              edad_combinada %in% 90:99 ~ 10,
                              edad_combinada %in% 100:110 ~ 11,
                              edad_combinada == 99999 ~ 12,
                              TRUE ~ 13)) %>% 
  group_by(edad_agrupada, edad_num) %>%
  summarise(n = n()) %>%
  collect() %>% 
  ungroup() %>% 
  mutate(Porcentaje = round(n/sum(n)*100,1)) %>% 
  adorn_totals("row") %>% 
  mutate(Frecuencia = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0)) %>% 
  arrange(edad_num) %>% 
  select(-edad_num) %>% 
  rename(Edad = edad_agrupada,
         'Porcentaje (%)' = Porcentaje) %>% 
  select(Edad, Frecuencia, 'Porcentaje (%)')

write_parquet(tbl15, "presentations/presentacion-rep-demografia/tablas/tbl15.parquet")

# Tabla 16 ----------------------------------------------------------------

tbl16 <- base %>% 
  filter(estado_2 == 1 & condicion_residencia %in% c(1,3)) %>% 
  mutate(edad_agrupada = case_when(edad_combinada %in% 0:9 ~ "0 a 9",
                                   edad_combinada %in% 10:19 ~ "10 a 19",
                                   edad_combinada %in% 20:29 ~ "20 a 29",
                                   edad_combinada %in% 30:39 ~ "30 a 39",
                                   edad_combinada %in% 40:49 ~ "40 a 49",
                                   edad_combinada %in% 50:59 ~ "50 a 59",
                                   edad_combinada %in% 60:69 ~ "60 a 69",
                                   edad_combinada %in% 70:79 ~ "70 a 79",
                                   edad_combinada %in% 80:89 ~ "80 a 89",
                                   edad_combinada %in% 90:99 ~ "90 a 99",
                                   edad_combinada %in% 100:110 ~ "100 a 110",
                                   edad_combinada == 99999 ~ "Sin información",
                                   TRUE ~ ">110"),
         edad_num = case_when(edad_combinada %in% 0:9 ~ 1,
                              edad_combinada %in% 10:19 ~ 2,
                              edad_combinada %in% 20:29 ~ 3,
                              edad_combinada %in% 30:39 ~ 4,
                              edad_combinada %in% 40:49 ~ 5,
                              edad_combinada %in% 50:59 ~ 6,
                              edad_combinada %in% 60:69 ~ 7,
                              edad_combinada %in% 70:79 ~ 8,
                              edad_combinada %in% 80:89 ~ 9,
                              edad_combinada %in% 90:99 ~ 10,
                              edad_combinada %in% 100:110 ~ 11,
                              edad_combinada == 99999 ~ 12,
                              TRUE ~ 13)) %>% 
  group_by(edad_agrupada, edad_num) %>%
  summarise(n = n()) %>%
  collect() %>% 
  ungroup() %>% 
  mutate(Porcentaje = round(n/sum(n)*100,1)) %>% 
  adorn_totals("row") %>% 
  mutate(Frecuencia = formatC(n, format = "f", big.mark = ".", decimal.mark = ",", digits = 0)) %>% 
  arrange(edad_num) %>% 
  select(-edad_num) %>% 
  rename(Edad = edad_agrupada,
         'Porcentaje (%)' = Porcentaje) %>% 
  select(Edad, Frecuencia, 'Porcentaje (%)')

write_parquet(tbl16, "presentations/presentacion-rep-demografia/tablas/tbl16.parquet")




