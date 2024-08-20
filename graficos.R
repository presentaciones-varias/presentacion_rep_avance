library(dplyr)
library(targets)
library(kableExtra)
library(arrow)
library(janitor)
library(stringr)
library(ggplot2)
library(readr)
library(arrow)
library(readxl)
options(scipen = 999)

base <- read_parquet(tar_read(homologated_variables_table), as_data_frame = FALSE)

df_1 <- base %>% 
  filter(estado_2==1 & condicion_residencia==1) %>% 
  count(edad_combinada) %>% 
  filter(edad_combinada!=9999 & edad_combinada <= 99) %>% 
  collect() %>% 
  mutate(tipo = "combinada",
         total = sum(n),
         porcentaje = n/total*100) %>% 
  rename(edad = edad_combinada)

df_2 <- base %>% 
  filter(estado_2==1 & condicion_residencia %in% c(1,3)) %>% 
  count(edad_combinada) %>% 
  filter(edad_combinada!=9999 & edad_combinada <= 99) %>% 
  collect() %>% 
  mutate(tipo = "combinada",
         total = sum(n),
         porcentaje = n/total*100) %>% 
  rename(edad = edad_combinada)

proyecciones <- read_excel("proyecciones.xlsx", sheet = "Hoja3") %>% 
  mutate(tipo = "proyecciones",
         total = sum(n),
         porcentaje = n/total*100)

df_1 %>% 
  rbind(proyecciones) %>% 
  ggplot() +
  aes(x = edad, y = porcentaje, group = tipo, fill = tipo, color = tipo) +
  geom_area(position = "identity", alpha = 0.5) +
  theme_minimal()

ggsave("presentations/presentacion-rep-demografia/imagenes/edad_plot_1.png")

df_2 %>% 
  rbind(proyecciones) %>% 
  ggplot() +
  aes(x = edad, y = porcentaje, group = tipo, fill = tipo, color = tipo) +
  geom_area(position = "identity", alpha = 0.5) +
  theme_minimal()

ggsave("presentations/presentacion-rep-demografia/imagenes/edad_plot_2.png")


df <- base %>% 
  filter(estado_2 == 1 & condicion_residencia == 1 & sexo %in% 1:2 & edad_combinada <= 99) %>% 
  count(edad_combinada, sexo) %>%
  collect() %>% 
  group_by(sexo) %>% 
  mutate(total = sum(n),
         porcentaje = n/total*100,
         porcentaje = ifelse(sexo==2, porcentaje*-1, porcentaje)) %>% 
  mutate(sexo = if_else(sexo == 1, "Hombres", "Mujeres"))

proyecciones <- read_excel("proyecciones.xlsx", sheet = "Hoja4") %>% 
  group_by(sexo) %>% 
  mutate(tipo = "proyecciones",
         total = sum(n),
         porcentaje = n/total*100,
         porcentaje = ifelse(sexo==2, porcentaje*-1, porcentaje)) %>% 
  ungroup() %>% 
  mutate(sexo = if_else(sexo == 1, "Hombres", "Mujeres"))

ggplot()+  # default x-axis is age in years;
  geom_col(
    data = df,
    mapping = aes(
      x = edad_combinada,
      y = porcentaje,
      fill = as.factor(sexo)),
    alpha = 0.2,                                    # more transparent
    width = 1) +
  geom_col(
    data = proyecciones, 
    mapping = aes(
      x = edad,                               # age categories as original X axis
      y = porcentaje,                                # % as original Y-axis
      fill = as.factor(sexo)),                             # fill of bars by gender
    alpha = 1,                                      # not transparent 
    width = 0.3) +
  coord_flip() +
  scale_y_continuous(limits = c(-2,2),
                     breaks = seq(-2, 2, by = 0.5)) +
  theme_minimal()

ggsave("presentations/presentacion-rep-demografia/imagenes/edad_sexo_plot_1.png")

df <- base %>% 
  filter(estado_2 == 1 & condicion_residencia %in% c(1,3) & sexo %in% 1:2 & edad_combinada <= 99) %>% 
  count(edad_combinada, sexo) %>%
  collect() %>% 
  group_by(sexo) %>% 
  mutate(total = sum(n),
         porcentaje = n/total*100,
         porcentaje = ifelse(sexo==2, porcentaje*-1, porcentaje)) %>% 
  mutate(sexo = if_else(sexo == 1, "Hombres", "Mujeres"))


ggplot()+  # default x-axis is age in years;
  geom_col(
    data = df,
    mapping = aes(
      x = edad_combinada,
      y = porcentaje,
      fill = as.factor(sexo)),
    alpha = 0.2,                                    # more transparent
    width = 1) +
  geom_col(
    data = proyecciones, 
    mapping = aes(
      x = edad,                               # age categories as original X axis
      y = porcentaje,                                # % as original Y-axis
      fill = as.factor(sexo)),                             # fill of bars by gender
    alpha = 1,                                      # not transparent 
    width = 0.3) +
  coord_flip() +
  scale_y_continuous(limits = c(-2,2),
                     breaks = seq(-2, 2, by = 0.5)) +
  theme_minimal()

ggsave("presentations/presentacion-rep-demografia/imagenes/edad_sexo_plot_2.png")

df <- base %>% 
  filter(estado_2 == 1 & condicion_residencia == 1 & sexo %in% 1:2 & edad_combinada <= 99) %>% 
  count(edad_combinada, sexo) %>%
  collect() %>% 
  group_by(sexo) %>% 
  mutate(total = sum(n),
         porcentaje = n/total*100,
         porcentaje = ifelse(sexo==2, porcentaje*-1, porcentaje),
         n = ifelse(sexo==2, n*-1, n)) %>% 
  mutate(sexo = if_else(sexo == 1, "Hombres", "Mujeres"))

proyecciones <- read_excel("proyecciones.xlsx", sheet = "Hoja4") %>% 
  group_by(sexo) %>% 
  mutate(tipo = "proyecciones",
         total = sum(n),
         porcentaje = n/total*100,
         porcentaje = ifelse(sexo==2, porcentaje*-1, porcentaje),
         n = ifelse(sexo==2, n*-1, n)) %>% 
  ungroup() %>% 
  mutate(sexo = if_else(sexo == 1, "Hombres", "Mujeres"))

ggplot()+  # default x-axis is age in years;
  geom_col(
    data = df,
    mapping = aes(
      x = edad_combinada,
      y = n,
      fill = as.factor(sexo)),
    alpha = 0.2,                                    # more transparent
    width = 1) +
  geom_col(
    data = proyecciones, 
    mapping = aes(
      x = edad,                               # age categories as original X axis
      y = n,                                # % as original Y-axis
      fill = as.factor(sexo)),                             # fill of bars by gender
    alpha = 1,                                      # not transparent 
    width = 0.3) +
  coord_flip() +
  scale_y_continuous(limits = c(-200000,200000),
                     breaks = seq(-200000, 200000, by = 50000)) +
  theme_minimal()

ggsave("presentations/presentacion-rep-demografia/imagenes/edad_sexo_plot_1_freq.png")

df <- base %>% 
  filter(estado_2 == 1 & condicion_residencia %in% c(1,3) & sexo %in% 1:2 & edad_combinada <= 99) %>% 
  count(edad_combinada, sexo) %>%
  collect() %>% 
  group_by(sexo) %>% 
  mutate(total = sum(n),
         porcentaje = n/total*100,
         porcentaje = ifelse(sexo==2, porcentaje*-1, porcentaje),
         n = ifelse(sexo==2, n*-1, n)) %>% 
  mutate(sexo = if_else(sexo == 1, "Hombres", "Mujeres"))

ggplot()+  # default x-axis is age in years;
  geom_col(
    data = df,
    mapping = aes(
      x = edad_combinada,
      y = n,
      fill = as.factor(sexo)),
    alpha = 0.2,                                    # more transparent
    width = 1) +
  geom_col(
    data = proyecciones, 
    mapping = aes(
      x = edad,                               # age categories as original X axis
      y = n,                                # % as original Y-axis
      fill = as.factor(sexo)),                             # fill of bars by gender
    alpha = 1,                                      # not transparent 
    width = 0.3) +
  coord_flip() +
  scale_y_continuous(limits = c(-200000,200000),
                     breaks = seq(-200000, 200000, by = 50000)) +
  theme_minimal()

ggsave("presentations/presentacion-rep-demografia/imagenes/edad_sexo_plot_2_freq.png")


