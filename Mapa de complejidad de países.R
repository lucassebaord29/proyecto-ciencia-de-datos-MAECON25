rm (list = ls()) # Limpiar el entorno de trabajo
# Cargar librerias

library(tidyverse)
library(economiccomplexity)
library(igraph)
library(ggplot2)
library(beepr)
library(readxl)
library(gridExtra)
library(sf)
library(dplyr)
library(paletteer)
# setear directorio

setwd('/Users/lucasordonez/Library/CloudStorage/Dropbox/Proyecto datos')

# Importar base de datos principal con información de exportaciones y atributos de productos
base_final <- read_csv("Data/Primary/base_final.csv")



## Calcular el índice de Balassa (RCA) ----------
# Mide si un país tiene ventaja comparativa revelada en un producto dado
bi_2223 <- balassa_index(base_final,
                         country = "country_iso3_code", 
                         product = "product", 
                         value = "export_value_avg")
## Calcular matriz de proximidad entre productos ----------
# Basado en la co-exportación de productos (probabilidad condicional de ventaja compartida)
prox_2223 <- proximity(bi_2223)


# Indices de complejidad (reflections)
com_ref_1819 <- complexity_measures(bi_2223, method = "reflections")
country_com_ref_1819 <- com_ref_1819$complexity_index_country 

# Convertir el resultado a un data frame
country_com_ref_1819 <-  data.frame(
  country = names(country_com_ref_1819),
  reflections_index = as.numeric(country_com_ref_1819))




# Indices de complejidad (fitness) 
com_fit_1819 <- complexity_measures(bi_2223, method = "fitness")
country_com_fit_1819 <- com_fit_1819$complexity_index_country

# Convertir el resultado a un data frame
country_com_fit_1819 <-  data.frame(
  country = names(country_com_fit_1819),
  fitness_index = as.numeric(country_com_fit_1819))


# Indices de complejidad (eigenvalues)

com_eig_1819 <- complexity_measures(bi_2223, method = "eigenvalues")
country_com_eig_1819 <- com_eig_1819$complexity_index_country

# Convertir el resultado a un data frame
country_com_eig_1819 <-  data.frame(
  country = names(country_com_eig_1819),
  eigenvalues_index = as.numeric(country_com_eig_1819))


# Unir los data frames de complejidad

country_complexity_indexes <- country_com_ref_1819 %>%
  left_join(country_com_fit_1819, by = "country") %>%
  left_join(country_com_eig_1819, by = "country") %>%
  
  # 2. Estandarizar las columnas de complejidad
  mutate(across(
    .cols = c("reflections_index", "fitness_index", "eigenvalues_index"),
    .fns = ~ as.numeric(scale(.)),
    .names = "{.col}_z"
  ))

# exportar el data frame de complejidad a un archivo CSV

write_csv(country_complexity_indexes, "Data/Primary/country_complexity_indexes.csv")




# 1. Cargar geometría mundial
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. Verificamos que los nombres de país coincidan (usás códigos ISO3, así que está perfecto)
head(country_complexity_indexes$country)  # Deben ser códigos como "ARG", "USA", etc.
country_complexity_indexes$country
# 3. Unir los datos al mapa usando la columna 'iso_a3'
world_eci <- left_join(world, country_complexity_indexes , by = c("iso_a3_eh" = "country"  ))





# Definir la paleta personalizada de púrpuras oscuros
my_palette <- as.character(paletteer::paletteer_dynamic("cartography::blue.pal", 20))
                           
                           
# MAPA 1: Reflections
map_reflections <- ggplot(world_eci) +
  geom_sf(aes(fill = reflections_index_z), color = "white", size = 0.1) +
  scale_fill_gradientn(
    colours = my_palette,
    name = "Reflections method index (Standardized)", # 
    na.value = "grey90",
    limits = c(-3, 3),
    oob = scales::squish,
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)  # Título arriba, centrado
      ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(4, "cm"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold")
  ) 


# MAPA 2: Fitness
map_fitness <- ggplot(world_eci) +
  geom_sf(aes(fill = fitness_index_z), color = "white", size = 0.1) +
  scale_fill_gradientn(
    colours = my_palette,
    name = "Fitness method index (Standardized)",  # Título de la leyenda
    na.value = "grey90",
    limits = c(-3, 3),
    oob = scales::squish,
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)  # Título arriba, centrado
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(4, "cm"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold")
  )

