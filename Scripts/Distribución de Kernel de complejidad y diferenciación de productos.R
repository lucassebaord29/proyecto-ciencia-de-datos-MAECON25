rm (list = ls()) # Limpiar el entorno de trabajo
# Cargar librerias

library(tidyverse)
library(economiccomplexity)
library(igraph)
library(ggplot2)
library(beepr)
library(readxl)
library(gridExtra)
library(viridis)  # Para colores elegantes y perceptualmente uniformes

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
product_com_ref_1819 <- com_ref_1819$complexity_index_product 

# Convertir el resultado a un data frame
product_com_ref_1819 <-  data.frame(
  product = names(product_com_ref_1819),
  reflections_index = as.numeric(product_com_ref_1819))




# Indices de complejidad (fitness) 
com_fit_1819 <- complexity_measures(bi_2223, method = "fitness")
product_com_fit_1819 <- com_fit_1819$complexity_index_product

# Convertir el resultado a un data frame
product_com_fit_1819 <-  data.frame(
  product = names(product_com_fit_1819),
  fitness_index = as.numeric(product_com_fit_1819))


# Indices de complejidad (eigenvalues)

com_eig_1819 <- complexity_measures(bi_2223, method = "eigenvalues")
product_com_eig_1819 <- com_eig_1819$complexity_index_product

# Convertir el resultado a un data frame
product_com_eig_1819 <-  data.frame(
  product = names(product_com_eig_1819),
  eigenvalues_index = as.numeric(product_com_eig_1819))


# Unir los data frames de complejidad

product_complexity_indexes <- product_com_ref_1819 %>%
  left_join(product_com_fit_1819, by = "product") %>%
  left_join(product_com_eig_1819, by = "product") %>%
  
  # 2. Estandarizar las columnas de complejidad
  mutate(across(
    .cols = c("reflections_index", "fitness_index", "eigenvalues_index"),
    .fns = ~ as.numeric(scale(.)),
    .names = "{.col}_z"
  ))

# exportar el data frame de complejidad a un archivo CSV

write_csv(product_complexity_indexes, "Data/Primary/product_complexity_indexes.csv")

# inner_join para unir con la base de datos original

# Extraer atributos únicos por producto para agregar al grafo
atributos_productos <- base_final %>%
  select(product,
         categoria_rauch,                   # Clasificación de Rauch (w, r, d)
         extended_rauch_description,       # Descripción extendida de Rauch
         product_name,                     # Nombre completo del producto
         product_space_cluster_name,       # Clúster del espacio de productos
         technological_categories,         # Categoría tecnológica
         technological_categories_ab,      # Abreviatura de la categoría tecnológica
         sector_name,                      # Nombre del sector principal
         sector_name_short,
         product_space_x, product_space_y) %>%            # Versión abreviada del sector
  distinct(product, .keep_all = TRUE)      # Garantiza una fila por producto


# inner_join para unir los índices de complejidad con los atributos de productos

product_complexity_indexes <- product_complexity_indexes %>%
  inner_join(atributos_productos, by = "product") 

# asigar rauch

product_complexity_indexes <- product_complexity_indexes %>%
  mutate(rauch_short = case_when(
    categoria_rauch == "n" ~ "Differentiated goods",
    categoria_rauch == "r" ~ "Reference prices",
    categoria_rauch == "w" ~ "Homogeneous goods"
  ))



# Distribución de kernel de reflections_index_z

ggplot(product_complexity_indexes, aes(x = reflections_index_z, fill = rauch_short, colour = rauch_short)) +
  geom_density(alpha = 0.5, linewidth = 1.2) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  scale_colour_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  labs(
    x = "Reflections method index (Standardized)",
    y = "Density",
    fill = "",
    colour = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 24),       # Tamaño del texto de cada ítem
    legend.title = element_text(size = 26, face = "bold"),  # Título más grande y en negrita
    legend.key.height = unit(1.2, "cm"),          # Altura de las llaves de color
    legend.key.width = unit(1.2, "cm"),           # Ancho de los íconos
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 8)),
    colour = guide_legend(override.aes = list(size = 8))
  )





