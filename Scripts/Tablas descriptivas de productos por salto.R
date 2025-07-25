# Limpiar el entorno de trabajo para evitar conflictos con objetos existentes
rm(list = ls())  # limpiar environment

# Cargar librerías necesarias para análisis de datos, gráficos y redes
library(tidyverse)              # Manipulación y visualización de datos
library(economiccomplexity)    # Análisis de complejidad económica (Balassa index, proximidad, etc.)
library(igraph)                # Creación y manipulación de grafos
library(ggplot2)               # Gráficos base
library(beepr)                 # Notificaciones sonoras (opcional)
library(readxl)                # Lectura de archivos Excel
library(gridExtra)             # Organización de múltiples gráficos
library(ggraph)                # Visualización de redes usando gramática ggplot2
library(viridis)               # Paleta de colores perceptualmente uniforme
library(RColorBrewer)

# Establecer directorio de trabajo (asegúrese de ajustar a su ruta local)
setwd('/Users/lucasordonez/Library/CloudStorage/Dropbox/Proyecto datos')

# Crear tabla con los códigos de productos identificados mediante la estrategia HH (Hausmann-Hidalgo)
# Estos representan los 10 productos más cercanos a la frontera tecnológica del país según dicha estrategia
product_codes_HH <- as.data.frame(c("2601", "2602", "1207", "2530", "2701",                         
                                    "2709", "0802", "7201", "1701", "2207"))
colnames(product_codes_HH) <- "product_code"
product_codes_HH$orden <- 1:nrow(product_codes_HH)  # Se agrega número de orden para mantener el ranking

# Crear tabla con códigos de productos diferenciados seleccionados manualmente
# Estos productos pertenecen a la categoría de bienes diferenciados según la clasificación de Rauch
product_codes_D <- as.data.frame(c("2530", "2701", "4102", "4301", "0603", 
                                   "1211", "3401", "2104", "3603", "4407"))
colnames(product_codes_D) <- "product_code"
product_codes_D$orden <- 1:nrow(product_codes_D)

# Importar nombres cortos de productos y eliminar la columna con nombre completo
# Esto se hace para evitar duplicidades al unir con otros datasets
product_name <- read_csv('Data/Primary/products_name.csv') %>%
  select(-product_name)

# Importar atributos de productos:
# 'es_diferenciado' indica si el bien es diferenciado (clasificación de Rauch)
# 'product_space_cluster_name' refiere al clúster del Espacio-Producto al que pertenece
atributos_productos <- read_csv('Data/Primary/atributos_productos.csv') %>%
  select(product, es_diferenciado, product_space_cluster_name)

# Unir nombre de producto con sus atributos mediante inner_join
product_name_atributos <- inner_join(product_name, atributos_productos, by = c("product_code" = "product"))

# Importar índices de complejidad económica por producto
# Se utiliza tanto el índice por reflexiones (Hausmann-Hidalgo) como el de fitness (Tacchella et al.)
# Se calcula el ranking de complejidad, asignando el puesto 1 al producto más complejo
product_complexity_indexes <- read_csv('Data/Primary/product_complexity_indexes.csv') %>%
  select(product, reflections_index_z, fitness_index_z) %>%
  mutate(
    rank_reflections = rank(-reflections_index_z, ties.method = "first"),
    rank_fitness     = rank(-fitness_index_z, ties.method = "first")
  )

# Unir toda la información: nombres, atributos y complejidad en una sola tabla
product_name_atributos_complexity <- inner_join(product_name_atributos, 
                                                product_complexity_indexes, 
                                                by = c("product_code" = "product"))

# Exportar la tabla completa para uso posterior o validación manual
write_csv(product_name_atributos_complexity, 
          'Data/Primary/product_name_atributos_complexity.csv')

# Ahora vamos a crear las tablas enriquecidas para product_codes_D y product_codes_HH
# Esto permite ver para cada producto su nombre, si es diferenciado, su clúster y su complejidad
product_codes_D <- product_codes_D %>%
  inner_join(product_name_atributos_complexity, by = "product_code")

product_codes_HH <- product_codes_HH %>%
  inner_join(product_name_atributos_complexity, by = "product_code")

# Exportar las tablas finales en formato Excel para su presentación o análisis externo
library(openxlsx)  # Se sugiere mover esta línea al bloque de carga de librerías al inicio para orden

write.xlsx(product_codes_D, 
           file = "Data/Primary/product_codes_D.xlsx", 
           sheetName = "product_codes_D")

write.xlsx(product_codes_HH,
           file = "Data/Primary/product_codes_HH.xlsx", 
           sheetName = "product_codes_HH")




