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
library(pheatmap)
library(RColorBrewer)
library(gridExtra)


setwd('/Users/lucasordonez/Library/CloudStorage/Dropbox/Proyecto datos')

# Importar base de datos principal con información de exportaciones y atributos de productos
base_final <- read_csv("Data/Primary/base_final.csv")



## Calcular el índice de Balassa (RCA) ----------
# Mide si un país tiene ventaja comparativa revelada en un producto dado
bi_2223 <- balassa_index(base_final,
                         country = "country", 
                         product = "product", 
                         value = "export_value_avg")

## Calcular matriz de proximidad entre productos ----------
# Basado en la co-exportación de productos (probabilidad condicional de ventaja compartida)
prox_2223 <- proximity(bi_2223)



# Armado de red
# Projections ----

net_2223 <- projections(prox_2223$proximity_country, prox_2223$proximity_product)

# Heatmat---------
# countries


colores <- colorRampPalette(brewer.pal(n = 9, name = "YlOrRd"))(100)




# heatmap proximity_country ######
pheatmap(prox_2223$proximity_country,   # Matriz de datos (similitud/proximidad entre países)
         
         color = colores,               # Usar la paleta de colores definida
         
         clustering_distance_rows = "euclidean",  # Método de distancia para filas
         clustering_distance_cols = "euclidean",  # Método de distancia para columnas
         clustering_method = "average",           # Método de agrupamiento (jerárquico promedio)
         
         show_rownames = TRUE,          # Mostrar nombres de los países en las filas
         show_colnames = TRUE,          # Mostrar nombres de los países en las columnas
         
         fontsize = 12,                 # Tamaño base del texto
         fontsize_row = 7,            # Tamaño del texto para nombres en filas
         fontsize_col = 7,            # Tamaño del texto para nombres en columnas
         
         cellwidth = 6.5,                # Ancho de cada celda del heatmap
         cellheight = 6.5,               # Alto de cada celda del heatmap
         
         angle_col = 90,                # Rotar nombres de columnas 45° para mejor legibilidad
         
         border_color = NA,             # Eliminar bordes entre celdas (más limpio visualmente)
         
         legend = TRUE,                 # Mostrar leyenda de colores
         legend_breaks = c(0, 0.25, 0.5, 0.75, 1),           # Marcas en la leyenda
         legend_labels = c("0", "0.25", "0.5", "0.75", "1"), # Etiquetas correspondientes
        
         treeheight_row = 0,           # Oculta dendrograma de filas
         treeheight_col = 0            # Oculta dendrograma de columnas
         )




# heatmap proximity_product ######
hmap_jerar <- pheatmap(prox_2223$proximity_product,   # Matriz de datos (similitud/proximidad entre países)
         
         color = colores,               # Usar la paleta de colores definida
         
         clustering_distance_rows = "euclidean",  # Método de distancia para filas
         clustering_distance_cols = "euclidean",  # Método de distancia para columnas
         clustering_method = "average",           # Método de agrupamiento (jerárquico promedio)
         
         show_rownames = FALSE,          # Mostrar nombres de los países en las filas
         show_colnames = FALSE,          # Mostrar nombres de los países en las columnas
         
         fontsize = 12,                 # Tamaño base del texto
         fontsize_row = 7,            # Tamaño del texto para nombres en filas
         fontsize_col = 7,            # Tamaño del texto para nombres en columnas
         
         cellwidth = 1.1,                # Ancho de cada celda del heatmap
         cellheight = 1.1,               # Alto de cada celda del heatmap
         
         angle_col = 90,                # Rotar nombres de columnas 45° para mejor legibilidad
         
         border_color = NA,             # Eliminar bordes entre celdas (más limpio visualmente)
         
         legend = TRUE,                 # Mostrar leyenda de colores
         legend_breaks = c(0, 0.25, 0.5, 0.75, 1),           # Marcas en la leyenda
         legend_labels = c("0", "0.25", "0.5", "0.75", "1"), # Etiquetas correspondientes
         
         treeheight_row = 0,           # Oculta dendrograma de filas
         treeheight_col = 0            # Oculta dendrograma de columnas
)


