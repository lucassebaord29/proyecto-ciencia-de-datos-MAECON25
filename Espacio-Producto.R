# Limpiar el entorno de trabajo para evitar conflictos con objetos existentes
rm(list = ls())  # limpiar environment

# Cargar librerías necesarias para análisis de datos, gráficos y redes
library(tidyverse)              # Manipulación y visualización de datos
library(dplyr)                 # Manipulación de datos (parte de tidyverse)
library(economiccomplexity)    # Análisis de complejidad económica (Balassa index, proximidad, etc.)
library(igraph)                # Creación y manipulación de grafos
library(ggplot2)               # Gráficos base
library(beepr)                 # Notificaciones sonoras (opcional)
library(readxl)                # Lectura de archivos Excel
library(gridExtra)             # Organización de múltiples gráficos
library(ggraph)                # Visualización de redes usando gramática ggplot2
# library(igraph)              # (Carga repetida, puede omitirse)
library(viridis)               # Paleta de colores perceptualmente uniforme
library(ggraph)
library(igraph)
library(ggplot2)
library(RColorBrewer)
library(rlang)# Establecer directorio de trabajo (asegúrese de ajustar a su ruta local)

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

# Cargar grafo preconstruido con estructura del Product Space
net_2223_product_ext <- readRDS('Backbone/net_2223_product_ext.rds')

# Agregar tamaño a los nodos en función del valor total exportado por producto
aggregated_products_2223 <- aggregate(base_final$export_value_avg, 
                                      by = list(country = base_final$product), 
                                      FUN = sum)  # Suma total por producto

# Convertir en vector con nombres para matchear con nodos del grafo
aggregated_products_2223 <- setNames(aggregated_products_2223$x, 
                                     aggregated_products_2223$country)

# Asignar tamaño a los nodos en el grafo según exportaciones agregadas
V(net_2223_product_ext)$size <- aggregated_products_2223[match(V(net_2223_product_ext)$name, 
                                                               names(aggregated_products_2223))]

# Crear diccionario de nombres cortos de productos para futuras visualizaciones
products_name <- base_final %>%
  select(product, product_name_short, product_name) %>%
  distinct(product, .keep_all = TRUE) %>%
  rename(product_code = product)

# Asegurar que el nombre de los nodos y la columna de producto sean del mismo tipo (string)
V(net_2223_product_ext)$name <- as.character(V(net_2223_product_ext)$name)
base_final$product <- as.character(base_final$product)

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

# Relacionar productos del grafo con los atributos mediante sus nombres únicos
match_ids <- match(V(net_2223_product_ext)$name, atributos_productos$product)

# Asignar atributos descriptivos al grafo, uno por uno
V(net_2223_product_ext)$categoria_rauch <- atributos_productos$categoria_rauch[match_ids]
V(net_2223_product_ext)$extended_rauch_description <- atributos_productos$extended_rauch_description[match_ids]
V(net_2223_product_ext)$product_name <- atributos_productos$product_name[match_ids]
V(net_2223_product_ext)$product_space_cluster_name <- atributos_productos$product_space_cluster_name[match_ids]
V(net_2223_product_ext)$technological_categories <- atributos_productos$technological_categories[match_ids]
V(net_2223_product_ext)$technological_categories_ab <- atributos_productos$technological_categories_ab[match_ids]
V(net_2223_product_ext)$sector_name <- atributos_productos$sector_name[match_ids]
V(net_2223_product_ext)$sector_name_short <- atributos_productos$sector_name_short[match_ids]
V(net_2223_product_ext)$product_space_x <- atributos_productos$product_space_x[match_ids]
V(net_2223_product_ext)$product_space_y <- atributos_productos$product_space_y[match_ids]


# Incorporando como atributo la diferenciación de bienes vs no diferenciados según Rauch


# Crear variable es_diferenciado en atributos_productos
atributos_productos <- atributos_productos %>%
  mutate(es_diferenciado = ifelse(
    extended_rauch_description == "Differentiated products", "Differentiated product", "Undifferentiated product"
  ))

# Asignar atributos descriptivos al grafo incluidos ahora, uno por uno
V(net_2223_product_ext)$es_diferenciado <- atributos_productos$es_diferenciado[match_ids]


# Product space por sector, cluster o categoría tecnológica ####
plot_product_space_by_color <- function(net, color_var = "sector_name_short", seed = 123) {
  
  node_data <- as_data_frame(net, what = "vertices")
  
  if (!(color_var %in% colnames(node_data))) {
    stop("La variable indicada no existe en los nodos del grafo.")
  }
  
  color_values <- node_data[[color_var]]
  n_colors <- length(unique(color_values))
  
  #  Fijar semilla para reproducibilidad
  set.seed(seed)
  
  # Crear paleta reproducible basada en Dark2 (sin amarillo)
  base_palette <- brewer.pal(8, "Dark2")
  palette <- colorRampPalette(base_palette)(n_colors)
  
  # Mapear los valores de color_var a los colores de la paleta
  color_mapping <- setNames(palette, unique(color_values))
  

  # Graficar
  g <- ggraph(net, 
              layout = "manual",
              x = V(net)$product_space_x,
              y = V(net)$product_space_y) +
    
    geom_edge_link(color = "grey80", alpha = 0.25) +
    
    geom_node_point(aes(size = size / 1e6, color = .data[[color_var]]),
                    alpha = 0.8) +
    
    scale_size(range = c(4, 18), guide = FALSE) +
    scale_color_manual(values = palette) +
    
    labs(color = "", size = "") +
    guides(
      color = guide_legend(override.aes = list(size = 12, shape = 15))
    ) +
    theme_void() +
    theme(
      plot.margin      = margin(20, 20, 20, 20),
      legend.position  = "bottom",
      legend.text      = element_text(size = 24),
      legend.key.size  = unit(4, "lines")
    )
  
  return(g)
}


# Por sector
plot_product_space_by_color(net_2223_product_ext, "sector_name_short")

# Por clúster
plot_product_space_by_color(net_2223_product_ext, "product_space_cluster_name")

# Por categoría tecnológica
plot_product_space_by_color(net_2223_product_ext, "technological_categories")

# Por diferenciaciación de Rauch
plot_product_space_by_color(net_2223_product_ext, "es_diferenciado")


# Bonus track: incorporando intensidad de proximidad entre productos como atributo de las aristas del grafo
# Incorporando intensidad de proximidad

# Calcular proximidad entre productos y asignar como atributo de las aristas del grafo

# 1. Extraer el grafo
g <- net_2223_product_ext

# 2. Extraer la matriz de proximidad producto–producto
prox_mat <- as.matrix(prox_2223$proximity_product)

# 3. Asegurarse de que los nombres coincidan con los vértices del grafo
rownames(prox_mat) <- colnames(prox_mat) <- V(g)$name

# 4. Obtener las aristas del grafo como pares de nombres
edge_mat <- get.edgelist(g, names = TRUE)

# 5. Para cada arista, extraer su valor de proximidad de la matriz
proximities <- mapply(function(u, v) prox_mat[u, v],
                      edge_mat[, 1],
                      edge_mat[, 2])

# 6. Asignar ese vector como atributo 'proximity' de las aristas
E(g)$proximity <- proximities

# 7. Guardar el grafo con el nuevo atributo
net_2223_product_ext <- g




plot_product_space_by_color_proximities <- function(net, color_var = "sector_name_short", seed = 123) {
  
  node_data <- as_data_frame(net, what = "vertices")
  
  if (!(color_var %in% colnames(node_data))) {
    stop("La variable indicada no existe en los nodos del grafo.")
  }
  
  color_values <- node_data[[color_var]]
  n_colors <- length(unique(color_values))
  
  set.seed(seed)
  
  base_palette <- brewer.pal(8, "Dark2")
  palette <- colorRampPalette(base_palette)(n_colors)
  
  g <- ggraph(net, 
              layout = "manual",
              x = V(net)$product_space_x,
              y = V(net)$product_space_y) +
    
    # Aristas con color por proximidad (usar edge_color)
    geom_edge_link(aes(edge_color = proximity),
                   width = 0.7, alpha = 0.25) +
    
    # Nodos coloreados por categoría
    geom_node_point(aes(size = size / 1e6, color = .data[[color_var]]),
                    alpha = 0.8) +
    
    scale_size(range = c(4, 18), guide = FALSE) +
    
    # Escala de color para nodos
    scale_color_manual(values = palette) +
    
    # Escala de color para aristas
    scale_edge_color_gradientn(
      colours = c("#30a5ff", "#66c2a5", "#F7C45E", "red"),
      values  = scales::rescale(c(0, 0.4, 0.55, 0.65, 1)),
      breaks  = c(0, 0.4, 0.55, 0.65, 1),
      labels  = c("0", "0.4", "0.55", "0.65", "1"),
      name    = "Proximidad"
    ) +
    
    labs(color = "", size = "") +
    
    guides(
      color = guide_legend(override.aes = list(size = 12, shape = 15)),
      edge_color = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        ticks = TRUE,
        barwidth = unit(8, "cm"),
        barheight = unit(0.4, "cm"),
        tickheight = unit(0.3, "cm")
      )
    ) +
    
    theme_void() +
    theme(
      plot.margin      = margin(20, 20, 20, 20),
      legend.position  = "bottom",
      legend.text      = element_text(size = 24),
      legend.title     = element_text(size = 28, face = "bold"),
      legend.key.size  = unit(4, "lines")
    )
  
  return(g)
}


# Por sector
plot_product_space_by_color_proximities(net_2223_product_ext, "sector_name_short")

# Por clúster
plot_product_space_by_color_proximities(net_2223_product_ext, "product_space_cluster_name")

# Por categoría tecnológica
plot_product_space_by_color_proximities(net_2223_product_ext, "technological_categories")

# Por diferenciación de Rauch

plot_product_space_by_color_proximities(net_2223_product_ext, "es_diferenciado")

