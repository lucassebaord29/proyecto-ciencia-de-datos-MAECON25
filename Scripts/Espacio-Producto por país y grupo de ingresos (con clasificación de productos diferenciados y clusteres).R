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

# exportar products_name

write_csv(products_name, "Data/Primary/products_name.csv")

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



# Crear variable es_diferenciado en atributos_productos
atributos_productos <- atributos_productos %>%
  mutate(es_diferenciado = ifelse(
    extended_rauch_description == "Differentiated products", "Differentiated product", "Undifferentiated product"
  ))

# Asignar atributos descriptivos al grafo incluidos ahora, uno por uno
V(net_2223_product_ext)$es_diferenciado <- atributos_productos$es_diferenciado[match_ids]

# Product space por sector, cluster o categoría tecnológica ####
# Esta sección permite graficar el Product Space coloreando los nodos por sector, clúster o categoría tecnológica.
# Para esto, se deben asignar los colores correspondientes a cada nodo del grafo
# utilizando las funciones definidas a continuación. Luego, se puede graficar el Product Space con los nodos coloreados.
# solo se va a colorear los productos que se presenten RCA

# Primero debemos asignar los colores que se utilizaron en el scrip Product Space

# Vector con los colores (vector_color)
vector_cluster_colors <- c(
  "Agriculture" = "#1B9E77",
  "Minerals" =  "#E6AB02",
  "Industrial Chemicals and Metals" = "#E7298A",
  "Construction, Building, and Home Supplies" =  "#D95F02",
  "Metalworking and Electrical Machinery and Parts" = "#66A61E",
  "Textile Apparel and Accessories" = "#666666",
  "Electronic and Electrical Goods" = "#7570B3",
  "Textile and Home Goods" = "#A6761D"
)

vector_technological_category_colors <- c(
  "Primary products" =   "#7FA718"   ,
  "Resource-based" = "#BF8B12", 
  "Medium technology" =  "#D03792" ,
  "Unclassified products" = "#666666", 
  "High technology" = "#1B9E77",
  "Low technology" =  "#B16548"
)

vector_sector_colors <- c(
  "Agriculture" = "#1B9E77", 
  "Minerals" = "#96A713", 
  "Chemicals" =  "#C16610", 
  "Textiles" = "#9D7426", 
  "Stone" = "#D59D08", 
  "Metals" = "#A66753", 
  "Machinery" = "#BC4399", 
  "Electronics" = "#8D6B86", 
  "Vehicles" = "#666666"
)


vector_differentiated_colors <- c(
  "Differentiated product" = "#1B9E77", 
  "Undifferentiated product" = "#666666"
)



# Función para asignar colores, la ejecuto con el siguiente script

source("Codes/Paletas de color (sector, cluster y tecnología).R")






# Aplicar las funciones a net_2223_product_ext por sector, cluster o tecnología
net_2223_product_ext <- assign_colors_by_sector(net_2223_product_ext)
net_2223_product_ext <- assign_colors_by_cluster(net_2223_product_ext)
net_2223_product_ext <- assign_colors_by_technology_category(net_2223_product_ext)
net_2223_product_ext <- assign_colors_by_differentiation_category(net_2223_product_ext)


# Función para graficar el Product Space de un país, coloreando los productos 
# con RCA = 1 según una categoría (sector, clúster o nivel tecnológico).
plot_product_space_by_rca <- function(net, bi_matrix, country = "Argentina", color_var_attr, vector_color) {
  
  # 1. Obtener los atributos de los nodos como data frame (útil si se quiere explorar)
  node_data <- as_data_frame(net, what = "vertices")
  
  # 2. Extraer el vector de RCA para el país seleccionado
  rca_vector <- as.numeric(bi_matrix[country, ])                 # Valores binarios de RCA para cada producto
  names(rca_vector) <- colnames(bi_matrix)                       # Asignar nombres de productos al vector
  V(net)$rca <- ifelse(V(net)$name %in% names(rca_vector)[rca_vector >= 1], 1, 0)
  
  # 3. Clasificar nodos según tengan o no ventaja comparativa revelada
  V(net)$category <- case_when(
    V(net)$rca == 1 ~ "RCA",     # RCA: Ventaja Comparativa Revelada
    TRUE            ~ "No RCA"   # En gris
  )
  V(net)$category <- factor(V(net)$category, levels = c("No RCA", "RCA"))
  
  # 4. Asignar colores a los nodos:
  # - Si RCA == 1 y el atributo de color no es NA, usar el color correspondiente
  # - Si no, usar gris claro ("grey70")
  color_values <- ifelse(
    V(net)$rca == 1 & !is.na(vertex_attr(net, color_var_attr)),
    vertex_attr(net, color_var_attr),
    "grey70"
  )
  
  # 5. Construir el gráfico con ggraph
  g <- ggraph(net,
              layout = "manual",
              x = V(net)$product_space_x,   # Coordenadas del layout manual
              y = V(net)$product_space_y) +
    
    # 5.1. Dibujar enlaces entre productos (edges)
    geom_edge_link(color = "grey80", alpha = 0.25) +
    
    # 5.2. Dibujar nodos con tamaño según exportaciones y color según RCA y categoría
    geom_node_point(aes(size = size / 1e6, color = color_values), alpha = 0.8) +
    
    # 5.3. Escala de tamaños (no se muestra la leyenda de tamaño)
    scale_size(range = c(4, 18), guide = FALSE) +
    
    # 5.4. Escala de colores basada en los valores hexadecimales directamente
    scale_color_identity(
      guide = "legend",
      breaks = vector_color,           # Colores que deben aparecer en la leyenda
      labels = names(vector_color)     # Etiquetas descriptivas para cada color
    ) +
    
    # 5.5. Etiquetas vacías para no mostrar "Color" ni "Size" como títulos de leyenda
    labs(color = "", size = "") +
    
    # 5.6. Configurar leyenda de color
    guides(color = guide_legend(
      override.aes = list(size = 12, shape = 15),  # Usar cuadrado grande en leyenda
      nrow = 2,                                    # Número de filas en la leyenda
      byrow = TRUE,
      title.position = "top"
    )) +
    
    # 5.7. Estética general del gráfico
    theme_void() +
    theme(
      plot.margin      = margin(20, 20, 20, 20),       # Márgenes alrededor del gráfico
      legend.position  = "bottom",                     # Ubicar leyenda debajo del gráfico
      legend.text      = element_text(size = 24),      # Tamaño del texto en leyenda
      legend.title     = element_text(size = 28, face = "bold"),  # Título si existiera
      legend.key.size  = unit(4, "lines")              # Tamaño del ícono en la leyenda
    )
  
  # 6. Retornar el objeto gráfico
  return(g)
}

plot_product_space_by_rca (
  net = net_2223_product_ext,
  bi_matrix = bi_2223,
  country = "Germany",
  color_var_attr = "cluster_color",    #  Las opciones son "sector_color", "cluster_color" o "tech_category_color"
  vector_color = vector_cluster_colors   # Las opciones son vector_sector_colors, vector_cluster_colors o vector_technological_category_colors
)


# Product space por grupo de ingreso ####

# Cargar paquetes necesarios para manipulación de datos
library(dplyr)
library(tidyr)

# Crear bi_matrix por grupo de ingreso:
# 1. Filtrar países con grupo de ingreso no nulo
# 2. Agrupar por grupo de ingreso y producto
# 3. Sumar exportaciones promedio por grupo-producto
# 4. Convertir a formato wide (filas: income groups, columnas: productos)
# 5. Usar nombres de grupos de ingreso como nombres de fila
bi_matrix_income_group <- base_final %>%
  filter(!is.na(`Income group`)) %>%
  group_by(`Income group`, product) %>%
  summarise(export_value = sum(export_value_avg, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = product, values_from = export_value, values_fill = 0) %>%
  column_to_rownames("Income group")

# Verificar dimensiones de la nueva matriz
# Filas: grupos de ingreso; Columnas: productos
dim(bi_matrix_income_group)

# Calcular el vector de exportaciones totales mundiales por producto
world_exports <- colSums(bi_matrix_income_group, na.rm = TRUE)
# Calcular el total global de exportaciones
total_world_exports <- sum(world_exports)

# Inicializar matriz RCA con misma estructura que la matriz original
bi_rca_income_group <- bi_matrix_income_group

# Calcular RCA para cada grupo de ingreso:
# (exportaciones del grupo / total del grupo) / (exportaciones mundiales / total mundial)
for (i in 1:nrow(bi_rca_income_group)) {
  group_exports <- bi_matrix_income_group[i, ]
  total_group_exports <- sum(group_exports)
  bi_rca_income_group[i, ] <- (group_exports / total_group_exports) / 
    (world_exports / total_world_exports)
}

# Binarizar la matriz RCA: 1 si RCA > 1, 0 en caso contrario
bi_rca_income_group_bin <- ifelse(bi_rca_income_group >= 1, 1, 0)

# Graficar el Product Space para un grupo de ingreso específico
plot_product_space_by_rca(
  net = net_2223_product_ext,                           # grafo de productos con coordenadas
  bi_matrix = bi_rca_income_group_bin,                  # matriz RCA binaria por grupo de ingreso
  country = "High income" ,                             # grupo de ingreso a grafica: las opciones son: Low income, Lower middle income, Upper middle income, High income
  color_var_attr = "differentiation_color",                     # atributo de color para nodos # Las opcinoes son: cluster_color, tech_category_color, sector_color y differentation_color
  vector_color = vector_differentiated_colors                  # vector que asigna color a cada categoría # las opciones son: vector_cluster_colors, vector_technological_category_colors, vector_sector_colors y vector_differentiated_colors
)


