# Limpiar el entorno de trabajo para evitar conflictos con objetos existentes
rm(list = ls())  # limpiar environment

# Cargar librerías necesarias para análisis de datos, gráficos y redes
library(patchwork)
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
library(tidyr)
library(dplyr)
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



# Calculo la métrica de distancia como el ATLAS ####

distance <- as.data.frame(distance(bi_2223, prox_2223$proximity_product))

# Agregás la columna 'country' con los nombres de las filas (países)
distance <- distance %>%
  mutate(country = rownames(distance))

# Pivotás el dataframe para pasarlo a formato largo
distance_pivot <- distance %>%
  pivot_longer(
    cols = -country,          # Pivotea todas las columnas excepto 'country'
    names_to = "product",     # Nombre de la nueva columna que guarda los productos
    values_to = "distance"    # Nombre de la columna con los valores de distancia
  )




# Paises, RCA y productos diferenciados ####

# Función para generar un dataframe con los productos de un país específico
# Incluye:
# - El valor del RCA
# - Si el RCA es mayor  a 1
# - Si el producto es diferenciado (1) o no (0), según la clasificación de Rauch

crear_tabla_rca <- function(net, bi_matrix, products_name, country = "Argentina") {
  # Verificar que el país exista en la matriz RCA
  if (!(country %in% rownames(bi_matrix))) {
    stop("El país especificado no existe en la matriz RCA.")
  }
  
  # Crear el dataframe con toda la información combinada
  tabla_rca <- tibble(
    product_code = colnames(bi_matrix),
    rca = bi_matrix[country, ]
  ) %>%
    left_join(products_name, by = "product_code") %>%
    mutate(
      es_diferenciado = ifelse(
        V(net)$extended_rauch_description[match(product_code, V(net)$name)] == "Differentiated products", 1, 0
      ),
      categoria_rca = ifelse(rca >= 1, "RCA > 1", "RCA ≤ 1")
    ) %>%
    select(product_code, product_name_short, rca, categoria_rca, es_diferenciado)
  
  # Devolver el dataframe
  return(tabla_rca)
}

# Ejemplo de uso: generar dataframe para un país específico
productos_rca_pais <- crear_tabla_rca(
  net = net_2223_product_ext,
  bi_matrix = bi_2223,
  products_name = products_name,
  country = "Argentina"  # Cambiar por el país que desees analizar
)



# Unir la tabla de productos con RCA y la tabla de distancias del país seleccionado

productos_rca_pais <- productos_rca_pais %>%
  left_join(
    distance_pivot %>% filter(country == "Argentina") %>% select(-country),
    by = c("product_code" = "product")
  ) %>%
  arrange(rca, desc(es_diferenciado), distance)  # Ordenar por RCA asc, diferenciados primero, distancia asc



# Importo los indices de complejidad de los productos 

product_complexity_indexes <- read_csv("Data/Primary/product_complexity_indexes.csv")



# left_join con procutos_rca_pais para agregar el índice de complejidad de los productos
productos_rca_pais <- productos_rca_pais %>%
  left_join(product_complexity_indexes, by = c("product_code" = "product")) 


# Solo con reflections


# Indices de complejidad (reflections)
com_ref_1819 <- complexity_measures(bi_2223, method = "reflections")
product_com_ref_1819 <- com_ref_1819$complexity_index_product 

# Calcular COI

COG <- complexity_outlook(
  bi_2223,
  prox_2223$proximity_product,
  product_com_ref_1819
)

# Extraer la matriz desde la lista
mat_gain <- COG$complexity_outlook_gain


# Convertir la matriz a data.frame y agregar columna de país
df_gain <- as.data.frame(COG$complexity_outlook_gain)
df_gain$country <- rownames(COG$complexity_outlook_gain)


# Convertir a formato largo
df_gain_long <- df_gain %>%
  pivot_longer(
    cols = -country,
    names_to = "product_code",
    values_to = "COG"
  )

# Filtrar para el país de interés (Argentina)

df_gain_long <- df_gain_long %>%
  filter(country == "Argentina") %>%
  select(-country)  # Eliminar la columna de país, ya que solo es para Argentina

# Unir con la tabla de productos RCA

productos_rca_pais <- productos_rca_pais %>%
  left_join(df_gain_long, by = "product_code") 


# Extraer de net_2223_product_ext los atributos de los productos size

productos_size <- data.frame(
  product_code = V(net_2223_product_ext)$name,
  size = V(net_2223_product_ext)$size
)

# incorporarlo al dataframe atributos_productos

atributos_productos <- atributos_productos %>%
  left_join(productos_size, by = c("product" = "product_code"))

# Unir con la tabla de productos RCA

productos_rca_pais <- productos_rca_pais %>%
  left_join(atributos_productos , by = c("product_code" = "product"))


library(ggplot2)
library(dplyr)
library(scales)



# Preparar datos
df_plot <- productos_rca_pais %>%
  filter(rca == 0) %>%
  select(-es_diferenciado.x) %>%
  rename(es_diferenciado = es_diferenciado.y)


# Graficar
ggplot(df_plot, aes(x = distance, y = COG,
                    size = size / 1e6,
                    fill = es_diferenciado)) +
  geom_point(alpha = 0.75, stroke = 0.2, shape = 21, colour = "grey30") +
  scale_size_continuous(guide = "none", range = c(3, 20)) +  # Aumentar tamaño de puntos y eliminar leyenda de size
  scale_fill_manual(
    name = "",
    values = c("Differentiated product" = "#1B9E77",
               "Undifferentiated product" = "#666666")
  ) +
  guides(fill = guide_legend(override.aes = list(size = 10))) +  # Aumentar puntos en leyenda
  labs(
    x = "Distance",
    y = "Opportunity Gain (COG)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    legend.key.size = unit(1.5, "lines"),
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(size = 14),
  )



# Preparar datos
df_plot <- productos_rca_pais %>%
  filter(rca == 0) %>%
  select(-es_diferenciado.x) %>%
  rename(es_diferenciado = es_diferenciado.y)


# Graficar diferenciados y no diferenciados
COG_diff <- ggplot(df_plot, aes(x = distance, y = COG,
                    size = size / 1e6,
                    fill = es_diferenciado)) +
  geom_point(alpha = 0.75, stroke = 0.2, shape = 21, colour = "grey30") +
  scale_size_continuous(guide = "none", range = c(3, 20)) +  # Aumentar tamaño de puntos y eliminar leyenda de size
  scale_fill_manual(
    name = "",
    values = c("Differentiated product" = "#1B9E77",
               "Undifferentiated product" = "#666666")
  ) +
  guides(fill = guide_legend(override.aes = list(size = 10))) +  # Aumentar puntos en leyenda
  labs(
    x = "Distance",
    y = "Opportunity Gain (COG)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    legend.key.size = unit(1.5, "lines"),
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(size = 14)
  )


# Graficar cluster
COG_cluster <- ggplot(df_plot, aes(x = distance, y = COG,
                    size = size / 1e6,
                    fill = product_space_cluster_name)) +
  geom_point(alpha = 0.75, stroke = 0.2, shape = 21, colour = "grey30") +
  scale_size_continuous(guide = "none", range = c(3, 20)) +  # Aumentar tamaño de puntos y eliminar leyenda de size
  scale_fill_manual(
    name = "",
    values = c(
      "Agriculture" = "#1B9E77",
      "Minerals" =  "#E6AB02",
      "Industrial Chemicals and Metals" = "#E7298A",
      "Construction, Building, and Home Supplies" =  "#D95F02",
      "Metalworking and Electrical Machinery and Parts" = "#66A61E",
      "Textile Apparel and Accessories" = "#666666",
      "Electronic and Electrical Goods" = "#7570B3",
      "Textile and Home Goods" = "#A6761D"
    )
  ) +
  guides(fill = guide_legend(override.aes = list(size = 10))) +  # Aumentar puntos en leyenda
  labs(
    x = "Distance",
    y = "Opportunity Gain (COG)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    legend.key.size = unit(1.5, "lines"),
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(size = 14))


# Combinar los gráficos usando patchwork


combined_plot_COG <- COG_diff + COG_cluster + plot_layout(nrow = 2)








# Graficar diferenciados y no diferenciados
PC_diff <- ggplot(df_plot, aes(x = distance, y = reflections_index_z,
                                size = size / 1e6,
                                fill = es_diferenciado)) +
  geom_point(alpha = 0.75, stroke = 0.2, shape = 21, colour = "grey30") +
  scale_size_continuous(guide = "none", range = c(3, 20)) +  # Aumentar tamaño de puntos y eliminar leyenda de size
  scale_fill_manual(
    name = "",
    values = c("Differentiated product" = "#1B9E77",
               "Undifferentiated product" = "#666666")
  ) +
  guides(fill = guide_legend(override.aes = list(size = 10))) +  # Aumentar puntos en leyenda
  labs(
    x = "Distance",
    y = "Product Complexity") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    legend.key.size = unit(1.5, "lines"),
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(size = 14)
  )





# Graficar cluster
PC_cluster <- ggplot(df_plot, aes(x = distance, y = reflections_index_z,
                                   size = size / 1e6,
                                   fill = product_space_cluster_name)) +
  geom_point(alpha = 0.75, stroke = 0.2, shape = 21, colour = "grey30") +
  scale_size_continuous(guide = "none", range = c(3, 20)) +  # Aumentar tamaño de puntos y eliminar leyenda de size
  scale_fill_manual(
    name = "",
    values = c(
      "Agriculture" = "#1B9E77",
      "Minerals" =  "#E6AB02",
      "Industrial Chemicals and Metals" = "#E7298A",
      "Construction, Building, and Home Supplies" =  "#D95F02",
      "Metalworking and Electrical Machinery and Parts" = "#66A61E",
      "Textile Apparel and Accessories" = "#666666",
      "Electronic and Electrical Goods" = "#7570B3",
      "Textile and Home Goods" = "#A6761D"
    )
  ) +
  guides(fill = guide_legend(override.aes = list(size = 10))) +  # Aumentar puntos en leyenda
  labs(
    x = "Distance",
    y = "Product Complexity"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    legend.key.size = unit(1.5, "lines"),
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(size = 14)
  )

# Combinar los gráficos usando patchwork

combined_plot_PC <- PC_diff + PC_cluster + plot_layout(nrow = 2)
