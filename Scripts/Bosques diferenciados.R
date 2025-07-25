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
# - Si el RCA es mayor a 1
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




# Resultado: tabla con RCA, nombre, categoría y si es diferenciado


# Product Space con etiquetas de salto diferenciado ####

# Función para graficar el Product Space con etiquetas de salto diferenciado
plot_product_space_salto_diferenciado <- function(net, bi_matrix, products_name, distance_df,
                                                  country = "Argentina", top_n = 10, title_suffix = "") {
  # 1. Extraer RCA del país
  rca_vector <- as.numeric(bi_matrix[country, ])
  names(rca_vector) <- colnames(bi_matrix)
  V(net)$rca <- ifelse(V(net)$name %in% names(rca_vector)[which(rca_vector >= 1)], 1, 0)
  
  # 2. Separar sets de productos, filtrando los productos diferenciados no exportados
  valid_names <- V(net)$name
  prods_rca1 <- intersect(names(rca_vector)[rca_vector >= 1], valid_names)
  prods_rca0 <- intersect(names(rca_vector)[rca_vector == 0], valid_names)
  prods_diff0 <- intersect(prods_rca0, valid_names)
  prods_diff0 <- prods_diff0[V(net)[prods_diff0]$extended_rauch_description == "Differentiated products"]
  
  # 3. Filtrar distancias y obtener productos más cercanos
  distance_country <- distance_df %>%
    filter(country == !!country & product %in% prods_diff0)
  
  # 4. Validación extra para evitar errores posteriores
  prods_rca0 <- intersect(prods_rca0, V(net)$name)
  prods_rca1 <- intersect(prods_rca1, V(net)$name)
  prods_diff0 <- intersect(prods_diff0, V(net)$name)
  
  # 5. Seleccionar los 'top_n' productos más cercanos (menor distancia)
  closest_products <- distance_country %>%
    arrange(distance) %>%
    distinct(product, .keep_all = TRUE) %>%
    slice_head(n = top_n) %>%
    pull(product)
  
  # 6. Categorizar nodos con orden explícito
  V(net)$category <- case_when(
    V(net)$name %in% closest_products ~ "Top 10 nearest differentiated products",
    V(net)$rca == 1                   ~ "RCA",
    TRUE                              ~ "No RCA"
  )
  V(net)$category <- factor(V(net)$category, levels = c("No RCA", "RCA", "Top 10 nearest differentiated products"))
  
  # 7. Asignar etiquetas numéricas a los nodos
  orden_top <- tibble(product_code = closest_products) %>%
    mutate(top_n_label = as.character(row_number()))
  
  V(net)$top_label <- NA
  for (i in 1:nrow(orden_top)) {
    nodo <- orden_top$product_code[i]
    V(net)$top_label[V(net)$name == nodo] <- orden_top$top_n_label[i]
  }
  
  # 8. Usar coordenadas precargadas como atributos del nodo
  g <- ggraph(net, layout = "manual", x = V(net)$product_space_x, y = V(net)$product_space_y) +
    geom_edge_link(color = "grey80", alpha = 0.25) +
    geom_node_point(aes(size = size / 1e6, color = category), alpha = 0.8) +
    geom_node_text(aes(label = top_label), size = 4.5, color = "black", fontface = "bold", repel = TRUE, na.rm = TRUE) +
    scale_color_manual(
      values = c("No RCA" = "grey70", "RCA" = "#28833C", "Top 10 nearest differentiated products" = "#FFA500"),
      breaks = c("No RCA", "RCA", "Top 10 nearest differentiated products")
    ) +
    scale_size(range = c(4, 18), guide = FALSE) +
    labs(color = "", size = "") +
    guides(color = guide_legend(override.aes = list(size = 12))) +
    theme_void() +
    theme(
      plot.margin = margin(20, 20, 20, 20),
      legend.position  = "bottom",
      legend.text = element_text(size = 24),
      legend.title = element_text(size = 24, face = "bold")
    )
  
  # 9. Tabla lateral con productos más cercanos
  lista_top <- tibble(product_code = closest_products) %>%
    left_join(products_name, by = "product_code") %>%
    distinct(product_code, .keep_all = TRUE) %>%
    mutate(product_name_wrapped = stringr::str_wrap(product_name_short, width = 30)) %>%
    mutate(orden = row_number()) %>%
    mutate(line = paste0(orden, ". ", product_name_wrapped, " (", product_code, ")"))
  
  print(lista_top)
  
  # opcional: Crear el dataframe final con el código, nombre, y si el producto es diferenciado (D) o no (ND)
  lista_top_final <- lista_top %>%
    left_join(tibble(product_code = V(net)$name, tipo_producto = ifelse(V(net)$extended_rauch_description == "Differentiated products", "D", "ND")),
              by = "product_code") %>%
    select(product_code, product_name_short, tipo_producto)
  
  
  print(lista_top_final)
  
  tabla <- ggplot(lista_top, aes(x = 1, y = reorder(line, -orden))) +
    geom_text(aes(label = line), hjust = 0, size = 5.5) +
    scale_y_discrete() +
    scale_x_continuous(limits = c(1, 3), expand = c(0, 0)) +
    theme_void() +
    ggtitle("Top 10 nearest differentiated products") +
    theme(plot.title = element_text(hjust = 0, size = 16, face = "bold"))
  
  # 10. Combinar grafo y tabla
  gridExtra::grid.arrange(g, tabla, ncol = 2, widths = c(4, 0.75))
}

# Ejemplo de uso:
plot_product_space_salto_diferenciado(
  net = net_2223_product_ext,
  bi_matrix = bi_2223,
  products_name = products_name,
  distance_df = distance_pivot,
  country = "Argentina",
  top_n = 10,
  title_suffix = "(2022-2023)"
)


# estructura del armado del bosque diferenciado
# 1) A partir del VCR se obtiene el salto diferenciado (10 productos)
# 2) Incorporo en la matriz binaria de VCR los productos del salto diferenciado
# 3) Calculo la distancia de los productos a partir de la matriz de proximidad y la nueva matriz binaria de VCR 
# 4) Incorporo en el product space el top 10 de productos más cercanos
# 5) Ploteo el product space del bosque diferenciado. Con un color los productos diferenciados, otro color productos no diferenciados y sin color los que no se presenta VCR


# === Función para simular saltos diferenciados ===
simular_saltos_diferenciados <- function(net, bi_matrix, products_name, distance_pivot, proximity_matrix, 
                                         country = "Argentina", cantidad_productos = 10, veces_salto = 1, top_n = 10) {
  
  # Validación: verificar si el país está en la matriz de VCR
  if (!(country %in% rownames(bi_matrix))) {
    stop("El país especificado no existe en la matriz RCA.")
  }
  
  # Inicializar matriz expandida (que se irá modificando) y lista de historial de productos
  bi_expandida <- bi_matrix
  historial_productos <- list()
  
  # Bucle principal: realizar saltos diferenciados
  for (i in seq_len(veces_salto)) {
    
    # Crear tabla con VCR, nombres de producto y si es diferenciado o no
    tabla_rca <- tibble(
      product_code = colnames(bi_expandida),
      rca = bi_expandida[country, ]
    ) %>%
      left_join(products_name, by = "product_code") %>%
      mutate(
        es_diferenciado = ifelse(
          V(net)$extended_rauch_description[match(product_code, V(net)$name)] == "Differentiated products", 1, 0
        ),
        categoria_rca = ifelse(rca >= 1, "RCA > 1", "RCA ≤ 1")
      ) %>%
      select(product_code, product_name_short, rca, categoria_rca, es_diferenciado)
    
    # Agregar la distancia al país para cada producto y ordenar
    tabla_rca <- tabla_rca %>%
      left_join(
        distance_pivot %>% filter(country == !!country) %>% select(-country),
        by = c("product_code" = "product")
      ) %>%
      arrange(rca, desc(es_diferenciado), distance)
    
    # Seleccionar productos diferenciados sin VCR, más cercanos según proximidad
    productos_nuevos <- tabla_rca %>%
      filter(categoria_rca == "RCA ≤ 1",
             es_diferenciado == 1,
             !(product_code %in% names(which(bi_expandida[country, ] == 1)))) %>%
      arrange(distance) %>%
      slice_head(n = cantidad_productos) %>%
      pull(product_code)
    
    # Salir si no se encontraron productos nuevos
    if (length(productos_nuevos) == 0) break
    
    # Incorporar los productos nuevos en la matriz binaria
    bi_expandida[country, productos_nuevos] <- 1
    
    # Guardar los productos del salto actual
    historial_productos[[paste0("salto_", i)]] <- productos_nuevos
    
    # Guardar copia de la matriz binaria en el entorno global por si se quiere analizar manualmente
    assign(paste0("bi_expandida_", i), bi_expandida, envir = .GlobalEnv)
  }
  
  # Convertir historial de productos en data frame (formato largo)
  productos_por_salto_df <- bind_rows(
    lapply(names(historial_productos), function(salto) {
      tibble(salto = salto, product_code = historial_productos[[salto]])
    })
  )
  
  # Extraer los productos con VCR inicial y convertir a data frame
  productos_vcr_iniciales <- names(which(bi_matrix[country, ] == 1))
  productos_vcr_iniciales_df <- tibble(
    salto = "salto_0",
    product_code = productos_vcr_iniciales
  )
  
  # Unir productos iniciales con productos de los saltos
  productos_por_salto_df <- bind_rows(
    productos_vcr_iniciales_df,
    productos_por_salto_df
  )
  
  # Calcular la nueva matriz de distancias con la matriz binaria expandida
  nueva_distancia <- as.data.frame(distance(bi_expandida, proximity_matrix)) %>%
    mutate(country = rownames(.)) %>%
    pivot_longer(cols = -country, names_to = "product", values_to = "distance")
  
  # Devolver los resultados como lista
  return(list(
    matriz_expandida = bi_expandida,
    productos_por_salto = productos_por_salto_df,
    distancia_final = nueva_distancia
  ))
}


# === Ejemplo de uso ===

resultado <- simular_saltos_diferenciados(
  net = net_2223_product_ext,
  bi_matrix = bi_2223,
  products_name = products_name,
  distance_pivot = distance_pivot,
  proximity_matrix = prox_2223$proximity_product,
  country = "Argentina",
  cantidad_productos = 10,
  veces_salto = 2,
  top_n = 10
)

# Extraer resultados de la lista devuelta por la función
matriz_expandida <- resultado$matriz_expandida
productos_por_salto <- resultado$productos_por_salto %>% inner_join(
  products_name, 
  by = c("product_code" = "product_code"))
distancia_final <- resultado$distancia_final

# Nota: es posible graficar el product space con los productos incorporados 
# en cada salto diferenciado de forma secuencial.
# Product space del bosque diferenciado ####
# inner_join de productos_por_salto y atributos_productos

productos_por_salto <- productos_por_salto %>%
  rename(product = product_code)

# Crear variable es_diferenciado en atributos_productos
atributos_productos <- atributos_productos %>%
  mutate(es_diferenciado = ifelse(
    extended_rauch_description == "Differentiated products", 1, 0
  ))


if ("salto" %in% colnames(atributos_productos)) {
  # Si la columna 'salto' ya existe, eliminarla
  atributos_productos <- atributos_productos %>%
    select(-salto)
}


# Unir con los saltos
atributos_productos <- atributos_productos %>%
  left_join(productos_por_salto, by = c("product" = "product"))



# Asignar atributos descriptivos al grafo incluidos ahora, uno por uno
V(net_2223_product_ext)$salto <- atributos_productos$salto[match_ids]
V(net_2223_product_ext)$es_diferenciado <- atributos_productos$es_diferenciado[match_ids]



# Definir colores manualmente según condición
V(net_2223_product_ext)$color_nodo <- ifelse(
  is.na(V(net_2223_product_ext)$salto),
  "grey70",
  ifelse(V(net_2223_product_ext)$es_diferenciado == 1, "#8470FF", "#28833C")
)

# Redefinir la variable de color como etiquetas para que aparezca la leyenda
V(net_2223_product_ext)$color_nodo <- ifelse(
  is.na(V(net_2223_product_ext)$salto),
  "No RCA",
  ifelse(V(net_2223_product_ext)$es_diferenciado == 1, "Differentiated products", "Undifferentiated products")
)

# En este caso se graficaria el product space del salto en el periodo t
# Graficar del bosque diferenciado
g <- ggraph(net_2223_product_ext, 
            layout = "manual",
            x = V(net_2223_product_ext)$product_space_x,
            y = V(net_2223_product_ext)$product_space_y) +
  
  geom_edge_link(color = "grey80", alpha = 0.25) +
  
  geom_node_point(aes(size = size / 1e6, color = color_nodo), alpha = 0.8) +
  
  scale_size(range = c(4, 18), guide = FALSE) +
  
  scale_color_manual(
    values = c(
      "No RCA" = "grey70",
      "Differentiated products" = "#8470FF",
      "Undifferentiated products" = "#28833C"
    ),
    name = NULL
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 12))) +
  
  theme_void() +
  theme(
    plot.margin      = margin(20, 20, 20, 20),
    legend.position  = "bottom",
    legend.text      = element_text(size = 24),
    legend.key.size  = unit(4, "lines")
  )

print(g)



# Nota: es posible graficar el product space con los productos incorporados 
# en cada salto diferenciado de forma secuencial.
#
# Al realizar un salto en el período t, el gráfico generado mostrará el product space 
# con los productos del salto diferenciado en t + 1, ya que se utilizan 
# la matriz extendida y el objeto `distancia_final`, que son calculados por 
# la función `simular_saltos_diferenciados`.
#
# Es importante asegurarse de que el argumento `country` coincida en ambas 
# funciones: `simular_saltos_diferenciados` y `plot_product_space_salto_diferenciado`.


plot_product_space_salto_diferenciado(
  net = net_2223_product_ext,
  bi_matrix = matriz_expandida,
  products_name = products_name,
  distance_df = distancia_final,
  country = "Argentina",
  top_n = 10,
  title_suffix = ""
)


