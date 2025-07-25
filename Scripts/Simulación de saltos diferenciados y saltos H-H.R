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
library(ggplot2)
library(patchwork)

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



# === Función para simular saltos diferenciados === #####
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

resultado_salto_diferenciado <- simular_saltos_diferenciados(
  net = net_2223_product_ext,
  bi_matrix = bi_2223,
  products_name = products_name,
  distance_pivot = distance_pivot,
  proximity_matrix = prox_2223$proximity_product,
  country = "Argentina",
  cantidad_productos = 10,
  veces_salto = 1,
  top_n = 10
)


# Extraer resultados de la lista devuelta por la función
matriz_expandida_sd <- resultado_salto_diferenciado$matriz_expandida
productos_por_salto_sd <- resultado_salto_diferenciado$productos_por_salto %>% inner_join(
  products_name, 
  by = c("product_code" = "product_code"))
distancia_final_sd <- resultado_salto_diferenciado$distancia_final







# === Función para simular saltos à la HH === ####
simular_saltos_ala_HH <- function(net, bi_matrix, products_name, distance_pivot, proximity_matrix, 
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
    
    # Seleccionar productos sin VCR, más cercanos según proximidad
    productos_nuevos <- tabla_rca %>%
      filter(categoria_rca == "RCA ≤ 1",
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




# === Simulación de saltos a la Hidalgo-Hausmann (HH) ===#####

resultado_ala_HH <- simular_saltos_ala_HH(
  net = net_2223_product_ext,
  bi_matrix = bi_2223,
  products_name = products_name,
  distance_pivot = distance_pivot,
  proximity_matrix = prox_2223$proximity_product,
  country = "Argentina",
  cantidad_productos = 10,
  veces_salto = 10,
  top_n = 10
)

# Extraer resultados de la lista devuelta por la función resultado_ala_HH
matriz_expandida_sHH <- resultado_ala_HH$matriz_expandida
productos_por_salto_sHH <- resultado_ala_HH$productos_por_salto
distancia_final_sHH <- resultado_ala_HH$distancia_final


# === Simulación de saltos diferenciados === ####

resultado_salto_diferenciado <- simular_saltos_diferenciados(
  net = net_2223_product_ext,
  bi_matrix = bi_2223,
  products_name = products_name,
  distance_pivot = distance_pivot,
  proximity_matrix = prox_2223$proximity_product,
  country = "Argentina",
  cantidad_productos = 10,
  veces_salto = 10,
  top_n = 10
)


# Extraer resultados de la lista devuelta por la función
matriz_expandida_sd <- resultado_salto_diferenciado$matriz_expandida
productos_por_salto_sd <- resultado_salto_diferenciado$productos_por_salto %>% inner_join(
  products_name, 
  by = c("product_code" = "product_code"))
distancia_final_sd <- resultado_salto_diferenciado$distancia_final



# Vamos a comparar con los indices de complejidad #####

# Importamos la base de datos de complejidad

country_complexity_indexes <- read_csv("Data/Primary/country_complexity_indexes.csv")




# Función para calcular índices de complejidad a partir de una matriz binaria de exportaciones
calcular_indices_complejidad <- function(bi_matrix) {
  
  # Reflections
  com_ref <- complexity_measures(bi_matrix, method = "reflections")
  df_ref <- data.frame(
    country = names(com_ref$complexity_index_country),
    reflections_index = as.numeric(com_ref$complexity_index_country)
  )
  
  # Fitness
  com_fit <- complexity_measures(bi_matrix, method = "fitness")
  df_fit <- data.frame(
    country = names(com_fit$complexity_index_country),
    fitness_index = as.numeric(com_fit$complexity_index_country)
  )
  
  # Eigenvalues
  com_eig <- complexity_measures(bi_matrix, method = "eigenvalues")
  df_eig <- data.frame(
    country = names(com_eig$complexity_index_country),
    eigenvalues_index = as.numeric(com_eig$complexity_index_country)
  )
  
  # Unir los resultados y estandarizar
  df_complejidad <- df_ref %>%
    left_join(df_fit, by = "country") %>%
    left_join(df_eig, by = "country") %>%
    mutate(across(
      .cols = c("reflections_index", "fitness_index", "eigenvalues_index"),
      .fns = ~ as.numeric(scale(.)),
      .names = "{.col}_z"
    ))
  
  return(df_complejidad)
}





# Funcion para calcular saltos secuenciales de un paìs ####
# saltos secuenciales Argentina ####
# Inicializar dataframes vacíos
ARG_por_vecessalto_sd <- data.frame()
ARG_por_vecessalto_sHH <- data.frame()

# Loop sobre la cantidad de saltos
for (i in 1:20) {
  
  # --- Simulación Hidalgo-Hausmann ---
  resultado_HH_i <- simular_saltos_ala_HH(
    net = net_2223_product_ext,
    bi_matrix = bi_2223,
    products_name = products_name,
    distance_pivot = distance_pivot,
    proximity_matrix = prox_2223$proximity_product,
    country = "Argentina",
    cantidad_productos = 10,
    veces_salto = i,
    top_n = 10
  )
  matriz_HH_i <- resultado_HH_i$matriz_expandida
  complejidad_HH_i <- calcular_indices_complejidad(matriz_HH_i)
  ARG_HH_i <- complejidad_HH_i %>%
    filter(country == "Argentina") %>%
    mutate(veces_salto = i, metodo = "HH")
  ARG_por_vecessalto_sHH <- bind_rows(ARG_por_vecessalto_sHH, ARG_HH_i)
  
  # --- Simulación Salto Diferenciado ---
  resultado_sd_i <- simular_saltos_diferenciados(
    net = net_2223_product_ext,
    bi_matrix = bi_2223,
    products_name = products_name,
    distance_pivot = distance_pivot,
    proximity_matrix = prox_2223$proximity_product,
    country = "Argentina",
    cantidad_productos = 10,
    veces_salto = i,
    top_n = 10
  )
  matriz_sd_i <- resultado_sd_i$matriz_expandida
  complejidad_sd_i <- calcular_indices_complejidad(matriz_sd_i)
  ARG_sd_i <- complejidad_sd_i %>%
    filter(country == "Argentina") %>%
    mutate(veces_salto = i, metodo = "Salto Diferenciado")
  ARG_por_vecessalto_sd <- bind_rows(ARG_por_vecessalto_sd, ARG_sd_i)
}


# hacer un ggplot de ARG_por_vecessalto_sd y ARG_por_vecessalto_sHH. En el eje Y reflections_index y en el eje x veces_salto


# 1. reflections_index_z
p1 <- ggplot() +
  geom_line(data = ARG_por_vecessalto_sd,
            aes(x = veces_salto, y = reflections_index, color = "Differentiated jump"),
            size = 1.5) +
  geom_line(data = ARG_por_vecessalto_sHH,
            aes(x = veces_salto, y = reflections_index, color = "HH jump"),
            size = 1.5) +
  labs(
    title = "",
    x = "Number of jumps",
    y = "Index (standardized)"
  ) +
  scale_color_manual(values = c("Differentiated jump" = "#1f77b4",
                                "HH jump" = "#d62728")) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 22),
    legend.key.width = unit(3, "cm"),
    plot.margin = margin(20, 20, 20, 20)
  )

# 2. fitness_index_z
p2 <- ggplot() +
  geom_line(data = ARG_por_vecessalto_sd,
            aes(x = veces_salto, y = fitness_index, color = "Differentiated jump"),
            size = 1.5) +
  geom_line(data = ARG_por_vecessalto_sHH,
            aes(x = veces_salto, y = fitness_index, color = "HH jump"),
            size = 1.5) +
  labs(
    title = "",
    x = "Number of jumps",
    y = "Index (standardized)"
  ) +
  scale_color_manual(values = c("Differentiated jump" = "#1f77b4",
                                "HH jump" = "#d62728")) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 22),
    legend.key.width = unit(3, "cm"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Combinar los dos plots horizontalmente
(p1 | p2) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


# Simulaciones secuenciales para múltiples países ####
# Lista de países a comparar
paises <- c("Argentina", "Chile", "Paraguay")

# Inicializar dataframes vacíos
resultados_sd <- data.frame()
resultados_sHH <- data.frame()

# Loop por país
for (pais in paises) {
  for (i in 1:20) {
    
    # --- Simulación Hidalgo-Hausmann ---
    resultado_HH_i <- simular_saltos_ala_HH(
      net = net_2223_product_ext,
      bi_matrix = bi_2223,
      products_name = products_name,
      distance_pivot = distance_pivot,
      proximity_matrix = prox_2223$proximity_product,
      country = pais,
      cantidad_productos = 10,
      veces_salto = i,
      top_n = 10
    )
    matriz_HH_i <- resultado_HH_i$matriz_expandida
    complejidad_HH_i <- calcular_indices_complejidad(matriz_HH_i)
    pais_HH_i <- complejidad_HH_i %>%
      filter(country == pais) %>%
      mutate(veces_salto = i, metodo = "HH")
    resultados_sHH <- bind_rows(resultados_sHH, pais_HH_i)
    
    # --- Simulación Salto Diferenciado ---
    resultado_sd_i <- simular_saltos_diferenciados(
      net = net_2223_product_ext,
      bi_matrix = bi_2223,
      products_name = products_name,
      distance_pivot = distance_pivot,
      proximity_matrix = prox_2223$proximity_product,
      country = pais,
      cantidad_productos = 10,
      veces_salto = i,
      top_n = 10
    )
    matriz_sd_i <- resultado_sd_i$matriz_expandida
    complejidad_sd_i <- calcular_indices_complejidad(matriz_sd_i)
    pais_sd_i <- complejidad_sd_i %>%
      filter(country == pais) %>%
      mutate(veces_salto = i, metodo = "Differentiated jump")
    resultados_sd <- bind_rows(resultados_sd, pais_sd_i)
  }
}

# Unir resultados
resultados_total <- bind_rows(resultados_sd, resultados_sHH)

# Modificar nombres resultados_total de Salto Diferenciado por 

# Graficar reflejando múltiples países
library(ggplot2)

p_reflections <- ggplot(resultados_total, aes(x = veces_salto, y = reflections_index,
                                              color = country , linetype = metodo)) +
  geom_line(size = 1.5) +
  labs(title = "", x = "Number of jumps", y = "Index (standardized)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16))

p_fitness <- ggplot(resultados_total, aes(x = veces_salto, y = fitness_index,
                                          color = country , linetype = metodo)) +
  geom_line(size = 1.5) +
  labs(title = "", x = "Number of jumps", y = "Index (standardized)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16))

(p_reflections | p_fitness) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

