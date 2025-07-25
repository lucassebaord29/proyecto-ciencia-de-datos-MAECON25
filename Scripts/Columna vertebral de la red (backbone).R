rm (list = ls()) #limpiar environment

# Cargar librerias necesarias para análisis de datos, gráficos y redes

library(tidyverse) # Manipulación y visualización de datos
library(economiccomplexity) # Análisis de complejidad económica
library(igraph) # Creación y manipulación de grafos
library(ggplot2) # Gráficos
library(beepr) # Notificaciones sonoras (opcional)
library(readxl) # Lectura de archivos Excel
library(gridExtra) # Organización de múltiples gráficos
library(ggraph) # Visualización de redes con ggplot2
library(igraph) # Carga repetida (puede omitirse si ya fue cargada)
library(viridis) # Paleta de colores para gráficos

# Establecer directorio de trabajo 
setwd('/Users/lucasordonez/Library/CloudStorage/Dropbox/Proyecto datos')


# importar base de datos principal 

base_final <- read_csv("Data/Primary/base_final.csv")



## Balassa Index ----------


bi_2223 <- balassa_index(base_final,
                         country = "country", 
                         product = "product", 
                         value = "export_value_avg")


## Proximity ------

prox_2223 <- proximity(bi_2223)



# Backbone MST + expansión por umbral de proximidad ####

library(igraph)
library(ggplot2)

# TRUE para activar el armado del backbone, FALSE para no ejecutarlo
if(TRUE){
  # 1) Función para generar MST máximo desde matriz de proximidad
  max_spanning_tree_from_proximity <- function(prox_object) {
    prox_mat <- prox_object$proximity_product
    diag(prox_mat) <- 0
    
    # Crear grafo desde la matriz
    g <- graph_from_adjacency_matrix(prox_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
    
    # Guardar los nombres originales
    original_names <- V(g)$name
    
    # Crear atributo adicional con índice numérico
    V(g)$idx <- 1:nrow(prox_mat)
    
    # Negar pesos para usar mst()
    E(g)$weight <- -E(g)$weight
    mst_neg <- mst(g)
    E(mst_neg)$weight <- -E(mst_neg)$weight
    
    # Mantener el atributo idx en el MST
    V(mst_neg)$idx <- V(g)$idx
    V(mst_neg)$name <- original_names
    
    # Reportar nodos y aristas
    cat("MST - Nodos:", vcount(mst_neg), ", Aristas:", ecount(mst_neg), "\n")
    
    return(mst_neg)
  }
  
  # 2) Función para agregar aristas por encima de un umbral de proximidad
  add_edges_above_threshold <- function(g, prox_mat, threshold) {
    
    # Guardamos la cantidad inicial de aristas del grafo antes de agregar nuevas
    initial_edges <- ecount(g)
    
    # Creamos un vector vacío donde vamos a almacenar los pares de nodos que superen el umbral de proximidad
    add_edges <- c()
    
    # Iteramos sobre todos los pares posibles de nodos del grafo
    for (n1 in V(g)) {
      for (n2 in V(g)) {
        
        # Obtenemos los índices asociados a cada nodo, según el atributo 'idx' que referencia a su posición en la matriz de proximidad
        id1 <- V(g)[n1]$idx
        id2 <- V(g)[n2]$idx
        
        # Evaluamos cada par solo una vez, evitando duplicados y autoconexiones (consideramos solo si id2 > id1)
        if (id2 > id1) {
          
          # Obtenemos el valor de proximidad entre los nodos n1 y n2 desde la matriz de proximidad
          proxi <- prox_mat[id1, id2]
          
          # Si los nodos no están conectados y su proximidad supera el umbral, los agregamos a la lista de nuevas aristas
          # Condición 1:  Verifica que no haya ya una arista entre los nodos n1 y n2 en el grafo. Si ya están conectados, no se hace nada.
          # Condición 2: chequea que el valor de proximidad entre esos dos nodos sea mayor al umbral definido
          if (!are.connected(g, n1, n2) && proxi > threshold) {
            add_edges <- c(add_edges, n1, n2)
          }
        }
      }
    }
    
    # Informamos la cantidad de nuevas aristas detectadas que cumplen con el criterio
    cat("Cantidad de aristas agregadas:", length(add_edges) / 2, "\n")
    
    # Agregamos todas las nuevas aristas encontradas al grafo
    g <- add_edges(g, add_edges)
    
    # Asignamos a cada nueva arista su peso correspondiente según el valor de proximidad en la matriz
    for (i in seq(1, length(add_edges), by = 2)) {
      id1 <- V(g)[add_edges[i]]$idx
      id2 <- V(g)[add_edges[i+1]]$idx
      
      # Localizamos cada nueva arista agregada y le asignamos como peso el valor correspondiente de la matriz de proximidad
      E(g)[add_edges[i] %--% add_edges[i+1]]$weight <- prox_mat[id1, id2]
    }
    
    # Calculamos y mostramos la cantidad final de aristas, y cuántas se agregaron respecto a la versión inicial
    final_edges <- ecount(g)
    added_edges <- final_edges - initial_edges
    
    cat("Red extendida - Nodos:", vcount(g), ", Aristas:", final_edges, ", Nuevas aristas:", added_edges, "\n")
    
    # Devolvemos el grafo resultante con las nuevas aristas incorporadas
    return(g)
  }
  
  
  # 3) Función para resumir características del grafo
  summarize_network <- function(g) {
    node_count <- gorder(g)
    edge_count <- gsize(g)
    avg_degree <- mean(degree(g))
    
    summary_df <- data.frame(
      nodes = node_count,
      edges = edge_count,
      average_degree = round(avg_degree, 2)
    )
    
    return(summary_df)
  }
  
  # 4) Aplicar el algoritmo a cada matriz de proximidad (crear MST)
  net_2223_product <- max_spanning_tree_from_proximity(prox_2223)
  summary_mst <- summarize_network(net_2223_product)
  summary_mst$stage <- "MST"
  
  # 5) Definir umbral de proximidad
  bef_threshold <- 0.56 # se selecciona este valor para cumplir el rule of thumb de grado promedio 4
  
  # 6) Agregar aristas adicionales a cada MST según el umbral
  net_2223_product_ext <- add_edges_above_threshold(net_2223_product, prox_2223$proximity_product, bef_threshold)
  summary_ext <- summarize_network(net_2223_product_ext)
  summary_ext$stage <- "MST + Threshold"
  
  # guardar el grafo extendido '/Users/lucasordonez/Maestría/Ciencia de datos/Proyecto datos'
  
  saveRDS(net_2223_product_ext, 
          file = 'Backbone/net_2223_product_ext.rds')
}

# importar grafo

net_2223_product_ext <- readRDS('Backbone/net_2223_product_ext.rds')


# 7a) Densidad de kernel sobre las proximidades antes del filtrado 
prox_values <- prox_2223$proximity_product[upper.tri(prox_2223$proximity_product)]
kde_plot <- ggplot(data.frame(prox = prox_values), aes(x = prox)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Densidad kernel de proximidades", x = "Proximidad", y = "Densidad") +
  theme_minimal()

print(kde_plot)

# 7b) Densidad de kernel sobre las proximidades antes del filtrado 
prox_values_ext <- E(net_2223_product_ext)$weight

kde_plot_ext <- ggplot(data.frame(prox = prox_values_ext), aes(x = prox)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Densidad kernel de proximidades (después del filtrado)", x = "Proximidad", y = "Densidad") +
  theme_minimal()

print(kde_plot_ext)


# 7c) incorporar ambos plots

library(gridExtra)

grid.arrange(kde_plot, kde_plot_ext, ncol = 2)


# 8) Consolidar resumen en tabla final (va a correr cuando la linea 51 este en TRUE)
summary_all <- rbind(summary_mst, summary_ext)
print(summary_all)
# resultado
# nodes edges average_degree           stage
# 1   816   815           2.00             MST
# 2   816  1611           3.95 MST + Threshold


# plus: generar un dataframe con net_2223_product_ext con los weight

edges <- as.data.frame(get.edgelist(net_2223_product))
edges$weight <- E(net_2223_product_ext)$weight
edges <- edges %>%
  rename(from = V1, to = V2) %>%
  mutate(weight = round(weight, 3))


# Graficar
g_net_2223_product_ext <- ggraph(net_2223_product_ext, 
            layout = "kk") +
  geom_edge_link(color = "grey80", alpha = 0.25) +
  geom_node_point(alpha = 0.8) +  # Usar el atributo de color
  theme_void() + 
  theme(
  )

g_MST <- ggraph(net_2223_product, 
                layout = "kk") +
  geom_edge_link(color = "grey80", alpha = 0.25) +
  geom_node_point(alpha = 0.8) +  # Usar el atributo de color
  theme_void() + 
  theme(
  )


library(ggraph)
library(igraph)
library(ggplot2)

# library(ggraph)
library(igraph)
library(ggplot2)

# Grafo 1: net_2223_product_ext
set.seed(123) # Para reproducibilidad
g_net_2223_product_ext <- ggraph(net_2223_product_ext, layout = "kk") +
  geom_edge_link(color = "grey80", alpha = 0.4) +
  geom_node_point(size = 4.5, color = "#2E8B57", shape = 21, fill = "#2E8B57", stroke = 0.2, alpha = 0.7) +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(family = "Helvetica", size = 12)
  ) 

# Grafo 2: MST net_2223_product
set.seed(123) # Para reproducibilidad
g_MST <- ggraph(net_2223_product, layout = "kk") +
  geom_edge_link(color = "grey80", alpha = 0.4) +
  geom_node_point(size = 4.5, color = "#63B8FF", shape = 21, fill = "#63B8FF", stroke = 0.2, alpha = 0.8) +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(family = "Helvetica", size = 12)
  ) 
