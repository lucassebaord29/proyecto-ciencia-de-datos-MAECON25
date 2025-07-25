# Función para asignar colores por sector
assign_colors_by_sector <- function(net) {
  
  # Mapeo de colores para cada sector
  sector_colors <- c(
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
  
  # Obtener los datos de los nodos
  node_data <- as_data_frame(net, what = "vertices")
  
  # Verificar si la variable 'sector_name_short' existe en los nodos
  if (!("sector_name_short" %in% colnames(node_data))) {
    stop("La variable 'sector_name_short' no existe en los nodos del grafo.")
  }
  
  # Obtener los valores de 'sector_name_short' de los nodos
  sector_values <- node_data$sector_name_short
  
  # Asignar colores a los nodos según los valores de 'sector_name_short'
  V(net)$sector_color <- sector_colors[sector_values]
  
  # Devolver el grafo con el atributo 'sector_color' asignado
  return(net)
}

# Función para asignar colores por clúster
assign_colors_by_cluster <- function(net) {
  
  # Mapeo de colores para cada clúster
  cluster_colors <- c(
    "Agriculture" = "#1B9E77",
    "Minerals" =  "#E6AB02",
    "Industrial Chemicals and Metals" = "#E7298A",
    "Construction, Building, and Home Supplies" =  "#D95F02",
    "Metalworking and Electrical Machinery and Parts" = "#66A61E",
    "Textile Apparel and Accessories" = "#666666",
    "Electronic and Electrical Goods" = "#7570B3",
    "Textile and Home Goods" = "#A6761D"
  )
  
  # Obtener los datos de los nodos
  node_data <- as_data_frame(net, what = "vertices")
  
  # Verificar si la variable 'product_space_cluster_name' existe en los nodos
  if (!("product_space_cluster_name" %in% colnames(node_data))) {
    stop("La variable 'product_space_cluster_name' no existe en los nodos del grafo.")
  }
  
  # Obtener los valores de 'product_space_cluster_name' de los nodos
  cluster_values <- node_data$product_space_cluster_name
  
  # Asignar colores a los nodos según los valores de 'product_space_cluster_name'
  V(net)$cluster_color <- cluster_colors[cluster_values]
  
  # Devolver el grafo con el atributo 'cluster_color' asignado
  return(net)
}

# Función para asignar colores por categoría tecnológica
assign_colors_by_technology_category <- function(net) {
  
  # Mapeo de colores para cada categoría tecnológica
  technological_category_colors <- c(
    "Primary products" =   "#7FA718"   ,
    "Resource-based" = "#BF8B12", 
    "Medium technology" =  "#D03792" ,
    "Unclassified products" = "#666666", 
    "High technology" = "#1B9E77",
    "Low technology" =  "#B16548"
  )
  
  # Obtener los datos de los nodos
  node_data <- as_data_frame(net, what = "vertices")
  
  # Verificar si la variable 'technological_categories' existe en los nodos
  if (!("technological_categories" %in% colnames(node_data))) {
    stop("La variable 'technological_categories' no existe en los nodos del grafo.")
  }
  
  # Obtener los valores de 'technological_categories' de los nodos
  tech_category_values <- node_data$technological_categories
  
  # Asignar colores a los nodos según los valores de 'technological_categories'
  V(net)$tech_category_color <- technological_category_colors[tech_category_values]
  
  # Devolver el grafo con el atributo 'tech_category_color' asignado
  return(net)
}


# Función parq asigna color por catergoría de diferenciación
assign_colors_by_differentiation_category <- function(net) {
  
  # Mapeo de colores para cada categoría de diferenciación
  differentiated_colors <- c(
    "Differentiated product" = "#1B9E77", 
    "Undifferentiated product" = "#666666"
  )
  
  # Obtener los datos de los nodos
  node_data <- as_data_frame(net, what = "vertices")
  
  # Verificar si la variable 'es_diferenciado' existe en los nodos
  if (!("es_diferenciado" %in% colnames(node_data))) {
    stop("La variable 'es_diferenciado' no existe en los nodos del grafo.")
  }
  
  # Obtener los valores de 'es_diferenciado' de los nodos
  differentiation_values <- node_data$es_diferenciado
  
  # Asignar colores a los nodos según los valores de 'es_diferenciado'
  V(net)$differentiation_color <- differentiated_colors[differentiation_values]
  
  # Devolver el grafo con el atributo 'differentiation_color' asignado
  return(net)
}

