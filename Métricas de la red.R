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
library(univariateML)


setwd("C:/Users/evala/OneDrive - Económicas - UBA/MAECON/DATOS/Proyecto Datos/Codes")

# Importar base de datos principal con información de exportaciones y atributos de productos
base_final <- read_csv("C:/Users/evala/OneDrive - Económicas - UBA/MAECON/DATOS/Proyecto Datos/Data/base_final.csv")

base_metricas <- base_final %>%
  select(country_iso3_code, product, export_value_avg)

base_metricas_char <- base_metricas %>%
  mutate(
    country_iso3_code = as.character(country_iso3_code),
    product = as.character(product),
    export_value_avg = as.character(export_value_avg)
  )


## Calcular el índice de Balassa (RCA) ----------
# Mide si un país tiene ventaja comparativa revelada en un producto dado

bi_2223 <- balassa_index(base_metricas,
                         country = 'country_iso3_code',
                         product = 'product',
                         value = 'export_value_avg')

## Calcular matriz de proximidad entre productos ----------
# Basado en la co-exportación de productos (probabilidad condicional de ventaja compartida)
prox_2223 <- proximity(bi_2223)

# Transformar como dataframe prox_2223


# Armado de red
# Projections ----

net_2223 <- projections(prox_2223$proximity_country, prox_2223$proximity_product)

#Hay dos proyecciones de la red bipartita.
#names(net_2223)
#[1] "network_country" #Espacio-País
#[2] "network_product" #Espacio-Producto

##Métricas de la red

## Orden
gorder(net_2223$network_country)
#138 países
gorder(net_2223$network_product)
#816 productos

##Tamaño
gsize(net_2223$network_country)
# 224
gsize(net_2223$network_product)
# 1817

##Densidad de la red. Es densa o esparsa?

edge_density(net_2223$network_country)
#0.02369618
edge_density(net_2223$network_country)*100
#2.369618

edge_density(net_2223$network_product)
#0.005464333
edge_density(net_2223$network_product)*100
#0.5464333

##Grado de la red

# calculo el grado de los nodos
deg_2223_c <- degree(net_2223$network_country)

deg_2223_p<- degree(net_2223$network_product)

##Grado promedio de la red

average_degree_c<-mean(deg_2223_c)
average_degree_p<-mean(deg_2223_p)

print(average_degree_c)
#3.246377
print(average_degree_p)
#4.453431

##Distribución de grado

# Calcular distribución de grado, graficar con el  -----
# ajuste correspondiente (según hipótesis de distribución esperada)

# Calculo el grado de distribuciòn
dd_2223_c <- degree_distribution(net_2223$network_country)
dd_2223_p <- degree_distribution(net_2223$network_product)

# escalas para el plot 

d_2223_c <- 1:max(deg_2223_c) - 1

d_2223_p <- 1:max(deg_2223_p) - 1

ind_2223_c <- dd_2223_c != 0

ind_2223_p <- dd_2223_p != 0

# Grafico la distribuciòn empirica 2223 y ajuste de distribución-PAISES
par(mfrow = c(1, 2))

##Distribución empírica
plot(d_2223_c[ind_2223_c], dd_2223_c[ind_2223_c], pch = 16,
     xlab = "Degree", ylab = "Log-Frequency",
     col = "blue")

##Ajustes de distribución
plot(d_2223_c[ind_2223_c], dd_2223_c[ind_2223_c], pch = 16,
     log = "xy",  # para que esté en escala log-log
     xlab = "Log-Degree", ylab = "Log-Frequency",
     col = "blue")
# Dibujar las distribuciones teóricas ajustadas
lines(mlpois(deg_2223_c), lwd = 2, lty = 1, col = "#FF82AB")   # Poisson
lines(mllnorm(deg_2223_c), lwd = 2, lty = 2, col = "red")    # Log-normal
lines(mlpower(deg_2223_c), lwd = 2, lty = 3, col = "purple") # Power law

#Agregar leyenda
legend("topright", 
       legend = c("Poisson", "Log-normal", "Power-law"),
       col = c("#FF82AB", "red", "purple"),
       lty = c(1, 2, 3),
       lwd = c(3, 2, 2),
       bty = "n")  # "n" para no dibujar caja

# Grafico la distribuciòn empirica 2223 y ajuste de distribución-PRODUCTOS
par(mfrow = c(1, 2))

##Distribución empírica
plot(d_2223_p[ind_2223_p], dd_2223_p[ind_2223_p], pch = 16,
     xlab = "Degree", ylab = "Log-Frequency",
     col = "blue")

##Ajustes de distribución
plot(d_2223_p[ind_2223_p], dd_2223_p[ind_2223_p], pch = 16,
     log = "xy",  # para que esté en escala log-log
     xlab = "Log-Degree", ylab = "Log-Frequency",
     col = "blue")

# Dibujar las distribuciones teóricas ajustadas
lines(mlpois(deg_2223_p), lwd = 2, lty = 1, col = "#FF82AB")   # Poisson
lines(mllnorm(deg_2223_p), lwd = 2, lty = 2, col = "red")    # Log-normal
lines(mlpower(deg_2223_p), lwd = 2, lty = 3, col = "purple") # Power law

# Agregar la leyenda
legend("topright", 
       legend = c("Poisson", "Log-normal", "Power-law"),
       col = c("#FF82AB", "red", "purple"),
       lty = c(1, 2, 3),
       lwd = c(3, 2, 2),
       bty = "n")  # "n" para no dibujar caja


##Diámetro de la red
#El diámetro es el camino más corto más largo en la red.
diameter(net_2223$network_country)
#9.094508
diameter(net_2223$network_product)
# 15.05186

##Coeficiente de clustering medio
transitivity(net_2223$network_country, type = "average")
# 0.2044094
transitivity(net_2223$network_product, type = "average")
#0.2347742

##Coeficientes global de clustering
transitivity(net_2223$network_country, type = "global")
# 0.4168937
transitivity(net_2223$network_product, type = "global")
#0.4301541


# [1] 0.9959329
#Estructura de red muy densa. Cuando no había borrado los pares pais-sector que tenían valor 0, daba completamente densa y conectada.
#De todos modos está casi completamente conectada


#Average path length
mean_distance(net_2223$network_country)
#3.38587
mean_distance(net_2223$network_product)
#4.597516





# Métricas Globales ----------------------------------------------------------

## is it a connected graph?
components(net_2223$network_country)$no
# 1
# Está conectada. Podemos usar el grafo entero
components(net_2223$network_product)$no
# 1
# Está conectada. Podemos usar el grafo entero

## check for loops and hyper-edges
any_multiple(net_2223$network_country)
# FALSE
any_multiple(net_2223$network_product)
# FALSE

any_loop(net_2223$network_country)
# FALSE
any_loop(net_2223$network_product)
# FALSE

# Medida de betweenness
betweenness_centrality_c <- betweenness(net_2223$network_country)

betweenness_centrality_p <- betweenness(net_2223$network_product)

# Medida de closeness
closeness_centrality_c <- closeness(net_2223$network_country)

closeness_centrality_p <- closeness(net_2223$network_product)

# Medida de eigenvector
eigenvector_centrality_c <- eigen_centrality(net_2223$network_country)$vector

eigenvector_centrality_p <- eigen_centrality(net_2223$network_product)$vector

# 2. Computar métricas de centralidad (p.ej.: closeness, betweenness, ####
# eigenvector, pagerank). LAS QUE CONSIDEREN RELEVANTES. Pensar la 
# interpretación de cada métrica.


#Se miden betweenness and closenees para ver la centralidad de los nodos y links basados en el camino más fuerte
#El camino más fuertes (strongest path) explica la supply chain más importanate entre dos sectores

betweenness_sp_c <- betweenness(net_2223$network_country,
                                directed = TRUE, # Assuming your product network is directed
                                weights = 1 / E(net_2223$network_country)$weight, # Using edge weights
                                normalized = TRUE)


betweenness_sp_p <- betweenness(net_2223$network_product,
                              directed = TRUE, # Assuming your product network is directed
                              weights = 1 / E(net_2223$network_product)$weight, # Using edge weights
                              normalized = TRUE)

#Top ten  by SP betweenness

order_betweenness_sp_c <- order(betweenness_sp_c, decreasing = TRUE)
order_betweenness_sp_p <- order(betweenness_sp_p, decreasing = TRUE)

order_countries <- names(betweenness_sp_c)[order_betweenness_sp_c]
order_product <- names(betweenness_sp_p)[order_betweenness_sp_p]

top_10_betweenness_sp_c <- head(order_betweenness_sp_c, 10)
top_10_betweenness_sp_countries <- head(order_countries, 10)

top_10_betweenness_sp_p <- head(order_betweenness_sp_p, 10)
top_10_betweenness_sp_product <- head(order_product, 10)


top_10_betweenness_sp_c <- data.frame(
  Pais = top_10_betweenness_sp_countries,
  Betweenness_ShortestPath = top_10_betweenness_sp_c
)
print(top_10_betweenness_sp_c)
#Pais Betweenness_ShortestPath
#1   TZA                      129
#2   KEN                       66
#3   ALB                        3
#4   TUN                      127
#5   UGA                      130
#6   BIH                       16
#7   RWA                      111
#8   TGO                      122
#9   BEN                       11
#10  HRV                       53

top_10_betweenness_sp_p <- data.frame(
  Producto = top_10_betweenness_sp_product,
  Betweenness_ShortestPath = top_10_betweenness_sp_p
)
print(top_10_betweenness_sp_p)

#Producto Betweenness_ShortestPath
#1      7326                      552
#2      9401                      796
#3      6403                      466
#4      3908                      299
#5      2920                      218
#6      3914                      305
#7      9030                      783
#8      8443                      649
#9      8432                      639
#10     8483                      685

#Downstream Closeness
closeness_downstream_sp_c <- closeness(net_2223$network_country, mode = "in",  weights = 1/E(net_2223$network_country)$weight)
closeness_downstream_sp_p <- closeness(net_2223$network_product, mode = "in",  weights = 1/E(net_2223$network_product)$weight)

#Top ten by downstream closeness

order_closeness_downstream_sp_c <- order(closeness_downstream_sp_c, decreasing = TRUE)
order_closeness_downstream_sp_p <- order(closeness_downstream_sp_p, decreasing = TRUE)

order_countries_c_ds <- names(closeness_downstream_sp_c)[order_closeness_downstream_sp_c]
order_products_c_ds <- names(closeness_downstream_sp_p)[order_closeness_downstream_sp_p]

top_10_closeness_downstream_sp_c <- head(order_closeness_downstream_sp_c, 10)
top_10_closeness_downstream_sp_countries <- head(order_countries_c_ds, 10)

top_10_closeness_downstream_sp_p <- head(order_closeness_downstream_sp_p, 10)
top_10_closeness_downstream_sp_product <- head(order_products_c_ds, 10)


top_10_closeness_downstream_sp_c <- data.frame(
  Pais = top_10_closeness_downstream_sp_countries,
  Closeness_Downstream_ShortestPath = top_10_closeness_downstream_sp_c
)
print(top_10_closeness_downstream_sp_c)

#Pais Closeness_Downstream_ShortestPath
#1   KEN                                66
#2   TUN                               127
#3   TZA                               129
#4   ALB                                 3
#5   GTM                                50
#6   SLV                               116
#7   MAR                                78
#8   DOM                                35
#9   UGA                               130
#10  BIH                                16

top_10_closeness_downstream_sp_p <- data.frame(
  Producto = top_10_closeness_downstream_sp_product,
  Closeness_Downstream_ShortestPath = top_10_closeness_downstream_sp_p
)
print(top_10_closeness_downstream_sp_p)

#Producto Closeness_Downstream_ShortestPath
#1      7326                               552
#2      4902                               373
#3      4005                               320
#4      8483                               685
#5      8419                               627
#6      8432                               639
#7      4016                               328
#8      8501                               688
#9      7616                               579
#10     8708                               740
 

#Upstream Closeness
closeness_upstream_sp_c <- closeness(net_2223$network_country, mode = "out", weights = 1/E(net_2223$network_country)$weight)
closeness_upstream_sp_p <- closeness(net_2223$network_product, mode = "out", weights = 1/E(net_2223$network_product)$weight)

#Top ten by upstream closeness
order_closeness_upstream_sp_c <- order(closeness_upstream_sp_c, decreasing = TRUE)
order_closeness_upstream_sp_p <- order(closeness_upstream_sp_p, decreasing = TRUE)

order_paises_c_us <- names(closeness_upstream_sp_c)[order_closeness_upstream_sp_c]
order_productos_c_us <- names(closeness_upstream_sp_p)[order_closeness_upstream_sp_p]

top_10_closeness_upstream_sp_c <- head(order_closeness_upstream_sp_c, 10)
top_10_closeness_upstream_sp_countries <- head(order_paises_c_us, 10)

top_10_closeness_upstream_sp_p <- head(order_closeness_upstream_sp_p, 10)
top_10_closeness_upstream_sp_productos <- head(order_productos_c_us, 10)


top_10_closeness_upstream_sp_countries <- data.frame(
  Pais = top_10_closeness_upstream_sp_countries,
  Closeness_Upstream_ShortestPath = top_10_closeness_upstream_sp_c
)
print(top_10_closeness_upstream_sp_countries)

#Pais Closeness_Upstream_ShortestPath
#1   KEN                              66
#2   TUN                             127
#3   TZA                             129
#4   ALB                               3
#5   GTM                              50
#6   SLV                             116
#7   MAR                              78
#8   DOM                              35
#9   UGA                             130
#10  BIH                              16

top_10_closeness_upstream_sp_p <- data.frame(
  Productos = top_10_closeness_upstream_sp_productos,
  Closeness_Upstream_ShortestPath = top_10_closeness_upstream_sp_p
)
print(top_10_closeness_upstream_sp_p)
#  Productos Closeness_Upstream_ShortestPath
#1       7326                             552
#2       4902                             373
#3       4005                             320
#4       8483                             685
#5       8419                             627
#6       8432                             639
#7       4016                             328
#8       8501                             688
#9       7616                             579
#10      8708                             740
