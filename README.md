# Proyecto. Si quieres cambio verdadero pues salta distinto: mapeando saltos y bosques diferenciados en el Espacio-
Producto de Argentina
## Autores. Lucas S. Ordoñez, Eva Landecky y Franco Ímpavido 


Este repositorio contiene un conjunto de scripts en R destinados a explorar la estructura productiva y la complejidad económica de los países mediante el análisis del Espacio-Producto, métricas de red y simulaciones de trayectorias de diversificación. Se utilizan datos de exportaciones, clasificaciones de productos y técnicas de redes complejas para visualizar patrones, identificar trayectorias de desarrollo potencial y evaluar estrategias de salto en el espacio de productos.

---

## 📁 Estructura de los Scripts

| Script | Descripción |
|--------|-------------|
| `Procesamiento de base de datos raw.R` | Carga y preprocesa los datos brutos de comercio internacional para construir la matriz binaria país-producto (RCA). También genera matrices de proximidad y prepara los datos para el análisis de complejidad. |
| `Simulación de saltos diferenciados y saltos H-H.R` | Implementa simulaciones que comparan dos estrategias de diversificación: una orientada a productos diferenciados y otra basada en cercanía según Hausmann-Hidalgo. Evalúa los productos incorporados, su complejidad y características. |
| `Espacio-Producto por país y grupo de ingresos...R` | Genera visualizaciones del espacio-producto coloreado por categorías de ingresos y clasificaciones de producto (diferenciados y clústeres). Se utiliza para comparar estructuras exportadoras según el nivel de desarrollo. |
| `Heatmaps.R` | Construye mapas de calor del espacio-producto y de la canasta exportadora de los países, utilizando clustering espectral para ordenar filas y columnas. |
| `Métricas de la red.R` | Calcula métricas de red (como grado, centralidad, clustering) sobre el espacio-producto. Permite caracterizar la posición estructural de productos en la red. |
| `Bosques diferenciados.R` | Simula trayectorias de desarrollo económico construyendo bosques de productos diferenciados. Muestra cómo un país podría incorporar nuevos productos siguiendo una estrategia estructurada. |
| `Distribución de Kernel de complejidad y diferenciación...R` | Analiza la distribución de la complejidad y diferenciación de productos mediante estimaciones de densidad de kernel, ayudando a visualizar la estructura de la oferta exportable. |
| `Dispersión de productos según distancia, COG y complejidad.R` | Grafica la dispersión de productos no exportados según su distancia al país, su ganancia esperada de complejidad (COG) y su complejidad individual. |
| `Espacio-Producto.R` | Construye el Espacio-Producto a partir de la matriz de proximidad. Permite visualizar la estructura global de conectividad entre productos. |
| `Espacio-Producto por país a partir de un salto...R` | Visualiza cómo cambia la estructura exportadora de un país luego de realizar un salto (diferenciado o H-H). Resalta los productos nuevos y sus conexiones. |
| `Paletas de color (sector, clúster y tecnología).R` | Define paletas de colores personalizadas utilizadas en las visualizaciones según sector, clúster o nivel tecnológico. |
| `Columna vertebral de la red (backbone).R` | Construye la columna vertebral del espacio-producto usando métodos como el Árbol de Expansión Máxima o filtros de proximidad. Reduce la red para enfocarse en los enlaces más relevantes. |
| `Mapa de complejidad de países.R` | Crea mapas mundiales coloreados por el Índice de Complejidad Económica (ECI) de cada país. |
| `Tablas descriptivas de productos por salto.R` | Genera tablas de productos seleccionados en los saltos simulados, incluyendo su complejidad, diferenciación y otras características descriptivas. |

---

## 🧰 Requisitos

- R >= 4.0.0
- Paquetes sugeridos:
  - `tidyverse`
  - `igraph`
  - `ggraph`
  - `pheatmap`
  - `RColorBrewer`
  - `readr`, `dplyr`, `tidyr`
  - `ggplot2`, `ggthemes`
  - `sp`, `sf`, `rnaturalearth`, `rnaturalearthdata`

Instalación:

```r
install.packages(c("tidyverse", "igraph", "ggraph", "pheatmap", "RColorBrewer", 
                   "readr", "dplyr", "tidyr", "ggplot2", "ggthemes", 
                   "sp", "sf", "rnaturalearth", "rnaturalearthdata"))
```

---

## ▶️ Ejecución sugerida

1. `Procesamiento de base de datos raw.R`
2. `Paletas de color...`
3. `Espacio-Producto.R` o `Columna vertebral de la red...`
4. Visualizaciones:
   - `Mapa de complejidad de países.R`
   - `Espacio-Producto por país...`
   - `Heatmaps.R`
   - `Dispersión...` y `Distribución de kernel...`
5. Análisis estructural:
   - `Métricas de la red.R`
   - `Bosques diferenciados.R`
   - `Simulación de saltos...`
   - `Tablas descriptivas...`

---

## 📌 Notas

- El análisis se basa en la literatura del Product Space (Hidalgo & Hausmann).
- Se utiliza la clasificación de productos diferenciados de Rauch y la clasificación de clústeres propuesta por Hausmann et al. (2011).
- La matriz RCA binaria es el insumo principal para construir la red de productos.
