# Proyecto. Si quieres cambio verdadero pues salta distinto: mapeando saltos y bosques diferenciados en el Espacio-Producto de Argentina
## Autores. Lucas S. Ordoñez, Eva Landecky y Franco Ímpavido 


Este repositorio contiene un conjunto de scripts en R destinados a explorar la estructura productiva y la complejidad económica de los países mediante el análisis del Espacio-Producto, métricas de red y simulaciones de trayectorias de diversificación. Se utilizan datos de exportaciones, clasificaciones de productos y técnicas de redes complejas para visualizar patrones, identificar trayectorias de desarrollo potencial y evaluar estrategias de salto en el espacio de productos.

---

## 📁 Estructura de los Scripts

| Script | Descripción |
|--------|-------------|
| `Procesamiento de base de datos raw.R` | Carga y preprocesa los datos de comercio internacional de THE ATLAS OF ECONOMIC COMPLEXITY para construir la matriz binaria país-producto (RCA). También genera matrices de proximidad y prepara los datos para el análisis de redes y complejidad. |
| `Simulación de saltos diferenciados y saltos H-H.R` | Implementa simulaciones que comparan dos estrategias de diversificación: una orientada a productos diferenciados y otra basada en cercanía según Hausmann-Hidalgo. Evalúa los productos incorporados, su complejidad y características. |
| `Espacio-Producto por país y grupo de ingresos...R` | Genera visualizaciones del Espacio-Producto coloreado por categorías de ingresos de los países y clasificaciones de producto (diferenciados y sectores). Se utiliza para comparar estructuras exportadoras. |
| `Heatmaps.R` | Construye mapas de calor del Espacio-Producto y de la canasta exportadora de los países, utilizando clustering jerárquico aglomerativo. |
| `Métricas de la red.R` | Calcula métricas de red (como grado, centralidad, clustering) sobre el Espacio-Producto y Espacio-País. Permite caracterizar la posición estructural de los productos y países en la red. |
| `Bosques diferenciados.R` | Simula trayectorias de desarrollo económico construyendo bosques de productos diferenciados. Muestra cómo un país podría incorporar nuevos productos siguiendo una estrategia de saltos diferenciados. |
| `Distribución de Kernel de complejidad y diferenciación...R` | Analiza la distribución de la complejidad y diferenciación de productos mediante estimaciones de densidad de kernel. |
| `Dispersión de productos según distancia, COG y complejidad.R` | Grafica la dispersión de productos  exportados según la medida de distancia, su ganancia esperada de complejidad (COG) y su complejidad económica. |
| `Espacio-Producto.R` | Construye el Espacio-Producto a partir de la matriz de proximidad. Permite visualizar el Espacio-Producto con diferentes estilos. |
| `Espacio-Producto por país a partir de un salto...R` | Visualiza cómo cambia la estructura exportadora de un país luego de realizar un salto (diferenciado o H-H). Resalta los productos nuevos y sus conexiones. |
| `Paletas de color (sector, clúster y tecnología).R` | Define paletas de colores personalizadas utilizadas en las visualizaciones según sector, clúster o nivel tecnológico. |
| `Columna vertebral de la red (backbone).R` | Construye la columna vertebral del espacio-producto usando métodos como el Árbol de Expansión Máxima y umbral de proximidad. Reduce la red para enfocarse en los enlaces más relevantes. |
| `Mapa de complejidad de países.R` | Crea mapas mundiales coloreados por el Índice de Complejidad Económica (ECI) de cada país. |
| `Tablas descriptivas de productos por salto.R` | Genera tablas de productos seleccionados en los saltos simulados, incluyendo su complejidad, diferenciación y otras características descriptivas. |

---
## 🧰 Requisitos

- **Versión mínima de R**: 4.0.0
- **Paquetes sugeridos**:

  - Manipulación y visualización de datos:
    - `tidyverse`
    - `readr`, `dplyr`, `tidyr`
    - `ggplot2`, `ggthemes`
  - Análisis de redes:
    - `igraph`, `ggraph`
    - `RColorBrewer`, `viridis`, `patchwork`, `gridExtra`
  - Visualización de datos:
    - `pheatmap`
  - Análisis de complejidad económica:
    - `economiccomplexity`
  - Datos espaciales y mapas:
    - `sp`, `sf`, `rnaturalearth`, `rnaturalearthdata`
  - Otros:
    - `readxl`, `beepr`, `rlang`

---

### 🔧 Instalación

Para instalar todos los paquetes necesarios, ejecutá el siguiente código en tu consola de R:

```r
install.packages(c(
  "tidyverse", "igraph", "ggraph", "pheatmap", "RColorBrewer", 
  "readr", "dplyr", "tidyr", "ggplot2", "ggthemes", 
  "sp", "sf", "rnaturalearth", "rnaturalearthdata",
  "economiccomplexity", "readxl", "beepr", "gridExtra", 
  "viridis", "patchwork", "rlang"
))

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
