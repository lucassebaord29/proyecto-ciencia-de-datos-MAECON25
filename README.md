# Proyecto de Ciencia de Datos MAECON2025

**Título del proyecto**: Si quieres cambio verdadero, pues salta distinto: Mapeando saltos y bosques diferenciados en el Espacio-Producto de Argentina  

**Autores**: Lucas S. Ordoñez, Eva Landecky y Franco Ímpavido  

**Docentes**: Viktoriya Semeshenko, Sergio A. De Raco

Este repositorio contiene un conjunto de scripts en R destinados a explorar la estructura productiva y la complejidad económica de los países mediante el análisis del Espacio-Producto, métricas de red y simulaciones de trayectorias de diversificación. Se utilizan datos de exportaciones, clasificaciones de productos y técnicas de redes complejas para visualizar patrones, identificar trayectorias de desarrollo potencial y evaluar estrategias de salto en el espacio de productos.

---

## 📁 Estructura de los Scripts

| Script | Descripción |
|--------|-------------|
| `Procesamiento de base de datos raw.R` | Carga y preprocesa los datos de comercio internacional de *The Atlas of Economic Complexity*. Construye la matriz binaria país-producto (RCA) y genera matrices de proximidad. |
| `Simulación de saltos diferenciados y saltos H-H.R` | Simula dos estrategias de diversificación: hacia productos diferenciados y hacia productos cercanos (Hausmann-Hidalgo). Evalúa productos incorporados, su complejidad y naturaleza. |
| `Espacio-Producto por país y grupo de ingresos...R` | Visualiza el Espacio-Producto coloreado por grupos de ingreso y categorías de producto. Compara estructuras exportadoras entre países. |
| `Heatmaps.R` | Construye mapas de calor del Espacio-Producto y de las canastas exportadoras, con clustering jerárquico aglomerativo. |
| `Métricas de la red.R` | Calcula métricas de red (grado, centralidad, clustering, etc.) para productos y países. Caracteriza su posición estructural. |
| `Bosques diferenciados.R` | Simula trayectorias de desarrollo económico mediante la construcción de bosques de productos diferenciados. |
| `Distribución de Kernel de complejidad y diferenciación...R` | Analiza la distribución de complejidad y diferenciación de productos mediante estimaciones de densidad de kernel. |
| `Dispersión de productos según distancia, COG y complejidad.R` | Grafica productos exportados según distancia, ganancia esperada de complejidad (COG) y complejidad económica. |
| `Espacio-Producto.R` | Construye y visualiza el Espacio-Producto a partir de la matriz de proximidad. |
| `Espacio-Producto por país a partir de un salto...R` | Muestra el cambio estructural de un país tras realizar un salto exportador (diferenciado o H-H). |
| `Paletas de color (sector, clúster y tecnología).R` | Define paletas de colores utilizadas en las visualizaciones según sector, clúster o tecnología. |
| `Columna vertebral de la red (backbone).R` | Reduce la red usando un Árbol de Expansión Máxima o umbral de proximidad. Identifica enlaces estructuralmente relevantes. |
| `Mapa de complejidad de países.R` | Genera mapas mundiales coloreados por el Índice de Complejidad Económica (ECI). |
| `Tablas descriptivas de productos por salto.R` | Tablas de productos seleccionados en saltos simulados, incluyendo características como complejidad y diferenciación. |

---

## 🧰 Requisitos

- **Versión mínima de R**: 4.0.0
- **Paquetes sugeridos**:

  - **Manipulación y visualización de datos**:  
    `tidyverse`, `readr`, `dplyr`, `tidyr`, `ggplot2`, `ggthemes`
  - **Análisis de redes**:  
    `igraph`, `ggraph`, `RColorBrewer`, `viridis`, `patchwork`, `gridExtra`
  - **Visualización de datos**:  
    `pheatmap`
  - **Complejidad económica**:  
    `economiccomplexity`
  - **Datos espaciales y mapas**:  
    `sp`, `sf`, `rnaturalearth`, `rnaturalearthdata`
  - **Otros**:  
    `readxl`, `beepr`, `rlang`

---

### 🔧 Instalación

Ejecutá este bloque en la consola de R para instalar todos los paquetes necesarios:

```r
install.packages(c(
  "tidyverse", "igraph", "ggraph", "pheatmap", "RColorBrewer", 
  "readr", "dplyr", "tidyr", "ggplot2", "ggthemes", 
  "sp", "sf", "rnaturalearth", "rnaturalearthdata",
  "economiccomplexity", "readxl", "beepr", "gridExtra", 
  "viridis", "patchwork", "rlang"
))
```

---

## ▶️ Ejecución sugerida

1. `Procesamiento de base de datos raw.R
2. `Columna vertebral de la red...` 
3. `Paletas de color...`  
3. `Espacio-Producto.R`  
4. Visualizaciones:
   - `Heatmaps.R` 
   - `Mapa de complejidad de países.R`  
   - `Espacio-Producto por país...`  
   - `Dispersión...` y `Distribución de kernel...`  
6. Análisis estructural:  
   - `Métricas de la red.R`  
   - `Bosques diferenciados.R`  
   - `Simulación de saltos...`  
   - `Tablas descriptivas...`

---

