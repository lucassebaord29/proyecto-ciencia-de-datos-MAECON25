rm (list = ls()) #limpiar environment
#Librerías ----

library(igraph)
library(tidyverse)
library(poweRlaw)
library(tidyxl)
library(disparityfilter)
library(concordance)
library(readr)
library(dplyr)
library(readxl)

# setear directorio

setwd('/Users/lucasordonez/Library/CloudStorage/Dropbox/Proyecto datos')





#Cargamos base de datos ----
country_product<-read.csv("Data/Secondary/hs92_country_product_year_4.csv")





# Borramos las columnas irrelevantes
country_product <- country_product %>% 
  select(-import_value,-global_market_share,-export_rca,-distance,-cog,-pci)

country_product$product_hs92_code <- as.character(country_product$product_hs92_code)

# Filtra años 2022 y 2023, agrupa y calcula promedio
country_product <- country_product %>%
  filter(year %in% c(2022, 2023)) %>%
  group_by(country_id, country_iso3_code, product_id, product_hs92_code) %>%
  summarise(export_value_avg = mean(export_value, na.rm = TRUE)) %>%
  ungroup()


# eliminar todas las filas con export_value == 0 

country_product <- country_product %>%
  filter(export_value_avg > 0)


#Hacemos que los códigos tengan 4 digitos
country_product$product_hs92_code <- str_pad(country_product$product_hs92_code, width = 4, side = "left", pad = "0")

#Contabilizamos la cantidad de productos ----
productos<- as.data.frame(unique(country_product$product_hs92_code)) 
#1220 productos

productos <- productos %>% 
  rename(product_hs92_code = "unique(country_product$product_hs92_code)")



# Filtrar códigos inválidos ----
#Aparecen los códigos 9999 y XXXX que aparecen como NA en la librería concordance para hacer la conversión a SITC4

productos_validos <- productos[!productos$product_hs92_code %in% c("9999", "XXXX"), ]
productos_validos <-as.data.frame(productos_validos)
productos_validos <- productos_validos %>% 
  rename(product_hs92_code=productos_validos)


#RAUCH ----

# Usar la función get_proddiff para obtener la categoría de Rauch, especificando el origen hs92=hs0
productos_validos$categoria_rauch <- get_proddiff(productos_validos$product_hs92_code, origin = "HS0")


# Función para obtener la letra con el mayor valor de proportion, manejando NA y casos vacíos
extraer_categoria_max <- function(categoria_df) {
  # Verificar si hay NA en la columna proportion
  if (all(is.na(categoria_df$proportion))) {
    return(NA)  # O puedes devolver un valor por defecto, como "sin valor"
  }
  
  # Extraer la letra con el valor máximo de proportion (ignorando NA)
  max_categoria <- categoria_df$rauch[which.max(categoria_df$proportion)]
  return(max_categoria)
}

# Aplicar la función a la columna "categoria_rauch"
productos_validos$categoria_rauch <- sapply(productos_validos$categoria_rauch, extraer_categoria_max)


sum(is.na(productos_validos$categoria_rauch)) #66

# Filtrar las filas que tienen NA en la columna 'nombre'
productos_rauch_na <- productos_validos[is.na(productos_validos$categoria_rauch), ] #df con los productos na
productos_validos <- productos_validos[!is.na(productos_validos$categoria_rauch), ] #df sin los productos na #940

productos_validos <- productos_validos %>% 
  mutate("extended_rauch_description" = ifelse(categoria_rauch == "w", "Homogeneous goods traded on an organized exchange", 
                                               ifelse(categoria_rauch == "r", "Reference priced", "Differentiated products")))


#CLUSTER y SECTOR DE PRODUCTOS ----
clasificacion_productos<- read.csv("Data/Secondary/product_hs92.csv")
as.character(clasificacion_productos$product_hs92_code)
productos_clasificados <- merge(productos_validos, clasificacion_productos,
                                by='product_hs92_code')


umap_layout <- read.csv("Data/Secondary/umap_layout_hs92.csv")
#Hacemos que los códigos tengan 4 digitos
umap_layout$product_hs92_code <- str_pad(umap_layout$product_hs92_code, width = 4, side = "left", pad = "0")

productos_clasificados_umap <- merge(productos_clasificados, umap_layout,
                                     by= "product_hs92_code") #816 productos

# LALL

#Tenemos la correspondencia para HS96 6 digitos, primero vamos a pasarlo a HS92 4 digitos

lall<- read.csv("Data/Secondary/Lall.csv")
lall <- lall %>% 
  select(-SITC2_4d, -SITC2_4d_description, -HS96_6d_description)

#Hacemos que los códigos tengan 6 digitos
lall$HS96_6d <- str_pad(lall$HS96_6d, width = 6, side = "left", pad = "0")


lall$product_hs92_code<- concord(sourcevar = lall$HS96_6d,
                                 origin = "HS1", destination = "HS0",
                                 dest.digit = 4, all = FALSE)
lall <- lall %>% 
  select(-HS96_6d)

lall_unique <- lall %>% 
  distinct(product_hs92_code, .keep_all = TRUE)

productos_clasificados_lall <- merge(productos_clasificados_umap, lall_unique,
                                     by = "product_hs92_code")

#PBI LEVEL ----

paises_nivel_ingreso <- read_excel('Data/Secondary/paises_por_ingresos.xlsx')
pbi_pc <- read_excel('Data/Secondary/pbi_pc.xlsx')

paises_nivel_ingreso <-paises_nivel_ingreso %>% 
  rename(country_iso3_code = Code)
pbi_pc <-pbi_pc %>% 
  rename(country_iso3_code = `Country Code`)

#Vemos si tenemos disponibilidad de datos para todos los países de la base country_product

paises_cp<- as.data.frame(unique(country_product$country_iso3_code))
paises_cp <- paises_cp %>% 
  rename(country_iso3_code = "unique(country_product$country_iso3_code)")
paises_cp_pbi_pc<- merge(paises_cp, pbi_pc, by= 'country_iso3_code')
paises_cp_nivel_ingreso <- merge(paises_cp, paises_nivel_ingreso, 
                                 by= 'country_iso3_code' )
paises_cp_nivel_ingreso_pbi_pc<-merge(paises_cp_pbi_pc,paises_cp_nivel_ingreso, 
                                      by='country_iso3_code')

filtrado_paises<- read.csv('Data/Secondary/growth_proj_eci_rankings.csv')
filtrado_paises<- as.data.frame(unique(filtrado_paises$country_iso3_code))
filtrado_paises<- filtrado_paises %>% 
  rename(country_iso3_code = 'unique(filtrado_paises$country_iso3_code)')

paises_final <- merge(paises_cp_nivel_ingreso_pbi_pc, filtrado_paises, 
                      by = 'country_iso3_code') #141 países

paises_final <- paises_final %>% 
  select(-`Country Name`)
paises_final <- paises_final %>% 
  rename(country = "Economy")

paises_final <- paises_final %>% 
  filter(!(country %in% c("Iraq", "Chad", "Ethiopia"))) #En la investigación orginal los sacan

#por no ser confiables sus datos


# MERGE ----
#Ya tenemos bases de productos y países todo filtrado y clasificado

#la base general la filtramos x paises
base_final <- merge(country_product, paises_final, by = "country_iso3_code")

#también le agrego producto que sería la tabla final, pero lo hago en una distinta 
#para ver todo por partes
base_final <-merge(base_final, productos_clasificados_lall, by = "product_hs92_code")

#Inclímos el sector feneral de cada producto
sector_productos <- clasificacion_productos %>% 
  select(product_name, product_name_short, product_id_hierarchy) %>% 
  filter(nchar(as.character(clasificacion_productos$product_id_hierarchy)) == 1) #la categoría other no aparece en la tabla filtrada que tenemos
sector_productos <- sector_productos %>% 
  rename(primer_digito_hierarchy = "product_id_hierarchy")
sector_productos <- sector_productos %>% 
  rename(sector_name = "product_name")
sector_productos <- sector_productos %>% 
  rename(sector_name_short = "product_name_short")

#extraemos primer digito de hierarchy
base_final$primer_digito_hierarchy <- substr(as.character(base_final$product_id_hierarchy), 1, 1)

# Unir los data frames usando left_join 
base_final <- base_final %>%
  left_join(sector_productos, by = "primer_digito_hierarchy")

base_final <- base_final %>% 
  select(-product_id.y,-show_feasibility,-natural_resource,-product_parent_id,
         -green_product,-product_id.x, -product_level, -product_id_hierarchy, 
         -primer_digito_hierarchy) #sacamos las variables que quieran

# renombrar la columna product_hs92_code a product

base_final <- base_final %>% 
  rename(product = product_hs92_code)

#limpiamos environment
rm(list = "clasificacion_productos", "filtrado_paises", "lall", "lall_unique", "paises_cp",
   "paises_cp_nivel_ingreso", "paises_cp_nivel_ingreso_pbi_pc", "paises_cp_pbi_pc",
   "paises_nivel_ingreso", "pbi_pc", "productos", "productos_clasificados", "productos_validos")

# Guardar la base final

write_csv(base_final, "Data/Primary/base_final.csv")

