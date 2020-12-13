# Tabla 

# En este ejemplo vamos a usar el paquete gt para crear una tabla. 

# https://gt.rstudio.com/ 

# Cargar paquetes 
library(gt)
library(dplyr)

#Crear base de datos 
test <- tibble(Año = c(1970, 1980, 1990, 1995, 2000, 2005, 2010, 2015, 2019),
                   Cantidad = c(84, 102, 153, 161, 174, 192, 221, 249, 272), 
                   Percentaje = c(2.3, 2.2, 2.9, 2.8, 2.8, 2.9, 3.2, 3.4, 3.5))

#Crear tabla 
test %>% gt() %>% 
  
  tab_header(
    title = "Tabla 2.1.1. Cantidad y porcentaje de poblacion migrante
                 a nivel global 1970 - 2019"
  ) %>%
  
  cols_label(Año = md("**Año**"),
             Cantidad = md("**Cantidad**"),  #negrtia 
             Percentaje = md("**Percentaje**") #cursiva 
             
  ) %>% 
  
  tab_style(
    style = list(
      cell_fill(color = "#171345", alpha = 1),
      cell_text(color = "White", style = "oblique")),
    locations = list(
      cells_body(columns = 1),
      cells_title(groups = c("title"))
    )
    
  ) %>% 
  
  tab_style(
    style = list(
      cell_fill(color = "#c9e7ea", alpha = 1)
    ),
    locations = cells_body(columns = 3)
  ) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#eea6c0", alpha = 0.5)
    ),
    locations =
      cells_body(columns = 2)
  ) %>%
  
  tab_spanner(label = "Data", columns = c(2:3)) %>%
  
  tab_style(
    locations = cells_column_spanners("Data"),
    style = list(
      cell_fill(color = "#171345", alpha = 1),
      cell_text(color = "white"))
    
  )  %>%
  
  tab_style(
    locations = cells_column_labels(c("Año", "Cantidad", "Percentaje")),
    style = list(cell_fill(color = "#991334"),
                 cell_text(color = "White"))
  )


991334
# Colores 
library(RColorBrewer)
display.brewer.all()
palette(brewer.pal(n = 8, name = "RdBu")) # sets a color scheme to the palette. 
palette()

"#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#D1E5F0" "#92C5DE" "#4393C3" "#2166AC"


testcolors <- c("#c57aa3", "#eea6c0", 	"#c9e7ea", "#171345")

https://www.color-hex.com/color-palette/101420 

https://html-color-codes.info/colors-from-image/ 




