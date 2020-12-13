#### Grafico circular

"Este guion reproduce el grafico circular que se expone en pagina 7 del informe 2019. Muestra migrantes en 2019 
segun continente que inhabitan".

library(ggplot2)
library(dplyr)
library(RColorBrewer)


datos <- data.frame(Country = c("Asia", "Europe", "North America",
                              "Africa", "Latin America", "Oceana"),
                  values = c(31, 30, 22, 10, 4, 3))

datos <- as_tibble(datos)

# Computar porcentajes / calculate percentages
datos$fraction = datos$values / sum(datos$values)

# Calcula porcentajes cumulativos / Compute the cumulative percentages (top of each rectangle)
datos$ymax <- cumsum(datos$fraction)

# Calcular el fondo de cada rectangulo / Compute the bottom of each rectangle 
datos$ymin <- c(0, head(datos$ymax, n=-1))

datos$labelPoisition <- (datos$ymax + datos$ymin) / 2


#Agregar etiqueta / add labels
datos$label <- paste0(datos$Country, " \n value: "
                    , datos$values, "%")



# Trazar el grafico / Make the plot

ggplot(datos, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Country)) +
  geom_rect() +
  coord_polar(theta = "y" ) +
  geom_label( data = datos[-5,], x=4.2, aes(y=labelPoisition, label=label), size=2.5) +
  geom_label( data = datos[5,], x=2, aes(y=labelPoisition, label=label), size=2.5) +
  xlim(c(1.5, 5)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(caption = "Test Caption") + 
  ggtitle("Test title")


# Colores y diseño / Colors and design

palette()
library(RColorBrewer)
display.brewer.all() 
palette(brewer.pal(n = 8, name = "Pastel1")) # sets a color scheme to the palette. 

ggplot(datos, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Country)) +
  geom_rect() +
  coord_polar(theta = "y" ) +
  geom_label( data = datos[-5,], x=4.2, aes(y=labelPoisition, label=label), size=2.5) +
  geom_label( data = datos[5,], x=2, aes(y=labelPoisition, label=label), size=2.5) +
  xlim(c(1.5, 5)) +
  scale_fill_brewer(palette = "Pastel1") +  
  theme_void() +
  theme(legend.position = "none") +
  labs(caption = "Test Caption") + 
  ggtitle("Test title")


# Elegir Colores de forma manual / Choose colors manually

testcolors <- c("#FFEBBC", "#004270", "#EF3D14", "#565756", "#16C3CE", "#FFB900")

ggplot(datos, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Country)) +
  geom_rect() +
  coord_polar(theta = "y" ) +
  geom_label( data = datos[-5,], x=4.5, aes(y=labelPoisition, label=label), size=2.5, alpha = 0.6) +
  geom_label( data = datos[5,], x=2, aes(y=labelPoisition, label=label), size=2.5, alpha = 0.6) +
  xlim(c(1.5, 5)) +
  scale_fill_manual(values = testcolors) +
  theme_void() +
  theme(legend.position = "none") +
  labs(caption = "Fuente: XXXX") + 
  ggtitle("Ejemplo grafico 2.1 - Continente de orgien") 

# https://htmlcolorcodes.com/ 
# https://imagecolorpicker.com/en/ 


