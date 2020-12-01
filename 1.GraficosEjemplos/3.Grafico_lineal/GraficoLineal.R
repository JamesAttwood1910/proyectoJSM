# Grafico lineal 

# En este ejemplo vamos a trazar un grafico de lineas para mostrar como la poblacion inmigrante ha cambiado para 
# distintos grupos nacionales. 

# Paquetes
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)

# Remover numeros scientificos 

options(scipen=999)

# Ejemplo base de datos
b <- data_frame(Año = c(2010, 2015, 2017, 2019), 
                Bolivia = c(30000, 35000, 60000, 80000),
                Argentina = c(45621, 23456, 345432, 28347), 
                Peru = c(20000, 15000,24000, 45000))


# Opcion 1 
b %>%   ggplot() +
  geom_line(aes(x=Año, y=Bolivia), color = "blue") + 
  geom_line(aes(x = Año, y = Argentina), color = "green") +
  ggtitle("Title") +
  ylab("Pais") + theme(axis.text.y = element_text())


# Opcion 2 (mejor opcion)
b %>% gather(key =  "Pais", value = "Pop", -Año) %>% 
  ggplot() + 
  geom_line(aes(x = Año, y = Pop, linetype = Pais)) + 
  scale_y_continuous(labels = comma_format(big.mark = ","))+
  scale_x_continuous(breaks = c(2010,2015,2017, 2019))

