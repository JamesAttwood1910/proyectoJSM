# En este documento se expone como crear un grafico de linea para mostrar la cantidad de inmigrantes de 
# diferentes nacionalidades. Los datos se pueden descargar en https://www.extranjeria.gob.cl/media/2020/03/visas_otorgadas_2019.xlsx 

# This document shows how to create a line graph for the arrival of immigrants from different countries to Chile throughout 2019. 
# The data can be downloaded from https://www.extranjeria.gob.cl/media/2020/03/visas_otorgadas_2019.xlsx 


# Paquetes / Packages

library(readxl)
library(scales)
library(ggplot2)
library(dplyr)

# Datos / data

visas_otorgadas_2019 <- read_excel("2. Datos/visas_otorgadas_2019.xlsx")

# Cambiar sistema a espanol / change system to spanish

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# Manipulación de los datos / manipulating the data

head(visas_otorgadas_2019$MES) 

f <- visas_otorgadas_2019 %>% mutate(test = case_when(MES == "Jan" ~ "1 Enero, 2019",
                                                      MES == "Feb" ~ "1 Febrero, 2019",
                                                      MES == "Mar" ~ "1 Marzo, 2019",
                                                      MES == "Apr" ~ "1 Abril, 2019",
                                                      MES == "May" ~ "1 Mayo, 2019",
                                                      MES == "Jun" ~ "1 Junio, 2019",
                                                      MES == "Jul" ~ "1 Julio, 2019",
                                                      MES == "Aug" ~ "1 Agosto, 2019",
                                                      MES == "Sep" ~ "1 Septiembre, 2019",
                                                      MES == "Oct" ~ "1 Octubre, 2019",
                                                      MES == "Nov" ~ "1 Noviembre, 2019",
                                                      TRUE ~ "1 Diciembre, 2019")) 


# variable para la fecha / date variable

f$test <- as.Date(f$test, format = "%d %B, %Y")
f$test

# Manipulación de los datos adicional y trazar grafico / additional data manipulation and plot graph

head(f)

f.2 <- f %>% select(PAÍS, MES, test) %>% group_by(PAÍS, test) %>% count(PAÍS) %>% 
  
  mutate(País_Agrupado = case_when(PAÍS == "Venezuela" ~ "Venezuela",
                                   PAÍS == "Haití" ~ "Haití",
                                   PAÍS == "Colombia" ~ "Colombia",
                                   PAÍS == "Perú" ~ "Perú",
                                   PAÍS == "Bolivia" ~ "Bolivia",
                                   TRUE ~ "Otro")) 

aggregate(n ~ País_Agrupado + test, data = f.2, "sum")  %>% ggplot() + 
  
  geom_line(aes(x = test, y = n)) + 
  
  facet_wrap(vars(País_Agrupado)) +
  
  theme_minimal() +
  
  scale_x_date(date_breaks = "3 month", date_labels = "%B") + 
  
  scale_y_continuous(n.breaks = 6, labels = comma_format(big.mark = ",")) +
  
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1)) + 
  
  labs(title = "Llegada mensual de inmigrantes en 2019",caption = "Fuente: Departament de Extranjería y Migración ") +
  ylab("Llegadas") +  xlab("") 






