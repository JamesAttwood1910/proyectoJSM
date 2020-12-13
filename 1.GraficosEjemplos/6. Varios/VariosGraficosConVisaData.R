## Visas Otorgadas 2019

# En este document se usan datos para visas ortorgadas en 2019 en Chile. Se manipulan los datos 
# para crear varios tipos de graficos. 
# Los datos se pueden descargar en https://www.extranjeria.gob.cl/media/2020/03/visas_otorgadas_2019.xlsx 

# This document explores data for granted visas in Chile in 2019. The data is manipulated to create several graphs. 
# The data can be downloaded from https://www.extranjeria.gob.cl/media/2020/03/visas_otorgadas_2019.xlsx 


# Paquetes / Packages

library(readxl)
library(scales)
library(ggplot2)
library(dplyr)

# Datos / Data 

visas_otorgadas_2019 <- read_excel("2. Datos/visas_otorgadas_2019.xlsx")



# Actividad de inmigrantes (masculino vs feminino) / Activity of immigrants (male vs female)

View(visas_otorgadas_2019)

head(visas_otorgadas_2019)

class(visas_otorgadas_2019$ACTIVIDAD)

visas_otorgadas_2019 %>% select(SEXO, ACTIVIDAD) %>% count(ACTIVIDAD, SEXO) %>%
  ggplot() + 
  facet_wrap(vars(SEXO)) +
  geom_bar(aes(x = ACTIVIDAD, y = n), stat = "identity", fill = "#D6604D") +
  coord_flip() + 
  scale_y_continuous(labels = comma_format(big.mark = ",")) + 
  theme_minimal() + 
  labs(title = "Actividad de Inmigrantes que llegaron en 2019", 
       subtitle = "", 
       caption = "Fuente: Departament de Extranjería y Migración") + ylab("Personas") + 
  xlab("Actividad")



# Calcular edad de inmigrantes 
# En cual comuna de Santiago viven inmigrantes cubanos qe tienen una edad de menos de 50 años. 
# Calculate age of immigrants 
# In which borough do cuban immigrants who are less than 50 years old live

visas_otorgadas_2019$date <- as.Date(visas_otorgadas_2019$NACIMIENTO) 
y <- visas_otorgadas_2019 %>% select(PAÍS, COMUNA, PROVINCIA, ESTUDIOS, date)
y$age <- as.numeric(difftime(as.Date(c("2019-12-31")), y$date, units = "weeks"))/52.25


y %>% filter(PAÍS == "Cuba" & age < 50) %>% group_by(PROVINCIA, COMUNA, ESTUDIOS) %>% tally() %>%
  
  filter(PROVINCIA == "Santiago") %>%
  
  ggplot() + 
  
  geom_bar(aes(x = ESTUDIOS, y = n), stat = "identity", fill = "#D6604D") + 
  
  facet_wrap(vars(COMUNA)) +
  
  theme_minimal() +
  
  coord_flip() +
  
  labs(title = "Nivel educaciónal de inmigrantes cubanos con menos de 50 años",
       subtitle = "Son inmigrantes que llegaron a la provincia de Santiago en 2019",
       caption = "Fuente: Departament de Extranjería y Migración") +
  ylab("Numero de inmigrantes") +  xlab("Nivel de educación")







# Distribucion de inmigrantes por las comunas de Santiago
# Distribution of immigrants in the boroughs of Santiago

y %>% filter(PROVINCIA == "Santiago") %>% group_by(PAÍS, COMUNA) %>% tally() %>% 
  
  arrange(-n) %>% mutate(new = case_when(PAÍS == "Venezuela" ~ "Venezuela",
                                         PAÍS == "Haití" ~ "Haití",
                                         PAÍS == "Colombia" ~ "Colombia",
                                         PAÍS == "Perú" ~ "Perú",
                                         PAÍS == "Bolivia" ~ "Bolivia",
                                         TRUE ~ "Otro")) %>%
  
  ggplot() + 
  
  geom_bar(aes(x = new, y = n), stat = "identity", fill = "#D6604D" ) + 
  
  facet_wrap(vars(COMUNA)) + 
  
  coord_flip() + 
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1)) +
  
  scale_y_continuous(labels = comma_format(big.mark = ",")) + 
  
  labs(title = "Distribución de inmigrantes que llegaron a Santiago en 2019",caption = "Fuente: Departament de Extranjería y Migración") +
  ylab("Numero de inmigrantes") +  xlab("País")




# Grafico Barra con Mes de llegada de inmigrantes Venezolanos 
# Bar chart for arrival month of Venezuelans 

visas_otorgadas_2019$MES <- month(visas_otorgadas_2019$MES, label = T)

Espanol <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

visas_otorgadas_2019 %>% filter(SEXO == "Masculino" & PAÍS == "Venezuela") %>%
  
  group_by(MES) %>% 
  tally() %>%
  
  hablar::convert(
    chr(MES)
  ) %>%
  
  ggplot() + 
  
  geom_bar(aes(x = MES, y = n), stat = "identity", fill = "#FF5733") + 
  
  coord_flip() + 
  
  theme_minimal() +
  
  scale_x_discrete(labels= Espanol) + 
  
  scale_y_continuous(labels = comma_format(big.mark = ",")) + 
  
  labs(title = "Mes de llegada de inmigrantes Venezolanos en 2019", caption = "Fuente: Departament de Extranjería y Migración") +
  ylab("Mes") +  xlab("Numero de inmigrantes")









# Grafico de linea para mostrar cantidad de llegadas a lo largo del año 2019
# Line graph to show the number of arrivals throughout 2019

f <- visas_otorgadas_2019 %>% mutate(test = case_when(MES == "ene" ~ "1 Enero, 2019",
                                                      MES == "feb" ~ "1 Febrero, 2019",
                                                      MES == "mar" ~ "1 Marzo, 2019",
                                                      MES == "abr" ~ "1 Abril, 2019",
                                                      MES == "may" ~ "1 Mayo, 2019",
                                                      MES == "J=jun" ~ "1 Junio, 2019",
                                                      MES == "jul" ~ "1 Julio, 2019",
                                                      MES == "ago" ~ "1 Agosto, 2019",
                                                      MES == "sep" ~ "1 Septiembre, 2019",
                                                      MES == "oct" ~ "1 Octubre, 2019",
                                                      MES == "nov" ~ "1 Noviembre, 2019",
                                                      TRUE ~ "1 Diciembre, 2019")) 


# variable para la fecha / date variable

f$test <- as.Date(f$test, format = "%d %B, %Y")

f$test

# Manipulación de los datos adicional y trazar grafico / additional data manipulation and plot graph

head(f$test)

f.2 <- f %>% select(PAÍS, MES, test) %>% group_by(PAÍS, test) %>% count(PAÍS) %>% 
  
  mutate(País_Agrupado = case_when(PAÍS == "Venezuela" ~ "Venezuela",
                                   PAÍS == "Haití" ~ "Haití",
                                   PAÍS == "Colombia" ~ "Colombia",
                                   PAÍS == "Perú" ~ "Perú",
                                   PAÍS == "Bolivia" ~ "Bolivia",
                                   TRUE ~ "Otro")) 

aggregate(n ~ País_Agrupado + test, data = f.2, "sum") %>%

  ggplot() + 
  
  geom_line(aes(x = test, y = n)) +
  
  facet_wrap(vars(País_Agrupado)) +
  
  theme_minimal() +
  
  scale_x_date(date_breaks = "2 month", date_labels = "%B") + 
  
  scale_y_continuous(n.breaks = 6, labels = comma_format(big.mark = ",")) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  
  labs(title = "Llegada mensual de inmigrantes en 2019",caption = "Fuente: Departament de Extranjería y Migración ") +
  ylab("Llegadas") +  xlab("") 






