# Grafico lollipop 

"Este guion reproduce el grafico 2.2.1 en pagina 8 del informe 2019"

library(readr)
library(ggplot2)
library(dplyr)
library(scales)

# Crear base de datos

a <- tibble(
  Año = c("2019", "2018", "2017", "2014", "2010", "2005",
          "2002", "1992", "1982",
          "1970", "1940", "1920", "1907","1885"),
  Población = c(1492522, 1250365, 746465, 410998, 305212, 212935,
                195320, 105170, 83805, 90441, 107273, 114117, 
                132312, 87077),
  Porciento = c(7.8, 6.6, 4.4, 2.3, 1.8, 1.3, 1.2, 0.8,
                0.7, 1, 2.1,3.1, 4.1, 3.4)
)

head(a)

a <- a %>% 
  arrange(Población) %>%
  mutate(Año = as.factor(Año))


# Trazar datos 

ggplot(data = a, aes(x = Año, y = Población)) + 
  geom_segment(aes(x = Año, xend = Año, y = 0, yend = Población), 
               color = ifelse(a$Año %in% c("2017"), "#EF3D14", "#054762"), 
               size = ifelse(a$Año %in% c("2017"), 2, 0.7)) + 
  geom_point(color = ifelse(a$Año %in% c("2017"), "#EF3D14", "#054762"),
             size = ifelse(a$Año %in% c("2017"), 3, 1)) +
  coord_flip() +  
  theme_minimal() + 
  annotate(geom = "label", x = grep("2017", a$Año), y = a$Población[which(a$Año=="2017")]*1.7,
           label = paste("Census 2017 \n Población =", a$Población[which(a$Año=="2017")]),
           fill = "#16C3CE") +
  scale_y_continuous(labels = comma_format(big.mark = ","), limits = c(0,2000000)) +
  labs(title = "Poplación inmigrante en Chile", caption = "Fuente: XXXX" )


# Como agregar mas diseños. 

install.packages("ggthemes") # use to add pre set themes to ggplot
library(ggthemes)

theme_set(theme_gray(base_size = 20)) ## to set a new theme to r projct




