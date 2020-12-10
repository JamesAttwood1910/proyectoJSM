# Piramide de poblacion Chile  / Population Pyramid Chile

# En este script se expone como crear un piramide de poblacion para Chile. Se comparan las poblaciones 
# inmigrantes, nativo, y total. Datos se descargan del census Chileno 2017 (Instituto Nacional de Estadistica)

https://www.censo2017.cl/descargas/inmigracion/181122-caracteristicas-de-la-inmigracion-internacional-en-chile-censo-2017.xlsx

# Grafico No3 contiene los datos usados. 

# This script shows how to create a population pyramid for Chile. The inmigrant, native, and total populations are shown. 
# The data is taken from the 2017 national census which is available with the below link an grafic No3.

https://www.censo2017.cl/descargas/inmigracion/181122-caracteristicas-de-la-inmigracion-internacional-en-chile-censo-2017.xlsx


# Paquetes / Packages

library(readxl)
library(janitor)
library(hablar)
library(dplyr)
library(tidyr)
library(ggplot2)

# Datos / Data

chiledata <- read_excel("1.GraficosEjemplos/5. Pirámide poblacional /Poblacion por edad y pais de nacimiento.xlsx")


# Preparación de datos / Preparing the data 

head(chiledata)

chiledata <- chiledata %>% row_to_names(row_number = 1) %>% clean_names() %>% rename("Age" = "na", "Hombres_Inmigrantes" = "hombres", "Mujeres_Inmigrantes" = "mujeres", 
                                                                                     "Hombres_Chile" = "hombres_2", "Mujeres_Chile" = "mujeres_2") %>%
  
  hablar::convert(int(Hombres_Inmigrantes, Mujeres_Inmigrantes,
                      Hombres_Chile, Mujeres_Chile))


a <- chiledata %>% mutate(Inmigrantes = abs(Hombres_Inmigrantes) + abs(Mujeres_Inmigrantes)) %>% 
  mutate(No_Inmigrantes = abs(Hombres_Chile) + abs(Mujeres_Chile)) %>% select(1,6,7) %>%
  pivot_longer(names_to = "Inmigrante", values_to = "Poblacion", cols = 2:3) %>%
  mutate(PopPerc = case_when(Inmigrante == "Inmigrantes" ~ round(Poblacion/sum(Poblacion)*100,2),
                             TRUE ~ round(Poblacion/sum(Poblacion)*100,2)),
         signal=case_when(Inmigrante=='Inmigrantes'~1,
                          TRUE~-1))

b <- a %>% filter(Inmigrante == "Inmigrantes")
PoblacionInmigrante <- sum(b$Poblacion)

c <- a %>% filter(Inmigrante == "No_Inmigrantes")
PoblacionNativo <- sum(c$Poblacion)

head(a)

d <-  a %>% select(1:3,5) %>% mutate(PopPerc = case_when(Inmigrante == "Inmigrantes" ~ round(Poblacion/sum(PoblacionInmigrante)*100,2),
                                                         TRUE ~ round(Poblacion/sum(PoblacionNativo)*100,2)))



aggregate(PopPerc ~ Inmigrante, d, "sum") # Para revisar que los porcentajes suman a 100%. Son 99.9 porque se redondearon. 
aggregate(Poblacion ~ Inmigrante, d, "sum") # para sumar los poblaciones


d <- d %>% mutate(PopPerc2 = case_when(Inmigrante == "No_Inmigrantes" ~ PopPerc*-1, 
                                       TRUE ~ PopPerc * 1))


# Trazar grafico / Plotting graph 

ggplot(d)+
  geom_bar(aes(x=Age,y=PopPerc2,fill=Inmigrante),stat='identity')+
  geom_text(aes(x=Age,y=PopPerc2+signal*0.5,label=abs(PopPerc2)))+
  coord_flip()+
  scale_fill_manual(name='',values=c('darkred','steelblue'))+
  scale_y_continuous(breaks=seq(-5,5,1),
                     labels=function(y){paste(abs(y),'%')}
  )+
  labs(x='',y='Población (%)',
       title='Pirámide de población de Chile',
       subtitle=paste('Total Población Chilena 2017', format(sum(d$Poblacion),big.mark=',')),
       caption='Fuente:  El Instituto Nacional de Estadísticas (INE)')+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')



ggplot(d)+
  geom_bar(aes(x=Age,y=PopPerc2,fill=Inmigrante),stat='identity')+
  coord_flip()+
  scale_fill_manual(name='',values=c('darkred','steelblue'))+
  scale_y_continuous(breaks=seq(-2,4,1),
                     labels= c("-2%","-1%","0%","1%","2%","3%","4%")
  )+
  scale_x_discrete(breaks = seq(0,100,25),
                   labels = c(0,25,50,75,100)) +
  labs(x='',y='Población (%)',
       title='Pirámide de población de Chile',
       subtitle=paste('Total Población Chilena 2017', format(sum(d$Poblacion),big.mark=',')),
       caption='Fuente:  El Instituto Nacional de Estadísticas (INE)')+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')


# Agrupar edad / grouping age

d$Age_grouped <- ifelse(d$Age %in% c(0:4), "0-4", 
                        ifelse(d$Age %in% c(5:9), "5-9",
                               ifelse(d$Age %in% c(10:14), "10-14", 
                                      ifelse(d$Age %in% c(15:19), "15-19",
                                             ifelse(d$Age %in% c(20:24), "20-24",
                                                    ifelse(d$Age %in% c(25:29), "25-29",
                                                           ifelse(d$Age %in% c(30:34), "30-34",
                                                                  ifelse(d$Age %in% c(35:39), "35-39",
                                                                         ifelse(d$Age %in% c(40:44), "40-44",
                                                                                ifelse(d$Age %in% c(45:49), "45-49",
                                                                                       ifelse(d$Age %in% c(50:54), "50-54",
                                                                                              ifelse(d$Age %in% c(55:59), "55-59",
                                                                                                     ifelse(d$Age %in% c(60:64), "60-64",
                                                                                                            ifelse(d$Age %in% c(65:69), "65-69",
                                                                                                                   ifelse(d$Age %in% c(70:74), "70-74",
                                                                                                                          ifelse(d$Age %in% c(75:79), "75-79",
                                                                                                                                 ifelse(d$Age %in% c(80:84), "80-84",
                                                                                                                                        ifelse(d$Age %in% c(85:89), "85-89",
                                                                                                                                               ifelse(d$Age %in% c(90:94), "90-94", "94+")))))))))))))))))))



d

dgroups <- aggregate(Poblacion ~ Inmigrante + Age_grouped, d, "sum") %>% pivot_wider(names_from = Inmigrante, values_from = Poblacion) %>%
  mutate(Poblacion_total = Inmigrantes + No_Inmigrantes) %>% pivot_longer(names_to = "Grupo_de_Poblacion", values_to = "Poblacion", -1) %>%
  mutate(PopPerc = case_when(Grupo_de_Poblacion == "Inmigrantes" ~ round(Poblacion/sum(PoblacionInmigrante)*100,2),
                             Grupo_de_Poblacion == "No_Inmigrantes" ~ round(Poblacion/sum(PoblacionNativo)*100,2),
                             TRUE ~ round(Poblacion/sum(PoblacionInmigrante+PoblacionNativo)*100,2)))

aggregate(PopPerc ~ Grupo_de_Poblacion, dgroups, "sum") # revisar percentajes / check percentages

dgroups <- dgroups %>% mutate(PopPerc2 =case_when(Grupo_de_Poblacion == "Inmigrantes" ~ PopPerc*-1,
                                                  TRUE ~ PopPerc* 1)) 

dgroups <- dgroups %>%  mutate(signal=case_when(Grupo_de_Poblacion=='Inmigrantes'~-1,
                                                TRUE~1))
dgroups$Age_grouped <- factor(dgroups$Age_grouped, levels=c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                                            "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                            "90-94",   "94+"))
dgroups

# Trazar de nuevo / graph again

ggplot(dgroups %>% filter(!Age_grouped == "No_Inmigrantes")) +
  geom_bar(aes(x=Age_grouped,y=PopPerc2,fill=Grupo_de_Poblacion),stat='identity') +
  coord_flip() + 
  scale_fill_manual(name='',values=c('darkred','steelblue', 'orange'), labels = c("Inmigrante", "Nativo", "Total")) +
  scale_y_continuous(breaks=seq(-20,20,5),
                     labels= c("-20%","-15%","-10%","-5%","0%","5%","10%", "15%", "20%")
  ) + 
  labs(x='',y='Población (%)',
       title='Pirámide de población de Chile',
       subtitle=paste('Total Población Chilena 2017:', format(sum(d$Poblacion),big.mark=',')),
       caption='Fuente:  El Instituto Nacional de Estadísticas (INE)'
  )+
  theme_minimal() +
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center') 

