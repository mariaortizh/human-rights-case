
---
title: "Vinculación DDHH"
author: "María"
date: "4/12/2020"
output: html_document
---
```{r}
library(tidyverse)
library(gtools)
library(survey)
library(skimr)
library(readxl)
library(ggplot2)
library(plyr)
library(ggalt)
```
```{r}
DH <- read_excel ("C:/Users/mortiz/Desktop/Trabajo final R/6_TD_FORM_VINC_2016.xlsx") 
violencia <- read_excel ("C:/Users/mortiz/Desktop/Trabajo final R/I_nivel_victimizacion_2015_est.xlsx") 
```
```{r}
DH_1 <- DH  %>% select("NOM_ENT",
                        "EV_FOR",
                        "EV_FUNC",
                        "EVFUNC_H",
                        "EVFUNC_M",
                        "EVFUNC_NE",
                        "EV_PG",
                        "EVPG_H",
                        "EVPG_M",
                        "EVPG_NE",
                        "FUN_CT_4",
                        "FUNDIF_4") 
```
#Renomabras las variables#
```{r}
names(DH_1)[names(DH_1) == "NOM_ENT"] <- "estado"
names(DH_1)[names(DH_1) == "EV_FOR"] <- "even"
names(DH_1)[names(DH_1) == "EV_FUNC"] <- "func"
names(DH_1)[names(DH_1) == "EVFUNC_H"] <- "func_h"
names(DH_1)[names(DH_1) == "EVFUNC_M"] <- "func_m"
names(DH_1)[names(DH_1) == "EVFUNC_NE"] <- "func_n"
names(DH_1)[names(DH_1) == "EV_PG"] <- "pubg"
names(DH_1)[names(DH_1) == "EVPG_H"] <- "pubg_h"
names(DH_1)[names(DH_1) == "EVPG_M"] <- "pubg_m"
names(DH_1)[names(DH_1) == "EVPG_NE"] <- "pubg_ne"
names(DH_1)[names(DH_1) == "FUN_CT_4"] <- "func_rec1"
names(DH_1)[names(DH_1) == "FUNDIF_4"] <- "func_rec2"
names(violencia)[names(violencia) == "Entidad"] <- "estado"
names(violencia)[names(violencia) == "Tasa_prevalencia"] <- "prevalencia"
```
```{r}
DH_1<-DH_1 %>%
  mutate(even=na_if(even,-1)) %>%	
  mutate(even=na_if(even,-1)) %>%	
  mutate(func=na_if(func,-1)) %>%	
  mutate(func_h=na_if(func_h,-1)) %>%	
  mutate(func_m=na_if(func_m,-1)) %>%	
  mutate(func_n=na_if(func_n,-1)) %>%	
  mutate(pubg=na_if(pubg,-1)) %>%	
  mutate(pubg_h=na_if(pubg_h,-1)) %>%	
  mutate(pubg_m=na_if(pubg_m,-1)) %>%	
  mutate(pubg_ne=na_if(pubg_ne,-1)) %>%	
  mutate(func_rec1=na_if(func_rec1,-1)) %>%	  
  mutate(func_rec2=na_if(func_rec2,-1)) %>% 
  mutate(func_rec2=na_if(func_rec2,-2))
```
#Fusión de las variables de funcionarios de Instituciones de Reclusión
```{r}
DH_1<-DH_1%>%
  
  mutate(func_rec=func_rec1+func_rec2) %>%
  
  select(-func_rec1,-func_rec2)
```
#Nueva variable de efectividad de las actividades por la participación del público en general
```{r}
DH_2<-DH_1 %>%
  
   mutate(efect=pubg/even)
```
#Eliminación de los valores perdidos
```{r}
DH_3=DH_2 %>% drop_na(efect)
```
#Fusión de las bases de datos por medio de los Estados
```{r}
DH_4=left_join(DH_2, violencia, by = "estado")
```
#Gráficas
```{r}
ggplot(DH_4, aes(y = reorder(estado, prevalencia),
                      x = efect)) + geom_bar(stat="identity", width=0.8,color="black", fill="gray") + labs(title = "",
       x = "Efectividad actividades Organismos de DDHH",
       y = "Estado de mayor a menor prevalencia de violencia")
```
```{r}
ggplot(DH_4, aes(y = reorder(estado, prevalencia),
                      x = efect,
                      xend = func_rec)) +
  geom_dumbbell(aes(y = reorder(estado, prevalencia),
                    x = efect,
                    xend = func_rec),
                colour="black",
                colour_x = "grey30",
                colour_xend = "grey50",
                size_x=1.4,
                size_xend=1.5)+
  geom_dumbbell(aes(y = reorder(estado, prevalencia),
                    x = efect,
                    xend = func_rec),
                colour="grey90",
                colour_x = "grey50",
                colour_xend = "grey70",
                size_x=1.5,
                size_xend=1.6)+
  labs(title = "",
       x = "Efectividad actividades Organismos de DDHH",
       y = "")
```

