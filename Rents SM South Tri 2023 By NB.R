library(stats)
library(ggplot2)
library(tidyverse)
library(readxl)
library(car)
library(broom)
library(clipr)
library(magrittr)
library(ggResidpanel)

options(scipen=999)



south_tri <- read_excel("South Tri Data Finalized.xlsx", sheet = "Final_Data")

#Data Cleaning

colnames(south_tri) <- gsub(" ", ".", colnames(south_tri))

colnames(south_tri)

str(south_tri)

colnames(south_tri)

#Data Analysis: Showing linear trend between SF/Units and Rent.
lmplot_sqft_rent <-  ggplot(south_tri, aes(x = Avg.SF,
                                           y = Asking.Rent)) +
                      geom_point(color = "purple") +
                      geom_smooth(method = "lm", se = FALSE)

lmplot_sqft_rent

boxplot(south_tri$Avg.SF)
boxplot(south_tri$Asking.Rent)


#Modeling
lm1 <- lm(Asking.Rent ~ Avg.SF*Unit.Type + factor(Building.Class) + factor(Neighborhood), data = south_tri)

wols1 <- lm(Asking.Rent ~ Avg.SF*Unit.Type + factor(Building.Class) + factor(Neighborhood), data = south_tri, weights = 1/Avg.SF)

summary(lm1) 
summary(wols1) 
 

bptest(wols1)


summary(wols1)
summary(wols1) %>% tidy() %>% write_clip()



