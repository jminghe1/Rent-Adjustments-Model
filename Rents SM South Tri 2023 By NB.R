library(stats)
library(ggplot2)
library(tidyverse)
library(readxl)
library(doBy)
library(car)
library(broom)
library(clipr)
library(magrittr)
options(scipen=999)


south_tri <- read_excel("South Tri Data Finalized.xlsx", sheet = "Final_Data")

#Data Cleaning

colnames(south_tri) <- gsub(" ", ".", colnames(south_tri))

colnames(south_tri)

str(south_tri)

south_tri <-  south_tri %>% mutate(Units = ifelse(Unit.Type == "Studio", 0,
                                                  ifelse(Unit.Type == "1BR", 1,
                                                         ifelse(Unit.Type == "2BR",2,3))))

south_tri <-  south_tri %>% mutate(Luxury = ifelse(Style == "Hi-Rise","Luxury", "Normal"))

unique(south_tri$Luxury)

# Data Analysis
studio <- south_tri %>% filter(Unit.Type == "Studio")
one_br <- south_tri %>% filter(Unit.Type == "1BR")
two_br <- south_tri %>% filter(Unit.Type == "2BR")
three_br <- south_tri %>% filter(Unit.Type == "3BR")

summaryBy(Asking.Rent ~ Style + Building.Class, data = studio, FUN = c(mean))
summaryBy(Asking.Rent ~ Style + Building.Class, data = one_br, FUN = c(mean))
summaryBy(Asking.Rent ~ Style + Building.Class, data = two_br, FUN = c(mean))
summaryBy(Asking.Rent ~ Style + Building.Class, data = three_br, FUN = c(mean))

#Data Analysis: Showing linear trend between SF and Rent.
lmplot_sqft_rent <-  ggplot(south_tri, aes(x = Avg.SF,
                                           y = Asking.Rent)) +
                      geom_point(color = "purple") +
                      geom_smooth(method = "lm", se = FALSE)

lmplot_sqft_rent


#Modeling

lm1 <- lm(Asking.Rent ~ Avg.SF*Units, data = south_tri)
summary(lm1)

lm2 <- lm(Asking.Rent ~ Avg.SF*Units + Building.Class, data = south_tri)
summary(lm2)

anova(lm1,lm2)
#Adding Building Class to the model is relevant.


lm3 <-lm(Asking.Rent ~ Avg.SF*Units + Building.Class + Style, data = south_tri)
summary(lm3)

anova(lm2,lm3)
# Adding Style to the model is relevant.
#However the effect between low rise and mid rise is small (114 and 115).
#Therefore, removing Style variable in favor of Luxury variable can better reflect the difference between Hi-Rise, Mid-Rise, and Low-Rise.


lm3.1 <- lm(Asking.Rent ~ Avg.SF*Units + Building.Class + Luxury, data = south_tri)
summary(lm3.1)

lm4 <- lm(Asking.Rent ~ Avg.SF*Units + Building.Class + Luxury + Neighborhood, data = south_tri)
summary(lm4)

anova(lm3.1,lm4)
#Adding Neighborhood is relevant to the model.


#Checking assumptions
plot(lm4)# Red line seems to be fairly linear under the Residuals vs Fitted plot. Linearity assumption is met.
vif(lm4, type = "predictor") # VIF test determines the model does not have multicollinearity.


summary(lm4) %>% tidy() %>% write_clip()
