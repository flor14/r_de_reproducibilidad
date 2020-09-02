library(readr)
library(tidyr)
library(ggplot2)

coffee_data <- read_csv("C:/Users/FLORENCIA/Dropbox/reproducibilidad_slides/coffee_data.csv")

coffee_data %>%
  pivot_longer(cols=aroma:moisture,
               names_to="caracteristicas_cafe",
               values_to="value") %>%
  ggplot(aes(value, total_cup_points))+
  geom_smooth(method= "gam", span=0.3, color='purple')+
  facet_wrap(~caracteristicas_cafe, scale="free_y")


