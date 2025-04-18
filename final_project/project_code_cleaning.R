library(tidyverse)
library(broom)
library(dplyr)

dat <- read.csv("plate_reading_2.csv")
names(dat)



dat_1 <- dat %>% 
  rename_with(~ sub("//.//d+$", "", .x)) %>%
  pivot_longer(cols = c("Cell_count", "Cell_count.1", "Cell_count.2", 
                        "Cell_count.3", "Cell_count.4", "Cell_count.5", 
                        "Cell_count.6", "Cell_count.7"), 
               names_to = "Measurement", 
               values_to = "cell_count")

dat_2 <- dat_1 %>% 
  rename_with(~ sub("//.//d+$", "", .x)) %>%
  pivot_longer(cols = c("standard", "standard.1", "water", "water.1", 
                        "ethanol", "ethanol.1", "hydrogen_peroxide", 
                        "hydrogen_peroxide.1"), 
               names_to = "reagent", 
               values_to = "absorbance")

dat_3 <- dat_2 %>% 
  pivot_longer(cols = c("Concentration", "Concentration.1", 
                        "Concentration.2", "Concentration.3", 
                        "Concentration.4", "Concentration.5"), 
               names_to = "concentration.1", 
               values_to = "concentration")

dat_cleaned <- dat_3 %>% 
  select("Well", 
         "cell_count", 
         "reagent", 
         "absorbance", 
         "concentration")


?kable_styling

dat_cleaned %>% 
  ggplot(aes(x = reagent, 
             y = absorbance, 
             color = concentration)) + 
  geom_point() + 
  theme_minimal()
