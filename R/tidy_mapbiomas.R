# tidy mapbiomas (file takes about three hours to generate in google earth engine)
library(tidyverse)
library(readxl)
library(sf)
library(stringi)

mapbiomas_legend <- read_excel("data//mapbiomas_6_legend.xlsx")
filein <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\AmazonConservation\\mapbiomas-brazil-collection-60-buriti-ma-area.csv"
mapbiomas_raw <- read.csv(filein, as.is = TRUE, encoding="UTF-8")
mapbiomas_raw$city = "Buriti"
mapbiomas_raw %>% 
  separate(band, c(NA, "year"), remove=FALSE) %>% 
  group_by(city, band, year, class, class_name) %>% 
  summarise(area_ha = sum(area, na.rm = TRUE)) %>% 
  ungroup() %>% left_join(mapbiomas_legend, 
                          by = c("class" = "aid"))
 # mutate()
