# 
#packages
library(tidyverse)
library(readxl)
library(sf)
library(stringi)

# Usefull refrerences -----------------------------------------------------

bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
bla_state_capitals <- data.frame(name_muni = c("Manaus", "Macapá", "Porto Velho", "Rio Branco", 
                                               "Boa Vista",
                                               "São Luís", "Cuiabá", "Belém", "Palmas"), 
                                 codmun7 = c("1302603", "1600303", "1100205", "1200401", 
                                             "1400100",
                                             "2111300", "5103403", "1501402", "1721000")
)
dfstates <- data.frame(bla_state_names, bla_state_siglas)

# Subset Mapbiomas -----------------------------------------------------
#Mapbiomas. Values are hectares.
#Subset of columns and years needed.
#Summary file for municipalities downloaded from https://mapbiomas.org/estatisticas
#.csv files created via "save as" from original excel sheets
#Mapbiomas transitions
file_trans <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\AmazonConservation\\mapbiomas_ge\\MapBiomas_COL6.0_UF-MUNICIPIOS_v12_trans.csv"
mapbiomas_trans <- read.csv(file_trans, as.is = TRUE) 
#tidy and select desired columns
cols_ref <- names(mapbiomas_trans)[1:23]
cols_trans <- paste("X",1985:2018, ".",1986:2019, sep="")
mapbiomas_trans %>% 
  filter(state %in% all_of(bla_state_siglas)) %>% 
  select(all_of(cols_ref), all_of(cols_trans)) %>%
  mutate(geo_code = as.character(geo_code)) -> mapbiomas_trans
#length(unique(mapbiomas_trans$geo_code)) #808
saveRDS(mapbiomas_trans, "mapbiomas_trans.RDS")


#Mapbiomas cover
file_cover <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\AmazonConservation\\mapbiomas_ge\\MapBiomas_COL6.0_UF-MUNICIPIOS_v12_cover.csv"
mapbiomas_cover <- read.csv(file_cover, as.is = TRUE) 
cols_cover_ref <- names(mapbiomas_cover)[1:13]
cols_cover <- paste("X",1985:2020, sep="")
mapbiomas_cover %>% 
  filter(state %in% all_of(bla_state_siglas)) %>% 
  select(all_of(cols_cover_ref), all_of(cols_cover)) %>%
  mutate(geo_code = as.character(geo_code)) -> mapbiomas_cover
#length(unique(mapbiomas_cover$geo_code)) #808
saveRDS(mapbiomas_cover, "mapbiomas_cover.RDS")


#Load data -----------------------------------------------------
#Mapbiomas. #Mapbiomas. Values are hectares.
mapbiomas_legend <- read_excel("data//mapbiomas_6_legend.xlsx")
mapbiomas_trans <- readRDS("mapbiomas_trans.RDS")
mapbiomas_cover <- readRDS("mapbiomas_cover.RDS")

#IBGE spatial data
ibge_muni <- "vector\\brazil_ninestate_municipalities\\ninestate_muni.shp"
sf_ninestate_muni <- st_read(ibge_muni)
ibge_state <- "vector\\ninestate_poly.shp"
sf_ninestate <- st_read(ibge_state)

# Prep summaries -----------------------------------------------------
#Forest transition long format with annual totals
#loss_km2 is area (km2) of natural forest that changed to anthropic class
#Used to select desired columns
cols_ref <- names(mapbiomas_trans)[1:23]
cols_trans <- paste("X",1985:2018, ".",1986:2019, sep="")
cols_cover_ref <- names(mapbiomas_cover)[1:13]
cols_cover <- paste("X",1985:2020, sep="")

mapbiomas_trans %>% 
  select(state, city, geo_code, 
         from_level_0, from_level_1, from_level_2, to_level_0, all_of(cols_trans)) %>% 
  filter(from_level_0 == "Natural", from_level_1 == "1. Forest", 
         to_level_0 == "Anthropic") %>% 
  mutate(forest_class = case_when(from_level_2 == "Forest Formation" ~ "loss_forest_km2", 
                                  from_level_2 == "Savanna Formation" ~ "loss_savanna_km2",
                                 TRUE ~ "loss_other_km2")) %>%
  pivot_longer(cols = starts_with("X"), names_to = "ayear", names_prefix ="X", 
               values_to = "forestcover_loss") %>% 
  group_by(state, city, geo_code, forest_class, ayear) %>% 
  summarise(loss_km2 = sum(forestcover_loss, na.rm=TRUE)/100) %>%
  mutate(year = as.numeric(substr(ayear,6,9))) %>% ungroup() -> mapbiomas_trans_forest
#length(unique(mapbiomas_trans_forest$geo_code)) #808

#export 
#complete set with municipality area
data.frame(sf_ninestate_muni) %>%
  select(CD_MUN, AREA_KM2) %>%
crossing(year = 2002:2019) %>% 
  left_join(
# join with loss lag values
mapbiomas_trans_forest %>%
  pivot_wider(id_cols = c(state, city, geo_code, year), 
              names_from = forest_class, values_from = loss_km2) %>% 
  mutate(loss_forest_km2 = replace_na(loss_forest_km2,0), 
         loss_savanna_km2 = replace_na(loss_savanna_km2,0), 
         loss_other_km2 = replace_na(loss_other_km2,0)) %>% 
  mutate(tot_transition_km2 = loss_forest_km2 + loss_savanna_km2 + loss_other_km2) %>%
  group_by(state, city, geo_code) %>%
  arrange(state, city, geo_code) %>% 
  mutate(lag01_lossarea_km2 = lag(tot_transition_km2, order_by = year), 
         lag02_lossarea_km2 = lag(tot_transition_km2, n=2, order_by = year), 
         lag03_lossarea_km2 = lag(tot_transition_km2, n=3, order_by = year), 
         lag04_lossarea_km2 = lag(tot_transition_km2, n=4, order_by = year), 
         lag05_lossarea_km2 = lag(tot_transition_km2, n=5, order_by = year), 
         lag06_lossarea_km2 = lag(tot_transition_km2, n=6, order_by = year), 
         lag07_lossarea_km2 = lag(tot_transition_km2, n=7, order_by = year), 
         lag08_lossarea_km2 = lag(tot_transition_km2, n=8, order_by = year), 
         lag09_lossarea_km2 = lag(tot_transition_km2, n=9, order_by = year), 
         lag10_lossarea_km2 = lag(tot_transition_km2, n=10, order_by = year)) %>% 
  filter(year >=2002), 
by = c("CD_MUN" = "geo_code", "year" = "year") 
) %>% 
  mutate(lossarea_per = round((replace_na(tot_transition_km2,0) / AREA_KM2)*100,3),
           lag01_lossarea_per = round((replace_na(lag01_lossarea_km2,0) / AREA_KM2)*100,3), 
         lag02_lossarea_per = round((replace_na(lag02_lossarea_km2,0) / AREA_KM2)*100,3), 
         lag03_lossarea_per = round((replace_na(lag03_lossarea_km2,0) / AREA_KM2)*100,3), 
         lag04_lossarea_per = round((replace_na(lag04_lossarea_km2,0) / AREA_KM2)*100,3), 
         lag05_lossarea_per = round((replace_na(lag05_lossarea_km2,0) / AREA_KM2)*100,3), 
         lag06_lossarea_per = round((replace_na(lag06_lossarea_km2,0) / AREA_KM2)*100,3), 
         lag07_lossarea_per = round((replace_na(lag07_lossarea_km2,0) / AREA_KM2)*100,3), 
         lag08_lossarea_per = round((replace_na(lag08_lossarea_km2,0) / AREA_KM2)*100,3), 
         lag09_lossarea_per = round((replace_na(lag09_lossarea_km2,0) / AREA_KM2)*100,3), 
         lag10_lossarea_per = round((replace_na(lag10_lossarea_km2,0) / AREA_KM2)*100,3)) %>% 
  #immediate, 3y and 5y
  mutate(loss_immediate_percent_muni = lossarea_per + 
           lag01_lossarea_per, 
         tot_loss3y_percent_muni = lag01_lossarea_per + 
           lag02_lossarea_per + lag03_lossarea_per, 
         tot_loss5y_percent_muni = lag01_lossarea_per + 
           lag02_lossarea_per + lag03_lossarea_per + 
           lag04_lossarea_per + lag05_lossarea_per, 
         loss_immediate_km2 = tot_transition_km2 + 
           lag01_lossarea_km2, 
         tot_loss3y_km2 = lag01_lossarea_km2 + 
           lag02_lossarea_km2 + lag03_lossarea_km2, 
         tot_loss5y_km2 = lag01_lossarea_km2 + 
           lag02_lossarea_km2 + lag03_lossarea_km2 + 
           lag04_lossarea_km2 + lag05_lossarea_km2) %>%
  arrange(state, city) -> df_transition

# Forest cover per municipality, medians for 1986, 2002, 2012, 2019
# For municipality level reference (i.e. not annual)
mapbiomas_cover %>% 
  select(state, city, geo_code, 
         level_0, level_1, level_2, all_of(cols_cover)) %>% 
  filter(level_0 =="Natural",
         level_1 == "1. Forest") %>% 
  mutate(forest_class = case_when(level_2 == "Forest Formation" ~ "cover_forest_km2", 
                                  level_2 == "Savanna Formation" ~ "cover_savanna_km2",
                                  TRUE ~ "cover_other_km2")) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "ayear",  
               values_to = "forestcover") %>% 
  group_by(state, city, geo_code, forest_class, ayear) %>% 
  summarise(forestcover_km2 = sum(replace_na(forestcover,0))/100) %>% 
  ungroup() %>% 
  mutate(year_flag = case_when(ayear %in% c("X1985", "X1986", "X1987") ~ "X1986", 
                                ayear %in% c("X2001", "X2002", "X2003") ~ "X2002", 
                               ayear %in% c("X2011", "X2012", "X2013") ~ "X2012", 
                               ayear %in% c("X2018", "X2019", "X2020") ~ "X2019", 
                               TRUE ~ NA_character_) 
  ) %>%  
  filter(!is.na(year_flag)) %>%
  group_by(state, city, geo_code, forest_class, year_flag) %>%  
  summarise(forestcover_med_km2 = median(forestcover_km2)) %>% 
  pivot_wider(id_cols = c(state, city, geo_code, year_flag), 
              names_from = forest_class, values_from = forestcover_med_km2) %>% 
  mutate(cover_forest_km2 = replace_na(cover_forest_km2,0), 
         cover_savanna_km2 = replace_na(cover_savanna_km2,0), 
         cover_other_km2 = replace_na(cover_other_km2,0)) %>% 
  mutate(cover_tot_km2 = cover_forest_km2 + cover_savanna_km2 + cover_other_km2) %>% 
  # total cover 
  pivot_wider(id_cols = c(state, city, geo_code), 
              names_from = year_flag, names_prefix = "forestcover_med_km_", 
              values_from = cover_tot_km2) -> df_cover_muni

data.frame(sf_ninestate_muni) %>%
  select(CD_MUN, AREA_KM2) %>% 
  left_join(df_cover_muni, by = c("CD_MUN"="geo_code")) %>% 
  mutate(forestcover_2002med_percent_86 = 
           (forestcover_med_km_X2002/forestcover_med_km_X1986)*100,	
         forestcover_2012med_percent_86 = 
           (forestcover_med_km_X2012/forestcover_med_km_X1986)*100, 
         forestcover_2019med_percent_86 = 
           (forestcover_med_km_X2019/forestcover_med_km_X1986)*100, 
         forestcover_1986med_percent_muni = 
           (forestcover_med_km_X1986 / AREA_KM2 )*100, 
         forestcover_2019med_percent_muni = 
           (forestcover_med_km_X2019 / AREA_KM2 )*100) %>% 
  mutate(cover_diff_1986_2019_per_muni = 
           forestcover_2019med_percent_muni - forestcover_1986med_percent_muni, 
         cover_diff_1986_2019_per_86 = 
           ((forestcover_med_km_X2019-forestcover_med_km_X1986) / 
              forestcover_med_km_X1986)*100, 
         cover_diff_1986_2012_per_86 = 
           ((forestcover_med_km_X2012-forestcover_med_km_X1986) / 
              forestcover_med_km_X1986)*100
  ) -> df_cover_muni_out

#export  
df_cover_muni_out %>% 
  arrange(state, city) %>%
  write.csv("muni_fixed_cover.csv", row.names = FALSE)


# Add loss as % of forest cover in 1986
df_transition %>% 
  left_join(df_cover_muni_out %>% 
              crossing(year = 2002:2019) %>% 
              select(CD_MUN, year, forestcover_med_km_X1986)) %>% 
  mutate(loss_immediate_percent_86 = 
           (loss_immediate_km2/forestcover_med_km_X1986)*100, 
         totloss_3y_percent_86 = 
           (tot_loss3y_km2/forestcover_med_km_X1986)*100, 
         totloss_5y_percent_86 = 
           (tot_loss5y_km2/forestcover_med_km_X1986)*100) -> df_transition_out
#export
df_transition_out %>% 
  write.csv("muni_fixed_lagloss.csv", row.names = FALSE)

#Urban cover
#complete set with municipality area
data.frame(sf_ninestate_muni) %>%
  select(CD_MUN, NM_MUN, SIGLA_UF, AREA_KM2) %>%
  crossing(year = 2002:2019) %>% 
  left_join(
    mapbiomas_cover %>% 
      select(state, city, geo_code, 
             level_0, level_1, level_2, all_of(cols_cover)) %>% 
      filter(level_2 == "Urban Infrastructure") %>% 
      pivot_longer(cols = starts_with("X"), names_to = "ayear",  
                   values_to = "urbancover") %>% 
      group_by(state, city, geo_code, level_2, ayear) %>%  
      summarise(urbancover_ha = sum(urbancover)) %>% 
      mutate(year = as.numeric(substring(ayear,2,5))) %>% 
      ungroup() %>%
      select(state,city, geo_code, year, urbancover_ha), 
    by = c("CD_MUN" = "geo_code", "year" = "year") )%>% 
  mutate(urbancover_ha = replace_na(urbancover_ha, 0)) %>%
  arrange(SIGLA_UF, NM_MUN) %>%
  write.csv("muni_fixed_urbancover.csv", row.names = FALSE)
