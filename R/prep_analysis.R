library(tidyverse)
library(readxl)
library(stringi)
#load data
df_muni <- read_excel("data/bla_municipalities_4trees.xlsx", 
                                   na = c("", "NA"),
                                   sheet = "municipality_fixed_ref",
                                   .name_repair = "universal")
df_muni_year <- read_excel("data/bla_municipalities_4trees.xlsx",
                      na = c("", "NA"),
                      sheet = "municipality_annual",
                      .name_repair = "universal")

#municipalities to include. Exclude new munis and state capitals
df_muni %>% filter(flag_include == "yes") %>% pull(muni_code) -> keep_muni #794

#Basic reference vectors
bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
bla_state_capitals <- data.frame(name_muni = c("Manaus", "Macapá", "Porto Velho", "Rio Branco", 
                                               "Boa Vista",
                                               "São Luís", "Cuiabá", "Belém", "Palmas"), 
                                 codmun7 = c(1302603, 1600303, 1100205, 1200401, 
                                             1400100,
                                             2111300, 5103403, 1501402, 1721000)
) %>% mutate(muni_upper = toupper(name_muni)) %>% 
  mutate(muni_inep = stri_trans_general(muni_upper, "Latin-ASCII"))

#select variables
names(df_muni_year)
var_response <- c("gdp_percapita_reais",  "gva_agri_percapita_reais",
                  "salary_mean_reais", "min_salary_mean")
var_timeconstant <- c("state_name", "muni_name", "muni_area_km2", 
                      "long", "lat" ,"dist_statecapital_km", 
                      "flag_urban", "indigenous_area_percent")
var_timevary <- c("year","pop_dens_km2", "tot_pop", "urban_ha_p1000",
                   "process_gold_p1000",  "gva_industry_percent", 
                  "tot_transition_km2",
                  "loss_immediate_km2", 
                  "tot_loss3y_km2", "tot_loss5y_km2", 
                    "loss_immediate_percent_86", 
                  "totloss_3y_percent_86", "totloss_5y_percent_86")

df_muni_year %>% 
  filter(!is.na(tot_transition_km2),  
         dist_statecapital_km >0, muni_code %in% all_of(keep_muni)) %>% 
  select(all_of(var_response), all_of(var_timeconstant), all_of(var_timevary)) %>%
  mutate(adate = as.Date(paste(year,"-01", "-01", sep="")), 
         format = c("%Y-%m-%d")) -> dfgam
which(is.na(dfgam)[,3]) #2176 salary values

#new variables
dfgam %>% 
  mutate(flag_gold = 
           factor(if_else(process_gold_p1000 >0,1,0))) -> dfgam
dfgam$muni_namef <- as.factor(dfgam$muni_name) 
dfgam$state_namef <- as.factor(dfgam$state_name)
dfgam$flag_urbanf <- as.factor(dfgam$flag_urban)
dfgam$yearf <- as.factor(dfgam$year)
dfgam$muni_factor <- paste(dfgam$state_name, dfgam$muni_name, sep = "_")
dfgam$muni_factor <- as.factor(dfgam$muni_factor)
dfgam$log_gdp_percapita_reais <- log(dfgam$gdp_percapita_reais)
dfgam$log_gva_percapita_reais <- log(dfgam$gva_agri_percapita_reais)
dfgam$log_min_salary_mean <- log1p(dfgam$min_salary_mean)
dfgam$gdp_percapita_usd = dfgam$gdp_percapita_reais / 3.946
dfgam$gva_agri_percapita_usd = dfgam$gva_agri_percapita_reais / 3.946

#Export for further use
saveRDS(dfgam, "data/dfgam.rds")


