library(tidyverse)
library(readxl)
library(scales)
library(mgcv)
library(stringi)

#google: r cran purtest example tutorial interpretation
#stationarity https://kevinkotze.github.io/ts-6-tut/
munin <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\AmazonConservation\\data\\bla_municipalities.xlsx"
df_muni <- read_excel(munin, 
                                   na = c("", "NA"),
                                   sheet = "municipality_fixed_ref",
                                   .name_repair = "universal")
df_muni_year <- read_excel(munin, 
                      na = c("", "NA"),
                      sheet = "municipality_annual",
                      .name_repair = "universal")

#list with missing mapbiomas
#df_muni_year %>% 
#  filter(is.na(forestcover_2002_percent_85)) %>% 
#  group_by(state_name, muni_name, muni_code) %>% 
#  summarise(acount = n()) %>% data.frame() %>% 
#  pull(muni_code) %>% as.character() #character(0)

#municipalities to include
df_muni %>% filter(flag_include == "yes") %>% pull(muni_code) -> keep_muni #794

#include presidents to sgregate timeseries
#presidents https://en.wikipedia.org/wiki/List_of_presidents_of_Brazil
pres_names <- c("Collor do Mello", "Franco", "Cardoso", "Lula", 
                "Rousseff", "Temer", "Bolsonaro")
pres_start <- c("15/03/1990", "02/10/1992", "01/01/1995", "01/03/2003", 
                "01/01/2011", "12/05/2016", "01/01/2019")
pres_end <- c("01/10/1992", "31/12/1994", 
              "31/12/2002", "31/12/2010", "11/05/2016", 
              "31/12/2018", "31/12/2021")
pres_party <- c("PRN", "PMDB", "PSDB", "PT", "PT", "MDB", "PSL/PL")
pres_party_direction <- c(NA, "center", "right-wing", "left_wing", 
                          "left-wing", "center", "right-wing")
df_presidents <- data.frame(pres_names = pres_names,
                            pres_party = pres_party, 
                            pres_party_direction = pres_party_direction,
                            pres_start = pres_start, 
                            pres_end = pres_end)

df_year_presidents <- data.frame(year = c(2002, 2003:2010, 2011:2016, 
                                          2017:2018, 2019), 
           president = c("c_cardoso", rep("a_lula",8), rep("b_rousseff", 6), 
                         rep("c_temer", 2), "c_bolsonaro"), 
           pres_group = c("other", rep("a_lula",8), rep("b_rousseff", 6), 
                          rep("other",3))
)
df_muni_year %>% left_join(df_year_presidents) -> df_muni_year
df_muni_year %>% 
  arrange(state_name, muni_name) %>% 
  group_by(state_name, muni_name) %>% 
  mutate(lag01_gva_agri = lag(gva_agri_percapita_reais, order_by = year), 
         lag01_gdp = lag(gva_agri_percapita_reais, order_by = year)) %>% 
  ungroup() -> df_muni_year
#df_muni_year %>% filter(is.na(dist_statecapital_km)) #0
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


#models
#mgcv timeseries
#https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
#https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
#https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
## gva and gdp lags do not improve model
names(df_muni_year)
var_response <- c("gdp_percapita_reais",  
                  "salary_mean_reais", "min_salary_mean")
var_timeconstant <- c("state_name", "muni_name", "muni_area_km2", 
                      "long", "lat" ,"dist_statecapital_km", 
                      "flag_urban", "indigenous_area_percent")
var_timevary <- c("year","pop_dens_km2", "tot_pop", 
                  "tot_transition_km2",
                  "mine_area_km2_construction_percapita", "mine_area_km2_gold_percapita",        
                  "mine_area_km2_metal_percapita",  "mine_area_km2_calcium_percapita",     
                  "process_construction_p1000", "process_gold_p1000",                  
                  "process_metal_p1000",  "process_calcium_p1000",
                  "gva_agri_percapita_reais", "gva_industry_percent", 
                  "main_sector", "dominant_sectors",
                  "school_per1000", "superior_course_per1000", "pg_per1000", 
                  "president", "pres_group", 
                  "loss_immediate_percent", "tot_loss3y_percent", 
                  "tot_loss5y_percent", "loss_immediate_km2", 
                  "tot_loss3y_km2", "tot_loss5y_km2", 
                  "forestcover_med_km_X1986",  "loss_immediate_percent_86", 
                  "totloss_3y_percent_86", "totloss_5y_percent_86")
var_lags <- c("lag01_lossarea_per", "lag02_lossarea_per", "lag03_lossarea_per", 
              "lag04_lossarea_per", "lag05_lossarea_per", "lag06_lossarea_per", 
              "lag07_lossarea_per", "lag08_lossarea_per", "lag09_lossarea_per", 
              "lag10_lossarea_per", 
              "lag01_lossarea_km2", "lag02_lossarea_km2", "lag03_lossarea_km2", 
              "lag04_lossarea_km2", "lag05_lossarea_km2", "lag06_lossarea_km2", 
              "lag07_lossarea_km2", "lag08_lossarea_km2", "lag09_lossarea_km2", 
              "lag10_lossarea_km2")

# Exclude new muni and state capitals
# Missing 4 (all formally installed after 2002)
#df_muni_cagr %>% filter(codmun7 == 5104526) #Ipiranga do Norte / Tapurah (MT)
#df_muni_cagr %>% filter(codmun7 == 5104542) #Itanhangá / Tapurah (MT)
#df_muni_cagr %>% filter(codmun7 == 1504752) # Mojuí dos Campos / Santarém (PA)

df_muni_year %>% 
  filter(!is.na(tot_transition_km2), !is.na(school_per1000), 
         !is.na(superior_course_per1000), !is.na(pg_per1000), 
         dist_statecapital_km >0, muni_code %in% all_of(keep_muni)) %>% 
  select(all_of(var_response), all_of(var_timeconstant), all_of(var_timevary), 
         all_of(var_lags)) %>%
  mutate(
         adate = as.Date(paste(year,"-01", "-01", sep="")), 
         format = c("%Y-%m-%d")) -> dfgam
which(is.na(dfgam)[,3]) #2176 salary values

#reclassify based on GDP levels
dfgam %>% 
  mutate(dominant_groups = case_when(dominant_sectors %in% 
                                       c("admin_agri", "admin_services", 
                                         "admin_industry") ~ "1", 
                                     dominant_sectors %in% c("agri_admin", 
                                                             "services_admin", 
                                                             "admin") ~"2", 
                                     dominant_sectors %in% c("industry_admin", 
                                                             "industry_agri", 
                                                             "services_industry", 
                                                             "industry_services") ~"3", 
                                     dominant_sectors %in% c("agri_industry", 
                                                             "services") ~"4", 
                                     dominant_sectors %in% c("agri", 
                                                             "services_agri")~"5", 
                                     dominant_sectors %in% c("agri_services", 
                                                             "industry")~"6", 
                                     TRUE ~ NA_character_ 
  ) 
  ) -> dfgam
dfgam %>% 
  mutate(dominant_groupsf2 = 
           factor(ifelse(dfgam$dominant_groups %in% c("1", "2"), 
                         "less", "more")), 
         flag_gold = 
           factor(if_else(mine_area_km2_gold_percapita >0,1,0))) -> dfgam
dfgam$muni_namef <- as.factor(dfgam$muni_name) 
dfgam$state_namef <- as.factor(dfgam$state_name)
dfgam$flag_urbanf <- as.factor(dfgam$flag_urban)
dfgam$pres_groupf <- as.factor(dfgam$pres_group)
dfgam$yearf <- as.factor(dfgam$year)
dfgam$muni_factor <- paste(dfgam$state_name, dfgam$muni_name, sep = "_")
dfgam$muni_factor <- as.factor(dfgam$muni_factor)
levels(dfgam$pres_groupf)#left wing Lula is the reference level 
dfgam$main_sectorf <- as.factor(dfgam$main_sector)
dfgam$dominant_sectorsf <- as.factor(dfgam$dominant_sectors) 
dfgam$dominant_groupsf <- as.factor(dfgam$dominant_groups)
dfgam$log_gdp_percapita_reais <- log(dfgam$gdp_percapita_reais)
dfgam$log_gva_percapita_reais <- log(dfgam$gva_agri_percapita_reais)
dfgam$gdp_percapita_usd = dfgam$gdp_percapita_reais / 3.946
dfgam$gva_agri_percapita_usd = dfgam$gva_agri_percapita_reais / 3.946

#Export for further use
saveRDS(dfgam, "dfgam.rds")
dfgam <- readRDS("dfgam.rds")

#GDP and forest loss
dfgam %>% 
  ggplot(aes(x=tot_loss_percent, y = log(gdp_percapita_reais), 
             colour= factor(year))) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_color_viridis_d("year") +
  facet_wrap(~state_name, ncol = 1) + 
  labs(title = "forest loss", 
       subtitle = "annual",
       x= "loss (% of municipality area)", 
       y = "GDP percapita (Reais, log transformed)") + 
  theme(legend.position="bottom") + 
  guides(col = guide_legend(nrow = 4))

#3 year cumulative
dfgam %>% 
  ggplot(aes(x=tot_loss3y_percent, y = log(gdp_percapita_reais), 
             colour= factor(year))) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_color_viridis_d("year") +
  facet_wrap(~state_name, ncol = 1) + 
  labs(title = "forest loss", 
       subtitle = "cumulative 3 year",
       x= "loss (% of municipality area)", 
       y = "GDP percapita (Reais, log transformed)") + 
  theme(legend.position="bottom") + 
  guides(col = guide_legend(nrow = 4))

#5 year cumulative
dfgam %>% 
  ggplot(aes(x=tot_loss5y_percent, y = log(gdp_percapita_reais), 
             colour= factor(year))) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_color_viridis_d("year") +
  facet_wrap(~state_name, ncol = 1) + 
  labs(title = "forest loss", 
       subtitle = "cumulative 5 year",
       x= "loss (% of municipality area)", 
       y = "GDP percapita (Reais, log transformed)") + 
  theme(legend.position="bottom") + 
  guides(col = guide_legend(nrow = 4))

#correlations with time varying covariates
#Pairs panel with human readable names
pairs_vars <- c('gdp_percapita_reais', 
                'gva_agri_percapita_reais', 'tot_loss_percent',
                'tot_loss3y_percent', 'tot_loss5y_percent')
#untransformed
dfgam %>%
  select(all_of(pairs_vars)) %>%
  rename(GDP = gdp_percapita_reais, 
         GVA_agri = gva_agri_percapita_reais, 
         loss_annual = tot_loss_percent,
         loss_3y = tot_loss3y_percent,
         loss_5y = tot_loss5y_percent) %>%
  psych::pairs.panels()

#log transformed
dfgam %>%
  select(all_of(pairs_vars)) %>%
  rename(GDP = gdp_percapita_reais, 
         GVA_agri = gva_agri_percapita_reais, 
         loss_annual = tot_loss_percent,
         loss_3y = tot_loss3y_percent,
         loss_5y = tot_loss5y_percent) %>% 
  mutate(GDP_log = log(GDP), GVA_agri_log = log(GVA_agri)) %>% 
  select(GDP_log, GVA_agri_log, loss_annual, loss_3y, loss_5y) %>%
  psych::pairs.panels()

#Subset to develop models
unique(dfgam$state_name)
dfgam[which(dfgam$gdp_percapita_reais == max(dfgam$gdp_percapita_reais)), 
      'state_name']
dfgam[which(dfgam$gdp_percapita_reais == min(dfgam$gdp_percapita_reais)), 
      'state_name']
dfgam %>% 
  filter(state_name %in% c("Amapá", "Pará", 
                           "Maranhão")) -> dfgam_test

# dominant sectors
dfgam %>% 
  group_by(dominant_sectors) %>% 
  summarise(count_obs = n(), 
            count_state = length(unique(state_name)), 
            count_muni = length(unique(muni_factor)), 
            gdp_median = median(gdp_percapita_reais), 
            gdp_q95 =  quantile(gdp_percapita_reais, probs = 0.95), 
            gdp_max = max(gdp_percapita_reais)) %>% 
  arrange(desc(gdp_max))

dfgam %>% 
  ggplot(aes(x=fct_reorder(dominant_sectors, gdp_percapita_reais, max), 
             y = log(gdp_percapita_reais))) + 
  geom_point() +
  geom_violin() + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) + 
  coord_flip() +
  labs(x="dominant sectors", 
       y = "GDP percapita (Reais)") +
  theme(
    legend.position = "none"
  )

dfgam %>% 
  ggplot(aes(x=fct_reorder(dominant_sectors, gdp_percapita_reais, max), 
             y = log(gdp_percapita_reais))) + 
  geom_point() +
  geom_violin(draw_quantiles = 0.5) + 
  coord_flip() + 
  facet_wrap(~state_name) +
  labs(x="dominant sectors", 
       y = "GDP percapita (Reais, log transform)") +
  theme(
    legend.position = "none"
  )

#reclassify based on GDP levels
dfgam %>% 
  ggplot(aes(x=dominant_groups, 
             y = gdp_percapita_reais)) +
  geom_violin() + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) + 
  coord_flip() +
  labs(x="dominant groups", 
       y = "GDP percapita (Reais)") +
  theme(
    legend.position = "none"
  )

dfgam %>% 
  ggplot(aes(x=year, y = gdp_percapita_reais, colour = dominant_groups)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) + 
  scale_color_viridis_d()

dfgam %>% 
  ggplot(aes(x=year, y = log(gdp_percapita_reais), 
             colour = dominant_groups)) + 
  geom_jitter(width=0.2, height = 0, size = 0.7) + 
  stat_smooth(method = "gam", se = FALSE) + 
  scale_color_viridis_d()

dfgam %>% 
  group_by(dominant_groups) %>% 
  summarise(count_obs = n(), 
            count_state = length(unique(state_name)), 
            count_muni = length(unique(muni_factor)), 
            gdp_median = median(gdp_percapita_reais), 
            gdp_q95 =  quantile(gdp_percapita_reais, probs = 0.95), 
            gdp_max = max(gdp_percapita_reais)) %>% 
  arrange(desc(gdp_max))

dfgam %>% 
  group_by(dominant_groupsf2) %>% 
  summarise(count_obs = n(), 
            count_state = length(unique(state_name)), 
            count_muni = length(unique(muni_factor)), 
            gdp_median = median(gdp_percapita_reais), 
            gdp_q95 =  quantile(gdp_percapita_reais, probs = 0.95), 
            gdp_max = max(gdp_percapita_reais)) %>% 
  arrange(desc(gdp_max))


#find which state missing more = Roraima.
dfgam %>% 
  filter(dominant_groupsf2 == "more") %>% 
  group_by(state_name) %>% 
  summarise(acount = n())

dfgam %>% filter(state_name == "Roraima") %>%
  group_by(dominant_groups, dominant_sectors) %>% 
  summarise(count_obs = n(), 
            count_state = length(unique(state_name)), 
            count_muni = length(unique(muni_factor)), 
            gdp_median = median(gdp_percapita_reais), 
            gdp_q95 =  quantile(gdp_percapita_reais, probs = 0.95), 
            gdp_max = max(gdp_percapita_reais)) %>% 
  arrange(desc(gdp_max))

#2019 summaries. reference levels .......
df_muni_year %>% 
  filter(!is.na(tot_forest_cover_2019_percent), year == "2019") %>% 
  ggplot(aes(x=tot_forest_cover_2019_percent, y=salary_mean_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam", se = FALSE) + 
  scale_y_continuous(lim=c(0,1100), 
                     breaks = c(0,200, 400, 600, 800, 1000)) + 
  labs(title= "(A)", 
       x = "forest cover 2019 (% of municipality area)", 
       y = "mean monthly salary (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_salary_forest
fig_2019_salary_forest
#Export
png(file = "figures//fig_2019_salary_forest.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_salary_forest
dev.off()

df_muni_year %>% 
  filter(!is.na(tot_forest_cover_2019_percent), year == "2019") %>% 
  ggplot(aes(x=tot_forest_cover_2019_percent, 
             y=(employed_informal/employed_total)*100)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(lim=c(0, 100)) +
  labs(title= "(B)", 
       x = "forest cover 2019 (% of municipality area)", 
       y = "informal employment (% of workforce)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_informal_forest
fig_2019_informal_forest
#Export
png(file = "figures//fig_2019_informal_forest.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_informal_forest
dev.off()

df_muni_year %>% 
  filter(!is.na(tot_forest_cover_2019_percent), year == "2019") %>% 
  ggplot(aes(x=tot_forest_cover_2019_percent, y=gdp_percapita_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +  
  labs(title= "(C)", 
       x = "forest cover 2019 (% of municipality area)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_gdp_forest
fig_2019_gdp_forest
#Export
png(file = "figures//fig_2019_gdp_forest.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_gdp_forest
dev.off()

df_muni_year %>% 
  filter(!is.na(tot_forest_cover_2019_percent), year == "2019") %>% 
  ggplot(aes(x=tot_forest_cover_2019_percent, 
             y=gva_agri_percapita_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +  
  labs(title= "(D)", 
       x = "forest cover 2019 (% of municipality area)", 
       y = "GVA agriculture per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_gva_forest
fig_2019_gva_forest  
#Export
png(file = "figures//fig_2019_gva_forest.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_gva_forest
dev.off()

#forest loss
df_muni %>% 
  left_join(df_muni_year %>% 
              filter(year == "2019") %>% 
              select(muni_name, salary_mean_reais)) %>%
  filter(!is.na(tot_loss_percent)) %>% 
  ggplot(aes(x=tot_loss_percent, y=salary_mean_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(lim=c(0,1100), 
                     breaks = c(0,200, 400, 600, 800, 1000)) + 
  labs(title= "(E)", 
       x = "forest loss 2002-2019 (% of municipality area)", 
       y = "mean monthly salary (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_salary_forestloss
fig_2019_salary_forestloss

#Export
png(file = "figures//fig_2019_salary_forestloss.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_salary_forestloss
dev.off()
#informal
df_muni %>% 
  left_join(df_muni_year %>% 
              filter(year == "2019") %>% 
              mutate(informal_per = (employed_informal/employed_total)*100) %>%
              select(muni_name, informal_per)) %>%
  filter(!is.na(tot_loss_percent)) %>% 
  ggplot(aes(x=tot_loss_percent, y=informal_per)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(lim=c(0, 100)) +
  labs(title= "(F)", 
       x = "forest loss 2002-2019 (% of municipality area)", 
       y = "informal employment (% of workforce)") +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_informal_forestloss
  fig_2019_informal_forestloss

#Export
png(file = "figures//fig_2019_informal_forestloss.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_informal_forestloss
dev.off()

df_muni %>% 
  filter(!is.na(tot_forest_cover_2019_percent)) %>% 
  ggplot(aes(x=tot_loss_percent, 
             y=gdp_percapita_reais_2019/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam", method.args = list(family = "tw")) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +  
  labs(title= "(G)", 
       x = "forest loss 2002-2019 (% of municipality area)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_gdp_forestloss
fig_2019_gdp_forestloss
#Export
png(file = "figures//fig_2019_gdp_forestloss.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_gdp_forestloss
dev.off()
#GVA agri forestloss
df_muni %>% 
  left_join(df_muni_year %>% 
              filter(year == "2019") %>% 
              select(muni_name, gva_agri_percapita_reais)) %>%
  filter(!is.na(tot_forest_cover_2019_percent)) %>% 
  ggplot(aes(x=tot_loss_percent, 
             y=gva_agri_percapita_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam", method.args = list(family = "tw")) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +  
  labs(title= "(H)", 
       x = "forest loss 2002-2019 (% of municipality area)", 
       y = "GVA agriculture per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_gva_forestloss
fig_2019_gva_forestloss  
#Export
png(file = "figures//fig_2019_gva_forestloss.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_gva_forestloss
dev.off()


#Annual patterns
#757 municipalities
df_muni_year %>% 
  select(muni_name, muni_area_km2, year, gdp_percapita_reais, tot_loss_percent, 
         school_per1000, superior_course_per1000, pg_per1000) %>% 
  filter(!is.na(tot_loss_percent), !is.na(school_per1000)) %>% 
  group_by(muni_name, muni_area_km2) %>% 
  summarise(acount = n(), 
            count_year = length(unique(year))) %>% 
  arrange(desc(count_year)) %>% 
  ungroup() %>% pull(muni_area_km2) %>% sum() #[1] 5030055

df_muni_year %>% 
  select(state_name, muni_name, muni_area_km2, year, 
         gdp_percapita_reais, tot_loss_percent, 
         school_per1000, superior_course_per1000, pg_per1000) %>% 
  filter(!is.na(tot_loss_percent), !is.na(school_per1000)) %>% 
  ggplot(aes(x=year, y = school_per1000)) + 
  geom_point() + 
  #stat_smooth(method="gam") + 
  stat_smooth(aes(colour = state_name), method="gam")


df_muni_year %>%
ggplot(aes(x=gva_agri_percapita_reais/3.946, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_x_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d() + 
  facet_wrap(~flag_urban) +
  labs(title= "(A)", 
       x = "GVA agriculture per capita (US$)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2))-> fig_gdp_agri_2002_2019
fig_gdp_agri_2002_2019

#Export
png(file = "figures//fig_gdp_agri_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_agri_2002_2019
dev.off()

df_muni_year %>%
  ggplot(aes(x=tot_loss_percent, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(B)", 
       x = "Forest transition (%)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_gdp_forest_2002_2019
fig_gdp_forest_2002_2019
#Export
png(file = "figures//fig_gdp_forest_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_forest_2002_2019
dev.off()

df_muni_year %>%
  ggplot(aes(x=tot_loss_percent, y=gva_agri_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(C)", 
       x = "Forest transition (%)", 
       y = "GVA agriculture per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_agri_forest_2002_2019
fig_agri_forest_2002_2019
#Export
png(file = "figures//fig_agri_forest_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_agri_forest_2002_2019
dev.off()

df_muni_year %>%
  #ggplot(aes(x=pass_rate_per, y=gdp_percapita_reais/3.946)) + 
  ggplot(aes(x=school_per1000, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3, 
                                                  accuracy = 1.0)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(D)", 
       x = "schools per 1000", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_gdp_school_2002_2019
fig_gdp_school_2002_2019
#Export
png(file = "figures//fig_gdp_school_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_school_2002_2019
dev.off()

df_muni_year %>%
  ggplot(aes(x= superior_course_per1000, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3, 
                                                  accuracy = 1.0)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(E)", 
       x = "post secondary courses per 1000", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_gdp_ps_2002_2019
fig_gdp_ps_2002_2019
#Export
png(file = "figures//fig_gdp_ps_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_ps_2002_2019
dev.off()

df_muni_year %>%
  mutate(pg_per1000 = count_pg_course/(tot_pop/1000)) %>%
  ggplot(aes(x=pg_per1000, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3, 
                                                  accuracy = 1.0)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(F)", 
       x = "post graduate courses per 1000", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_gdp_pg_2002_2019
fig_gdp_pg_2002_2019
#Export
png(file = "figures//fig_gdp_pg_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_pg_2002_2019
dev.off()

df_muni %>% 
  filter(!is.na(tot_forest_cover_2019_percent)) %>%
ggplot(aes(x = tot_forest_cover_2019_percent, y = gdp_percapita_reais_2019)) +
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous("GDP per capita (thousands Reais)", 
                     labels = unit_format(unit = "K", 
                                          scale = 1e-3, 
                                          accuracy = 0.1)) +
  labs(title = "GDP and forest cover 2019", 
       x = "remaining forest cover (% of municipality)") + 
  theme(plot.title.position = "plot") -> fig_gdp_variation
fig_gdp_variation
png(file = "figures//fig_gdp_variation.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_gdp_variation + theme(text = element_text(size = 16))
dev.off()
  
df_muni %>% 
  filter(!is.na(tot_forest_cover_2019_percent)) %>%
  ggplot(aes(x = school_per1000, y = gdp_percapita_reais_2019)) +
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous("GDP per capita (thousands Reais)", 
                     labels = unit_format(unit = "K", 
                                          scale = 1e-3, 
                                          accuracy = 0.1)) +
  labs(title = "GDP and forest cover 2019", 
       x = "schools per 1000") + 
  theme(plot.title.position = "plot")   

df_muni %>% 
  filter(!is.na(tot_forest_cover_2019_percent)) %>%
  ggplot(aes(x = pass_rate_per, y = gdp_percapita_reais_2019)) +
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous("GDP per capita (thousands Reais)", 
                     labels = unit_format(unit = "K", 
                                          scale = 1e-3, 
                                          accuracy = 0.1)) +
  labs(title = "GDP and forest cover 2019", 
       x = "school pass rate") + 
  theme(plot.title.position = "plot") 

#models
#mgcv timeseries
#https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
#https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
#https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
## gva and gdp lags do not improve model
var_response <- c("gdp_percapita_reais")
var_timeconstant <- c("state_name", "muni_name", "muni_area_km2", "dist_statecapital_km", 
         "flag_urban")
var_timevary <- c("year","pop_dens_km2", "tot_loss_percent", 
                  "gold_area_km2_percapita",
                  "gva_agri_percapita_reais", "gva_industry_percent", 
                  "main_sector",
                  "school_per1000", "superior_course_per1000", "pg_per1000", 
                  "president", "pres_group")
var_lags <- c("lag01_lossarea_per", "lag02_lossarea_per", "lag03_lossarea_per", 
              "lag04_lossarea_per", "lag05_lossarea_per", "lag06_lossarea_per", 
              "lag07_lossarea_per", "lag08_lossarea_per", "lag09_lossarea_per", 
              "lag10_lossarea_per")
high_gdp_muni <- c("Vitória do Xingu", 
                   "Canaã dos Carajás", "Campos de Júlio")
df_muni_year %>% 
  filter(!is.na(tot_loss_percent), !is.na(school_per1000), 
         !is.na(superior_course_per1000), !is.na(pg_per1000), 
         dist_statecapital_km >0) %>% 
  select(all_of(var_response), all_of(var_timeconstant), all_of(var_timevary), 
         all_of(var_lags)) %>% 
  mutate(tot_loss3y_percent = lag01_lossarea_per + 
           lag02_lossarea_per + lag03_lossarea_per, 
         tot_loss5y_percent = lag01_lossarea_per + 
           lag02_lossarea_per + lag03_lossarea_per + 
           lag04_lossarea_per + lag05_lossarea_per, 
         adate = as.Date(paste(year,"-01", "-01", sep="")), 
         format = c("%Y-%m-%d")) -> dfgam
which(is.na(dfgam)[,3]) #0 no nulls
dfgam$muni_namef <- as.factor(dfgam$muni_name) 
dfgam$state_namef <- as.factor(dfgam$state_name)
dfgam$flag_urbanf <- as.factor(dfgam$flag_urban)
dfgam$pres_groupf <- as.factor(dfgam$pres_group)
dfgam$yearf <- as.factor(dfgam$year)
dfgam$muni_factor <- paste(dfgam$state_name, dfgam$muni_name, sep = "_")
dfgam$muni_factor <- as.factor(dfgam$muni_factor)
levels(dfgam$pres_groupf)#left wing Lula is the reference level 
dfgam$main_sectorf <- as.factor(dfgam$main_sector)
saveRDS(dfgam, "dfgam.rds")
dfgam <- readRDS("dfgam.rds")



#Subset to develop models
unique(dfgam$state_name)
dfgam[which(dfgam$gdp_percapita_reais == max(dfgam$gdp_percapita_reais)), 
            'state_name']
dfgam[which(dfgam$gdp_percapita_reais == min(dfgam$gdp_percapita_reais)), 
      'state_name']
dfgam %>% 
  filter(state_name %in% c("Amapá", "Pará", 
                           "Maranhão")) -> dfgam_test

table(dfgam$flag_urban)
#library(corrgram) -18 (lag 1,2,3)
corrgram(dfgam[, c(var_response, "tot_loss_percent", var_lags)],
         lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)
cor.test(dfgam$gdp_percapita_reais, 
         dfgam$school_per1000) #0.07020494
cor.test(dfgam$gdp_percapita_reais, 
         dfgam$superior_course_per1000) #- 0.004 NS
cor.test(dfgam$gdp_percapita_reais, 
         dfgam$pg_per1000) #0.078
psych::pairs.panels(dfgam[, ])

#Model
#Need to use log as there are (5 or so) extreme outlier gdp_percapita values 
test<- EnvStats::boxcox(dfgam$gdp_percapita_reais)
hist(test$data)
testlog <- log(dfgam$gdp_percapita_reais)
hist(testlog)
#lags (gdp and gva) do not improve model
memory.limit(30000)#needed to run gam.check
myctrl <- list(keepData = TRUE)
model_00 <- gam(log(gdp_percapita_reais) ~ year*flag_urbanf +
                  pres_groupf + 
                  s(year, by = state_namef, k=5, m=1, bs="tp") +
                  s(gva_agri_percapita_reais) +
                  s(dist_statecapital_km, by = state_namef) + 
                  s(state_namef, bs="re"), 
                data = dfgam_test, 
                family = "tw",
                method="REML", 
                control = myctrl)
gam.check(model_00) 
summary(model_00)
plot(model_00, scale = 0)
saveRDS(model_00, "model_00.rds")
model_00 <- readRDS("model_00.rds")

model_00 <- gam(log(gdp_percapita_reais) ~ year*flag_urbanf +
                  pres_groupf + 
                  s(yearf, bs="re") +
                  s(pop_dens_km2) +
                  s(tot_loss5y_percent) +
                  s(gva_agri_percapita_reais) +
                  s(school_per1000) + 
                   s(pg_per1000) + 
                  s(dist_statecapital_km, by = state_namef), 
                 data = dfgam, 
                family = "tw",
                method="REML", 
                control = myctrl)
gam.check(model_00) 
summary(model_00)
plot(model_00, scale = 0)
saveRDS(model_00, "model_00.rds")
model_00 <- readRDS("model_00.rds")

#
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B", 
             maxIter = 99, msMaxIter = 99, keepData = TRUE)
model_01 <- gamm(log(gdp_percapita_reais) ~ year*flag_urbanf +
                   pres_groupf +
                   s(pop_dens_km2) +
                   s(tot_loss5y_percent) +
                   s(gva_agri_percapita_reais) +
                   s(school_per1000) + 
                   s(pg_per1000) + 
                   s(dist_statecapital_km, by = state_namef), 
                 data = dfgam, 
                 method="REML" 
                 )
summary(model_01$lme)
library(forecast)
arma_res <- auto.arima(resid(model_01$lme, type = "normalized"),
                       stationary = TRUE, seasonal = FALSE)

#arma_res$coef #without president
#       ar1        ar2        ar3        ma1        ma2 
#0.2820864 -0.3996548  0.8024361  0.5823226  0.9371343 
#with president
arma_res$coef
#        ar1         ma1 
#0.88137497 -0.03356724 
memory.limit(30000)#needed to speed up models and run gam.check
# AR test
model_01ar1_test <-gamm(log(gdp_percapita_reais) ~ year*flag_urbanf +
      pres_groupf + 
      s(year, by = state_namef, k=5, m=1, bs="tp") +
      s(gva_agri_percapita_reais) + 
        s(pop_dens_km2) +
        s(tot_loss5y_percent) +
        s(school_per1000) + 
        s(pg_per1000) + 
      s(dist_statecapital_km, by = state_namef) + 
      s(state_namef, bs="re"), 
    correlation = corARMA(form = ~ year|muni_namef, p = 1), 
    data = dfgam_test, 
    method="REML", 
    control = ctrl)
saveRDS(model_01ar1_test, "model_01ar1_test.rds")
model_01ar1_test <- readRDS("model_01ar1_test.rds")
summary(model_01ar1_test$lme) 
summary(model_01ar1_test$gam)

#GAMM AR ...
dfgam$muni_factor <- paste(dfgam$state_name,dfgam$muni_name, sep = "_")
dfgam$muni_factor <- as.factor(dfgam$muni_factor)
model_01_ar1 <- gamm(log(gdp_percapita_reais) ~ year*flag_urbanf +
                             pres_groupf + 
                             s(year, by = state_namef, k=5, m=1, bs="tp") +
                             s(gva_agri_percapita_reais) + 
                             s(pop_dens_km2) +
                             s(tot_loss5y_percent) +
                             s(school_per1000) + 
                             s(pg_per1000) + 
                             s(dist_statecapital_km, by = state_namef) + 
                             s(state_namef, bs="re"), 
                           correlation = corARMA(form = ~ year|muni_factor, p = 1), 
                           data = dfgam, 
                           method="REML", 
                           control = ctrl)
saveRDS(model_01_ar1, "model_01_ar1.rds")
model_01_ar1 <- readRDS("model_01_ar1.rds")
summary(model_01_ar1$lme) 
summary(model_01_ar1$gam) #r2 = 0.83
gam.check(model_01_ar1$gam) #problem is with residual > 1

#Compare models
anova(model_01$lme)

#residuals
#Add residuals to model data.frame
res_gam <- resid(model_00, type = "deviance")
hist(res_gam)
#df_00 <- model_00$data[,1:11]
dfgam$m00_res_gam <- res_gam

dfgam$m01_res_gamm <- res_gamm
dfgam$m01_res_gamm_ar1 <- res_gamm_ar1

#AR test
res_gamm_art <- resid(model_01ar1_test$lme, type = "normalized")
hist(res_gamm_art)
df_art <- model_01ar1_test$lme$data[,1:11]
df_art$m01_res_gamm_art <- res_gamm_art

#Ar1
res_gamm_lme <- resid(model_01$lme, type = "normalized")
res_gamm_gam <- resid(model_01$gam, type = "deviance")
res_gamm_ar1 <- resid(model_01_ar1$lme, type = "normalized")
hist(res_gamm_ar1)
df_ar1 <- model_01_ar1$lme$data[,1:13]
df_ar1$m01_res_gamm_ar1 <- res_gamm_ar1

#AR2
res_gamm_ar2 <- resid(model_01_ar2$lme, type = "normalized")
df_ar2 <- model_01_ar2$lme$data[,1:11]
df_ar2$m01_res_gamm_ar2 <- res_gamm_ar2
#AR3
res_gamm_ar3 <- resid(model_01_ar3$lme, type = "normalized")
df_ar3 <- model_01_ar3$lme$data[,1:11]
df_ar3$m01_res_gamm_ar3 <- res_gamm_ar3

library(timetk)
df_ar1 %>%
  group_by(state_namef, dist_statecapital_km) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = m01_res_gamm_ar1, 
    .lags = 11
  ) -> tidy_acf

#export as .png  250 * 1000
tidy_acf %>% 
  ggplot(aes(x = lag, y = PACF, color = state_namef, 
             group = state_namef)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  # Add cutoffs
  geom_line(aes(y = .white_noise_upper), color = "black", 
            linetype = 2) +
  geom_line(aes(y = .white_noise_lower), color = "black", 
            linetype = 2) +
  # Add facets
  facet_wrap(~ state_namef, ncol = 1) +
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) + labs(
    title = "Partial AutoCorrelation (PACF)",
    subtitle = "GAMM AR(1) residuals", 
    x = "lag (year)"
  )

#How many lags of GVA / five year forest loss to use??
#Cross-correlations
names(df_ar2)
df_ar2 %>%
  group_by(state_namef, dist_statecapital_km) %>% 
  tk_acf_diagnostics(
    .date_var = year,
    .value = `log(gdp_percapita_reais)`,
    .ccf_vars = gva_agri_percapita_reais, 
    .lags = 11
  ) -> tidy_ccf_gdp_agri

df_ar2 %>%
  group_by(state_namef, dist_statecapital_km) %>% 
  tk_acf_diagnostics(
    .date_var = year,
    .value = `log(gdp_percapita_reais)`,
    .ccf_vars = c(gva_agri_percapita_reais, tot_loss5y_percent, 
                  school_per1000), 
    .lags = 11
  ) -> tidy_ccf_gdp
  
#export as .png  250 * 1000
tidy_ccf_gdp %>% 
  filter(lag <11) %>%
  ggplot(aes(x = lag, y = CCF_school_per1000, 
             color = state_namef, 
             group = state_namef)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.7, linetype=2) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  # Add facets
  facet_wrap(~ state_namef, ncol = 1) +
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) + labs(
    title = "Cross Correlation", 
    subtitle = "GDP and schools",
    x = "lag (year)", 
    y = "correlation coefficient"
  )

#https://www.kaggle.com/janiobachmann/time-series-i-an-introductory-start/script
dfgam %>%
  group_by(state_name, muni_name) %>% 
  plot_acf_diagnostics(
    date,
    AAPL,
    .ccf_vars = GOOGL,
    .show_ccf_vars_only = TRUE,
    .interactive=FALSE, 
    .line_color = "black",
    .point_color =palette_light()[[2]],
    .line_size = 1.5,
    .title = "Cross Correlation of Technology Stocks"
  ) 
