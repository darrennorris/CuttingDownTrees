#
#packages
library(tidyverse)
library(readxl)
library(gridExtra)
library(timetk)
library(mgcv)
library(scales)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(ggspatial)
library(gratia)
memory.limit(30000)

# load data
#file from prep_analysis.R
dfgam <- readRDS("data/dfgam.rds") #14292 obs. 38 vars

#794 of 808 municipalities
#Excluding state capitals and those whose borders changed during the study period
dfgam %>% pull(muni_factor) %>% as.character() %>% unique() %>% length() #794
  
# Basic summaries ---------------------------------------------------------
#GDP increase over time
dfgam %>% group_by(year) %>% 
  summarise(gdp_median_reais = median(gdp_percapita_reais), 
            gdp_median_usd = median(gdp_percapita_reais) / 3.946) %>% 
  filter(year %in% c(2002, 2019)) %>% 
  mutate(lead_gdp = lead(gdp_median_usd)) %>% 
  mutate(gdp_diff = lead_gdp - gdp_median_usd, 
         gdp_inc = lead_gdp/gdp_median_usd)

#GVA agriculture increase over time
dfgam %>% group_by(year) %>% 
  summarise(gva_median_reais = median(gva_agri_percapita_usd * 3.946), 
            gva_median_usd = median(gva_agri_percapita_usd)) %>% 
  filter(year %in% c(2002, 2019)) %>% 
  mutate(lead_gva = lead(gva_median_usd)) %>% 
  mutate(gva_diff = lead_gva - gva_median_usd, 
         gva_inc = lead_gva/gva_median_usd)

#Salary increase over time
dfgam %>% group_by(year) %>% 
  filter(!is.na(min_salary_mean)) %>%
  summarise(median_salary = median(min_salary_mean)) %>% 
  filter(year %in% c(2006, 2019)) %>% 
  mutate(lead_salary = lead(median_salary)) %>% 
  mutate(salary_diff = lead_salary - median_salary, 
         salary_inc = lead_salary/median_salary)

#Salary in reais
dfgam %>% group_by(year) %>% 
  filter(!is.na(salary_mean_reais)) %>%
  summarise(median_salary = median(salary_mean_reais)) %>% 
  filter(year %in% c(2006, 2019)) %>% 
  mutate(lead_salary = lead(median_salary)) %>% 
  mutate( salary_usd = lead_salary / 3.946, 
          salary_diff = lead_salary - median_salary, 
         salary_inc = lead_salary/median_salary)

#Forest loss
count_muni <- length(unique(dfgam$muni_factor)) 
count_muni #794

dfgam %>% 
  group_by(state_name, muni_name, muni_area_km2) %>% 
  summarise(acount = n()) %>% ungroup() %>% 
  pull(muni_area_km2) %>% sum() -> tot_muni_area_km2 #4975904
tot_muni_area_km2 #4975904

# Useful values -----------------------------------------------------------

# Maximum annual forest loss
dfgam %>% 
  group_by(year) %>%
  summarise(area = sum(tot_transition_km2, na.rm=TRUE)) %>% 
  pull(area) %>% max() -> mapbiomasloss_all_max #[1] 24800.01
# Maximum annual GDP per capita
dfgam %>% 
  group_by(year) %>%
  summarise(area = median(gdp_percapita_usd, na.rm=TRUE)) %>% 
  pull(area) %>% max() -> gdp_percapita_usd_median_max #[1] 3401.44
# Maximum annual agricultre GVA per capita
dfgam %>% 
  group_by(year) %>%
  summarise(area = median(gva_agri_percapita_usd, na.rm=TRUE)) %>% 
  pull(area) %>% max() -> gva_percapita_usd_median_max #593.70
# Maximum annual salary
dfgam %>% 
  group_by(year) %>%
  summarise(area = median(min_salary_mean, na.rm=TRUE)) %>% 
  filter(!is.na(area)) %>%
  pull(area) %>% max() -> min_salary_mean_median_max #[1] 1.9


# Figure 2 ----------------------------------------------------------------

axis_trans_mapbiomasgva <- mapbiomasloss_all_max / gva_percapita_usd_median_max
axis_trans_mapbiomasall <- mapbiomasloss_all_max / gdp_percapita_usd_median_max
axis_trans_mapbiomassal <- mapbiomasloss_all_max / min_salary_mean_median_max

#data frame to hold labels
df_mapbiomasall_labels_gva <- data.frame(year = c(2019, 2016), 
                                     yaxis_value = mapbiomasloss_all_max +(mapbiomasloss_all_max*0.05),
                                     label_values = c(round(mapbiomasloss_all_max,0), 
                                                      round(gdp_percapita_usd_median_max,0)),
                                     label_values_gva = c(round(mapbiomasloss_all_max,0), 
                                                          round(gva_percapita_usd_median_max,0)))
df_mapbiomasall_labels_gdp <- data.frame(year = c(2019, 2019), 
                                         yaxis_value = c(mapbiomasloss_all_max +(mapbiomasloss_all_max*0.05), 
                                                         22000), 
                                         label_values_salary = c(round(mapbiomasloss_all_max,0), 
                                                                 round(min_salary_mean_median_max,2)), 
                                         label_values = c(round(mapbiomasloss_all_max,0), 
                                                          round(gdp_percapita_usd_median_max,0)) 
                                         )
#mycols_mapbiomas <- c("savanna" = "deeppink4", 
#                      "forest" = "deeppink")

dfgam %>% 
  group_by(year) %>% 
  summarise(tot_transition_km2 = sum(tot_transition_km2), 
            gva_per_capita_usd = median(gva_agri_percapita_usd)) %>%
  ggplot(aes(x=year, y=tot_transition_km2)) + 
  geom_col(fill="deeppink", colour="blue") + 
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="grey60", size = 1.5) +
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(aes(x=year, y=gva_per_capita_usd*axis_trans_mapbiomasgva), 
            size=2, color="black") + 
  scale_y_continuous(limits = c(0, 27000), 
                     #for the second y-axis
                     sec.axis = sec_axis(~./axis_trans_mapbiomasgva,#divided by transformation rate, in order to be represented based on the first y-axis
                                         name = "agriculture GVA per capita (US$)")) + 
  scale_x_continuous(limits = c(2001.5, 2019.5))  + 
  geom_label(data = df_mapbiomasall_labels_gva, 
             aes(x= year, y = yaxis_value, label = label_values_gva), 
             colour = c("blue", "black")) + 
  labs(title = "(A)", 
       y = bquote('forest loss'~(km^2)))  +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position = "top", legend.box = "horizontal") -> figa_loss_gva
figa_loss_gva

dfgam %>% 
  group_by(year) %>% 
  summarise(tot_transition_km2 = sum(tot_transition_km2), 
            gdp_per_capita_usd = median(gdp_percapita_usd)) %>%
  ggplot(aes(x=year, y=tot_transition_km2)) + 
  geom_col(fill="deeppink", colour="blue") + 
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="grey60", size = 1.5) +
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(aes(x=year, y=gdp_per_capita_usd*axis_trans_mapbiomasall), 
            size=2, color="black") + 
  scale_y_continuous(limits = c(0, 27000), 
                     #for the second y-axis
                     sec.axis = sec_axis(~./axis_trans_mapbiomasall,#divided by transformation rate, in order to be represented based on the first y-axis
                                         name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(2001.5, 2019.5))  + 
  geom_label(data = df_mapbiomasall_labels_gdp, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(title = "(B)", 
       y = bquote('forest loss'~(km^2)))  +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position = "top", legend.box = "horizontal") -> figb_loss_gdp
figb_loss_gdp

#salary
dfgam %>% 
  group_by(year) %>% 
  summarise(tot_transition_km2 = sum(tot_transition_km2), 
            min_salary_median = median(min_salary_mean, na.rm = TRUE)) %>%
  ggplot(aes(x=year, y=tot_transition_km2)) + 
  geom_col(fill="deeppink", colour="blue") + 
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="grey60", size = 1.5) +
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(aes(x=year, y=min_salary_median*axis_trans_mapbiomassal), 
            size=2, color="black") + 
  scale_y_continuous(limits = c(0, 27000), 
                     #for the second y-axis
                     sec.axis = sec_axis(~./axis_trans_mapbiomassal,#divided by transformation rate, in order to be represented based on the first y-axis
                                         name = "average salary")) + 
  scale_x_continuous(limits = c(2001.5, 2019.5))  + 
  geom_label(data = df_mapbiomasall_labels_gdp, 
             aes(x= year, y = yaxis_value, label = label_values_salary), 
             colour = c("blue", "black")) + 
  labs(title = "(C)", 
       y = bquote('forest loss'~(km^2)))  +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position = "top", legend.box = "horizontal") -> figa_loss_salary
figa_loss_salary

png(file = "data/figures//fig_loss_gdp_salary.png", 
    bg = "white", type = c("cairo"), 
    width=5000, height=8000, res = 600)
gridExtra::grid.arrange(figa_loss_gva, figb_loss_gdp, 
                        figa_loss_salary, ncol = 1)
dev.off()

# Correlations
# GDP and GVA with annual forest loss
dfgam %>% 
  group_by(year) %>%
  summarise(area = sum(tot_transition_km2, na.rm=TRUE), 
            gdp = median(gdp_percapita_reais), 
            gva = median(gva_agri_percapita_reais)) %>%
  summarise(cor_rho_gva = cor.test(gva, area, 
                                 method = "spearman")$estimate, 
            cor_rho_gdp = cor.test(gdp, area, 
                                 method = "spearman")$estimate, 
            cor_p_gva = cor.test(gva, area, 
                                 method = "spearman")$p.value,
            cor_p_gdp = cor.test(gdp, area, 
                                 method = "spearman")$p.value,
  )
# Salary with annual forest loss
dfgam %>% 
  filter(!is.na(min_salary_mean)) %>%
  group_by(year) %>%
  summarise(area = sum(tot_transition_km2, na.rm=TRUE), 
            salary = median(min_salary_mean)) %>%
  summarise(cor_r_salary = cor.test(salary, area, 
                                    method = "spearman")$estimate, 
            cor_p_salary = cor.test(salary, area, 
                                    method = "spearman")$p.value
  )

#Annual for municipalities
c1 <- cor.test(dfgam$gva_agri_percapita_reais, dfgam$tot_transition_km2) 
c1#0.041
cor.test(dfgam$gdp_percapita_reais, dfgam$tot_transition_km2) #0.074
cor.test(dfgam$log_gdp_percapita_reais, dfgam$tot_transition_km2) #0.13
cor.test(dfgam$min_salary_mean, dfgam$tot_transition_km2) #0.20

# GAM models all municipalities  --------------------------------------------

colNA <- names(dfgam)[sapply(dfgam, anyNA)]
#Explain
#All municipalities
dfgam %>% 
  select(!all_of(colNA)) -> dfgam_gdp
# values for tweedie and AR1 calculated in "gdp_bams.R"

dfgam_gdp %>% 
  arrange(muni_factor, year) %>% 
  group_by(muni_factor) %>%
  mutate(start_year = min(year)) %>% 
  mutate(start_event = year== start_year) %>% 
  ungroup() -> dfgam_gdp

myctrl <- list(keepData = TRUE, trace = TRUE)  
#GVA - # Natural forest loss from past five years (km2)
bam_loss_gvaallkm <- bam(log_gva_percapita_reais ~ 
                           #Spatial smooth
                           s(long, lat) + 
                           #Spatial proximity
                           s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                           #Time
                           s(year, state_namef, bs='fs', m=1) +
                           #s(year, by = state_namef) +
                           s(yearf, bs = "re") +
                           #Forest loss
                           # s(loss_immediate_percent_86, k=4) +
                           s(tot_loss5y_km2, k=4) +
                           #Random 
                           s(state_namef, bs="re") + 
                           s(muni_factor, bs="re"),
                         #AR1 residual errors
                         rho=0.894, AR.start = dfgam_gdp$start_event, 
                         family=Tweedie(1.99),
                         method = "fREML",
                         discrete = TRUE,
                         data = dfgam_gdp, 
                         control = myctrl)   
summary(bam_loss_gvaallkm)
plot(bam_loss_gvaallkm, scale = 0, all.terms = TRUE) #300 x 250
# Now with natural forest loss as proportion of 1986 natural forest cover
bam_loss_gvaall <- bam(log_gva_percapita_reais ~ 
                         #Spatial smooth
                         s(long, lat) + 
                         #Spatial proximity
                         s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                         #Time
                         s(year, state_namef, bs='fs', m=1) +
                         #s(year, by = state_namef) +
                         s(yearf, bs = "re") +
                         #Forest loss
                         # s(loss_immediate_percent_86, k=4) +
                         s(totloss_5y_percent_86, k=4) +
                         #Random 
                         s(state_namef, bs="re") + 
                         s(muni_factor, bs="re"),
                       #AR1 residual errors
                       rho=0.894, AR.start = dfgam_gdp$start_event, 
                       family=Tweedie(1.99),
                       method = "fREML",
                       discrete = TRUE,
                       data = dfgam_gdp, 
                       control = myctrl)   
summary(bam_loss_gvaall)
plot(bam_loss_gvaall, scale = 0, all.terms = TRUE)

#GDP - # Natural forest loss from past five years (km2)
bam_loss_gdpallkm <- bam(log_gdp_percapita_reais ~ 
                           #Spatial smooth
                           s(long, lat) + 
                           #Spatial proximity
                           s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                           #Time
                           s(year, state_namef, bs='fs', m=1) +
                           #s(year, by = state_namef) +
                           s(yearf, bs = "re") +
                           #Forest loss
                           s(tot_loss5y_km2, k=4) +
                           #Random 
                           s(state_namef, bs="re") + 
                           s(muni_factor, bs="re"),
                         #AR1 residual errors
                         rho=0.894, AR.start = dfgam_gdp$start_event, 
                         family=Tweedie(1.99),
                         method = "fREML",
                         discrete = TRUE,
                         data = dfgam_gdp, 
                         control = myctrl)   
summary(bam_loss_gdpallkm)
plot(bam_loss_gdpallkm, scale = 0, all.terms = TRUE)#300 x 250

# Now with natural forest loss as proportion of 1986 natural forest cover
bam_loss_gdpall <- bam(log_gdp_percapita_reais ~ 
                         #Spatial smooth
                         s(long, lat) + 
                         #Spatial proximity
                         s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                         #Time
                         s(year, state_namef, bs='fs', m=1) +
                         #s(year, by = state_namef) +
                         s(yearf, bs = "re") +
                         #Forest loss
                         s(totloss_5y_percent_86, k=4) +
                         #Random 
                         s(state_namef, bs="re") + 
                         s(muni_factor, bs="re"),
                       #AR1 residual errors
                       rho=0.894, AR.start = dfgam_gdp$start_event, 
                       family=Tweedie(1.99),
                       method = "fREML",
                       discrete = TRUE,
                       data = dfgam_gdp, 
                       control = myctrl)   
summary(bam_loss_gdpall)
plot(bam_loss_gdpall, scale = 0, all.terms = TRUE)#300 x 250

#salary 
dfgam %>% 
  filter(!is.na(min_salary_mean)) -> dfgam_salary
colNAsal <- names(dfgam_salary)[sapply(dfgam_salary, anyNA)]
#Explain
#All municipalities
dfgam_salary %>% 
  select(!all_of(colNAsal)) -> dfgam_salary
# values for tweedie and AR1 calculated in "gdp_bams.R"
dfgam_salary %>% 
  arrange(muni_factor, year) %>% 
  group_by(muni_factor) %>%
  mutate(start_year = min(year)) %>% 
  mutate(start_event = year== start_year) %>% 
  ungroup() -> dfgam_salary

bam_loss_salaryallkm <- bam(log_min_salary_mean ~ 
                              #Spatial smooth
                              s(long, lat) + 
                              #Spatial proximity
                              s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                              #Time
                              s(year, state_namef, bs='fs', m=1) +
                              #s(year, by = state_namef) +
                              s(yearf, bs = "re") +
                              #Forest loss
                              s(tot_loss5y_km2, k=4) +
                              #Random 
                              s(state_namef, bs="re") + 
                              s(muni_factor, bs="re"),
                            #AR1 residual errors
                            rho=0.874, AR.start = dfgam_salary$start_event, 
                            family=Tweedie(1.34),
                            method = "fREML",
                            discrete = TRUE,
                            data = dfgam_salary, 
                            control = myctrl)   
summary(bam_loss_salaryallkm)
plot(bam_loss_salaryallkm, scale = 0, all.terms = TRUE) ##300 x 250

#Salary with urban area and muni size. No improvement.
bam_loss_salaryallkm_area <- bam(log_min_salary_mean ~ 
                                   #Spatial smooth
                                   s(long, lat) + 
                                   #Spatial proximity
                                   s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                                   #Time
                                   s(year, state_namef, bs='fs', m=1) +
                                   #s(year, by = state_namef) +
                                   s(yearf, bs = "re") +
                                   #Forest loss
                                   s(tot_loss5y_km2, k=4) +
                                   #muni area
                                   s(muni_area_km2) +
                                   #urban area 
                                   s(urban_ha_p1000) +
                                   #Random 
                                   s(state_namef, bs="re") + 
                                   s(muni_factor, bs="re"),
                                 #AR1 residual errors
                                 rho=0.874, AR.start = dfgam_salary$start_event, 
                                 family=Tweedie(1.34),
                                 method = "fREML",
                                 discrete = TRUE,
                                 data = dfgam_salary, 
                                 control = myctrl) 
summary(bam_loss_salaryallkm_area)
plot(bam_loss_salaryallkm_area, scale = 0, all.terms = TRUE) ##300 x 250

# Now with natural forest loss as proportion of 1986 natural forest cover
bam_loss_salaryall <- bam(log_min_salary_mean ~ 
                            #Spatial smooth
                            s(long, lat) + 
                            #Spatial proximity
                            s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                            #Time
                            s(year, state_namef, bs='fs', m=1) +
                            #s(year, by = state_namef) +
                            s(yearf, bs = "re") +
                            #Forest loss
                            s(totloss_5y_percent_86, k=4) +
                            #Random 
                            s(state_namef, bs="re") + 
                            s(muni_factor, bs="re"),
                          #AR1 residual errors
                          rho=0.874, AR.start = dfgam_salary$start_event, 
                          family=Tweedie(1.34),
                          method = "fREML",
                          discrete = TRUE,
                          data = dfgam_salary, 
                          control = myctrl)   
summary(bam_loss_salaryall)
plot(bam_loss_salaryall, scale = 0, all.terms = TRUE) ##300 x 250

# Select municipalites ----------------------------------------------------
# Select municipalities with less and more forest cover
#Use municipality summary
df_muni <- read_excel("data/bla_municipalities_4trees.xlsx", 
                      na = c("", "NA"),
                      sheet = "municipality_fixed_ref",
                      .name_repair = "universal") 
df_muni$muni_factor <- paste(df_muni$state_name, df_muni$muni_name, sep = "_")
df_muni$muni_factor <- as.factor(df_muni$muni_factor)
df_muni$flag_urbanf <- as.factor(df_muni$flag_urban)

#add vars from dfmuni
df_muni %>% left_join(dfgam %>% group_by(muni_factor) %>% 
  summarise(median_gold_p1000 = median(process_gold_p1000), 
         median_pop_dens = median(pop_dens_km2), 
         median_industry = median(gva_industry_percent))) -> df_muni

# 41 municipalities with low cover in 1986
df_muni %>% filter(forestcover_1986med_percent_muni <=40, 
                 indigenous_area_percent < 50, flag_include=="yes") %>%
  pull(muni_factor) %>% as.character() %>% unique() -> n40#41
df_muni %>% filter(forestcover_1986med_percent_muni <=30, 
                 indigenous_area_percent < 50, flag_include=="yes") %>%
  pull(muni_factor) %>% as.character() %>% unique() -> n30#16

df_muni %>% 
  filter(forestcover_1986med_percent_muni <=40, 
         indigenous_area_percent < 50, flag_include=="yes") -> df_muni_cover40
keep_states <- unique(df_muni_cover40$state_name) 
keep_states
#        "Amazonas"  "Amapá"  "Maranhão"    "Mato Grosso" "Pará"       
 #[6] "Roraima"     "Tocantins" 
summary(df_muni_cover40$dist_statecapital_km) #44 - 753 km 210 median
summary(df_muni_cover40$muni_area_km2) # #200 - 12535 km2 median 1287
summary(df_muni_cover40$indigenous_area_percent) # mean = 1.11, max = 21
summary(df_muni_cover40$median_gold_p1000) #0
summary(df_muni_cover40$median_pop_dens) # median = 7.67, max = 150
summary(df_muni_cover40$median_industry) # median = 4.9, max = 41.5

# 205
df_muni %>% 
  filter(forestcover_1986med_percent_muni >=60, indigenous_area_percent <= 21, 
         dist_statecapital_km <= 753, muni_area_km2 <= 12535, 
         forestcover_2019med_percent_muni >=60, flag_include=="yes",
         state_name %in% keep_states) %>% 
  filter(median_gold_p1000==0, 
         median_pop_dens <=150, 
         median_industry <= 42) %>% 
  ungroup() -> df_muni_cover60
length(unique(paste(df_muni_cover60$state_name, df_muni_cover60$muni_name))) #205
# <= 50%, 111
df_muni %>% 
  filter(forestcover_1986med_percent_muni >=60, indigenous_area_percent <= 21, 
         dist_statecapital_km <= 753, muni_area_km2 <= 12535, 
         forestcover_2019med_percent_muni <=50,
         state_name %in% keep_states, flag_include=="yes") %>%
  filter(median_gold_p1000==0, 
         median_pop_dens <=150, 
         median_industry <= 42) %>% 
  ungroup() -> df_muni_cover60less
length(unique(paste(df_muni_cover60less$state_name, df_muni_cover60less$muni_name))) #111

#357 municipalites in matched subset
df_muni_cover40 %>% mutate(trees = "few") %>% 
  bind_rows(df_muni_cover60  %>% mutate(trees = "many")) %>% 
  bind_rows(df_muni_cover60less %>% mutate(trees = "many_loss")) -> dfmatched
nrow(dfmatched) #357
matched_area <- sum(dfmatched$muni_area_km2)
(matched_area / tot_muni_area_km2) * 100 #17.88
#Analysis with matched groups
#matched subset
dfgam %>% 
  right_join(dfmatched %>% select(state_name, muni_name, trees)) -> dfgam_matched
dfgam_matched$cover_group <- as.factor(dfgam_matched$trees)
levels(dfgam_matched$cover_group) <- c("less cover", 
                                       "more cover", 
                                       "more loss")

# Study area map ----------------------------------------------------------

#Study area map showing matched locations
#Basic reference vectors
bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
dfstates <- data.frame(bla_state_names, bla_state_siglas)

#Municipal polygons
ibge_muni <- "data/vector//brazil_ninestate_municipalities//ninestate_muni.shp"
sf_ninestate_muni <- st_read(ibge_muni) 
# load data
world <- ne_countries(scale = "medium", returnclass = "sf")
#  world map
#ggplot(data = world) +
#  geom_sf() + 
#  coord_sf(xlim = c(-72, -35), ylim = c(-35.00, 4.00), expand = T)
 
projcrs <- st_crs(sf_ninestate_muni)
sf_matched <- st_as_sf(x = dfmatched,                         
               coords = c("long", "lat"),
               crs = projcrs)
sf_matched$cover_group <- as.factor(sf_matched$trees)
levels(sf_matched$cover_group ) <- c("less cover", 
                                     "more cover", 
                                     "more loss")

sf_ninestate_muni %>% 
  ggplot()+ 
  geom_sf(data = world, fill="cornsilk") +
  geom_sf(aes(fill = SIGLA_UF), 
          color = "black", size = 0.1, show.legend = FALSE) + 
  geom_sf(data = sf_matched, colour="white",  
          size = 3.4,  show.legend = FALSE) +
  geom_sf(data = sf_matched, aes(colour = cover_group, 
                                 shape = cover_group), 
          size = 3) + 
  #coord_sf(crs = 4326, datum = NA) + 
  coord_sf(xlim = c(-72, -33), ylim = c(-35.00, 4.00), 
           crs = 4326, expand = T) +
  scale_fill_grey() + 
  annotation_scale(location = "br", width_hint = 0.5) +
  facet_wrap(~cover_group) +
  theme_bw() + 
  labs(color = 'cover class', shape = 'cover class') + 
  theme(text = element_text(size = 20), 
        plot.title.position = "plot", 
        legend.position="top") -> fig_map_studyarea
#export
png(file = "data/figures//fig_map_studyarea.png", bg = "white", type = c("cairo"), 
    width=8000, height=4000, res = 600)
fig_map_studyarea
dev.off()

#Population in 2019
dfgam_matched %>% 
  filter(year == 2019) %>% 
  pull(tot_pop) %>% sum() -> pop_matched_2019 
pop_matched_2019 #7988731
dfgam %>% 
  filter(year == 2019) %>% 
  pull(tot_pop) %>% sum() -> pop_total_2019
pop_total_2019 #21113376

#Figure 3

dfgam_matched %>% 
  filter(!is.na(min_salary_mean)) %>%
  ggplot(aes(x=year, y=gva_agri_percapita_reais / 3.946)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  facet_wrap(~cover_group) + 
  scale_y_continuous("agriculture GVA per capita (US$)", 
                     labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) + 
  labs(title = "(A)") +
  theme(plot.title.position = "plot") -> fig_GVA_matched
fig_GVA_matched

dfgam_matched %>% 
  filter(!is.na(min_salary_mean)) %>%
  ggplot(aes(x=year, y=gdp_percapita_reais / 3.946)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  facet_wrap(~cover_group) + 
  scale_y_continuous("GDP per capita (US$)", 
                     labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) + 
  labs(title = "(B)") +
  theme(plot.title.position = "plot") -> fig_GDP_matched
fig_GDP_matched

dfgam_matched %>% 
  filter(!is.na(min_salary_mean)) %>%
  ggplot(aes(x=year, y=min_salary_mean)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  facet_wrap(~cover_group) + 
  scale_y_continuous("average salary") + 
  labs(title = "(C)") +
  theme(plot.title.position = "plot") -> fig_salary_matched
fig_salary_matched
#Export
png(file = "data/figures//fig_economic_matched.png", 
    bg = "white", type = c("cairo"), 
    width=4000, height=5000, res = 600)
grid.arrange(fig_GVA_matched, 
             fig_GDP_matched, fig_salary_matched, ncol = 1)
dev.off()


# GAMs Matched municipalities --------------------------------------------
# values for tweedie and AR1 calculated in "gdp_bams.R"

dfgam_matched %>% 
  filter(!is.na(min_salary_mean)) %>% 
  arrange(muni_factor, year) %>% 
  group_by(muni_factor) %>%
  mutate(start_year = min(year)) %>% 
  mutate(start_event = year== start_year) %>% 
  ungroup() -> dfgam_matched_model
#Export for further use (checking)
saveRDS(dfgam_matched_model, "data/dfgam_matched_model.rds")

myctrl <- list(keepData = TRUE, trace = TRUE)  
#GVA
bam_loss_gva <- bam(log_gva_percapita_reais ~ 
                     cover_group +
                     #Spatial smooth
                     s(long, lat) + 
                     #Spatial proximity
                     s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                     #Time
                     s(year, state_namef, bs='fs', m=1) +
                     s(yearf, bs = "re") +
                     #Random 
                      #s(cover_group, bs="re") +
                     s(state_namef, bs="re") + 
                     s(muni_factor, bs="re"), 
                   #AR1 residual errors
                   rho=0.874, AR.start = dfgam_matched_model$start_event, 
                   family=Tweedie(1.99),
                   method = "fREML",
                   discrete = TRUE,
                   data = dfgam_matched_model, 
                   control = myctrl)   
summary(bam_loss_gva)
plot(bam_loss_gva, scale = 0, all.terms = TRUE) # 500 W x 400 H
#gratia::appraise(bam_loss_gva) #needs lots of memory
#parametric_effects(bam_loss_gva) #needs a lot of memory

#GDP
bam_loss_01 <- bam(log_gdp_percapita_reais~ 
                 cover_group +
                 #Spatial smooth
                 s(long, lat) + 
                 #Spatial proximity
                 s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                 #Time
                 s(year, state_namef, bs='fs', m=1) +
                 #s(year, by = state_namef) +
                 s(yearf, bs = "re") +
                 #Random 
                 s(state_namef, bs="re") + 
                   #s(cover_group, bs="re") +
                 s(muni_factor, bs="re"), 
               #AR1 residual errors
               rho=0.893, AR.start = dfgam_matched_model$start_event, 
               family=Tweedie(1.99),
               method = "fREML",
               discrete = TRUE,
               data = dfgam_matched_model, 
               control = myctrl)   
summary(bam_loss_01)
plot(bam_loss_01, scale = 0, all.terms = TRUE) # 500 W x 400 H

#Salary
bam_loss_02 <- bam(log_min_salary_mean ~ 
                     cover_group +
                     #Spatial smooth
                     s(long, lat) + 
                     #Spatial proximity
                     s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                     #Time
                     s(year, state_namef, bs='fs', m=1) +
                     #s(year, by = state_namef) +
                     s(yearf, bs = "re") +
                     #Random 
                     s(state_namef, bs="re") + 
                     #s(cover_group, bs="re") +
                     s(muni_factor, bs="re"),
                   #AR1 residual errors
                   rho=0.843, AR.start = dfgam_matched_model$start_event, 
                   family=Tweedie(1.337),
                   method = "fREML",
                   discrete = TRUE,
                   data = dfgam_matched_model, 
                   control = myctrl)   
summary(bam_loss_02)
plot(bam_loss_02, scale = 0, all.terms = TRUE) # 500 W x 400 H


# Other well-being essentials --------------------------------------------------------
# other essentials to a standard of living
df_muni %>% filter(flag_include == "yes") %>% 
  pull(tot_pop_2019) %>% sum() -> pop_2019 #21113376

df_muni %>% 
  right_join(dfmatched %>% 
               select(state_name, muni_name, trees)) -> df_muni_matched
df_muni_matched$cover_group <- as.factor(df_muni_matched$trees)
levels(df_muni_matched$cover_group) <- c("less cover", 
                                         "more cover", 
                                         "more loss")

#Population
df_muni_matched %>% pull(tot_pop_2019) %>% sum() -> pop_2019_matched
pop_2019_matched/ pop_2019 *100

# Figure 4   ------------------------------------------------------
# forest cover change and poverty
dfpoverty %>% 
  select(muni_factor,urban_flag, 
         flag_sanitation_plan, flag_int_complete_cover, cover_diff) %>%
  pivot_longer(cols = starts_with("flag")) %>% 
  mutate(myname = ifelse(name=="flag_sanitation_plan", "sanitation plan", 
                         "full intenet connectivity")) %>% 
  filter(!is.na(value)) %>%
  ggplot(aes(x = factor(value), y = cover_diff, fill = urban_flag)) + 
  #geom_point(aes(group=urban_flag), 
  #           position=position_jitterdodge(jitter.width = .05), 
  #           alpha = 0.2) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_x_discrete("condition existed in 2019", labels = c("no", "yes")) + 
  scale_fill_grey("urban", start = 0.7,
                  end = 1.0, labels = c("no", "yes")) +
  facet_wrap(~ myname, ncol = 1) + 
  labs(y = "forest cover difference\n(1986 - 2019)") + 
  labs(title = "(A)") + 
  theme_bw() +
  theme(plot.title.position = "plot") -> fig_essential_cover
fig_essential_cover

#differences between cover classes
set.seed(10)
nudgeWidth = 0.5
nudgeVec <- (runif(nrow(df_muni_matched))-0.5)*nudgeWidth

nudgeHeight = 0.1
nudgeVecH <- (runif(nrow(df_muni_matched))-0.5)*nudgeHeight

#internet
df_muni_matched %>% 
  ggplot(aes(x = cover_group, y = flag_int_complete_cover)) + 
  geom_violin() + 
  scale_y_continuous("full internet\nconnectivity", 
                     breaks = c(0,1), labels = c("no", "yes")) + 
  scale_x_discrete("forest cover", labels = c("less", "more", "more loss")) +
  geom_point(position=position_nudge(x = nudgeVec, y = nudgeVecH), 
             alpha=0.35, size =1,
             aes(colour=cover_group)) + 
  #geom_point(position=position_nudge(x = nudgeVec, y = nudgeVecH), 
  #           shape=21, color = "black", fill=NA) +
  labs(title = "(B)") +
  theme(plot.title.position = "plot", 
        axis.title.x = element_blank(), 
        legend.position="none") -> fig_essential_inter
fig_essential_inter

#sanitation
df_muni_matched %>% 
  ggplot(aes(x = cover_group, y = flag_sanitation_plan)) + 
  geom_violin() +
  scale_y_continuous("sanitation plan\napproved", 
                     breaks = c(0,1), labels = c("no", "yes")) +
  scale_x_discrete("forest cover", labels = c("less", "more", "more loss")) +
  geom_point(position=position_nudge(x = nudgeVec, y = nudgeVecH), 
             alpha=0.35, size = 1,
             aes(colour=cover_group)) + 
  #geom_point(position=position_nudge(x = nudgeVec, y = nudgeVecH), 
  #           shape=21, color = "black", fill=NA) +
  labs(title = "(C)") + 
  theme(plot.title.position = "plot", 
        #axis.title.x = element_blank(), 
        legend.position="none") -> fig_essential_sani 
fig_essential_sani

#Export
png(file = "data/figures//fig_essentials_matched.png", 
    bg = "white", type = c("cairo"), 
    width=3500, height=2000, res = 600)
lay <- rbind(c(1,1,2,2),
             c(1,1,3,3))
grid.arrange(fig_essential_cover, 
             fig_essential_inter, fig_essential_sani,
             layout_matrix = lay)
dev.off()

#Explain
# With both internet and sanitation
dfpoverty %>% 
  filter(flag_sanitation_plan ==1, flag_int_complete_cover ==1) %>%
  pull(muni_factor) %>% as.character() -> muni_prosperous #68
length(muni_prosperous) / nrow(dfpoverty) * 100 #8.6%

df_muni %>% filter(muni_factor %in% all_of(muni_prosperous)) %>% 
  group_by(muni_factor, muni_area_km2) %>% summarise(acount = n()) %>% 
  pull(muni_area_km2) %>% sum() -> prosperous_area
prosperous_area / tot_muni_area_km2 *100 #9.04%

#with internet connectivity
dfpoverty %>% 
  filter(flag_int_complete_cover ==1) %>%
  pull(muni_factor) %>% as.character() -> muni_internet #325
length(muni_internet) / nrow(dfpoverty) #0.4

#sanitation plan
dfpoverty %>% 
  filter(flag_sanitation_plan ==1) %>%
  pull(muni_factor) %>% as.character() -> muni_sanitation #157
length(muni_sanitation) / nrow(dfpoverty) #0.2

#Probability of basic conditions same among cover groups?
#Sanitation
df_muni_matched %>% 
  group_by(cover_group) %>% 
  summarise(tot_pop = n(), 
            tot_san = sum(flag_sanitation_plan)) %>% 
  mutate(tot_no_san = tot_pop - tot_san, 
         prop_san = tot_san/tot_pop)
sanitation <- c(12, 29, 23)
nosanitation <- c(29, 176, 88)
totpop <- c(41, 205, 111)
prop.test(sanitation, totpop)
#X-squared = 6.1645, df = 2, p-value = 0.04585
#   prop 1    prop 2    prop 3 
#0.2926829 0.1414634 0.2072072 

#Internet
df_muni_matched %>% 
  filter(!is.na(flag_int_complete_cover)) %>%
  group_by(cover_group) %>% 
  summarise(tot_pop = n(), 
            tot_int = sum(flag_int_complete_cover)) %>% 
  mutate(tot_no_int = tot_pop - tot_int, 
         prop_int = tot_int/tot_pop)
internet <- c(17, 82, 38)
nointernet <- c(24, 122, 73)
prop.test(internet, totpop)
#X-squared = 1.1991, df = 2, p-value = 0.5491
#   prop 1    prop 2    prop 3 
#0.4146341 0.4000000 0.3423423 

#Both
df_muni_matched %>% 
  filter(!is.na(flag_int_complete_cover)) %>%
  mutate(flag_both = flag_int_complete_cover + flag_sanitation_plan) %>%
  mutate(flag_both = ifelse(flag_both==2,1,0)) %>% 
  group_by(cover_group) %>% 
  summarise(tot_pop = n(), 
            tot_both = sum(flag_both)) %>% 
  mutate(tot_no_both = tot_pop - tot_both, 
         prop_both = tot_both/tot_pop)
both <- c(5, 14, 8)
prop.test(both, totpop)
#X-squared = 1.4363, df = 2, p-value = 0.4876
#    prop 1     prop 2     prop 3 
#0.12195122 0.06829268 0.07207207

# Matched comparisons ------------------------------------------
# Median and range for data table
df_muni_matched %>% 
  group_by(trees) %>% 
  summarise(count_muni = n(), 
            count_states = length(unique(state_name)),
            median_cover_1986 = median(forestcover_1986med_percent_muni), 
            min_cover_1986 = min(forestcover_1986med_percent_muni), 
            max_cover_1986 = max(forestcover_1986med_percent_muni)
  )
df_muni_matched %>% 
  group_by(trees) %>% 
  summarise(median_cover_2019 = median(forestcover_2019med_percent_muni), 
            min_cover_2019 = min(forestcover_2019med_percent_muni), 
            max_cover_2019 = max(forestcover_2019med_percent_muni)
  )
df_muni_matched %>% 
  group_by(trees) %>% 
  summarise(total_area = sum(muni_area_km2), 
            median_area = median(muni_area_km2), 
            min_area = min(muni_area_km2), 
            max_area = max(muni_area_km2), 
            state_count = length(unique(state_name)))

df_muni_matched %>% 
  group_by(trees) %>% 
  summarise(dist = median(dist_statecapital_km), 
            min_dist = min(dist_statecapital_km), 
            max_dist = max(dist_statecapital_km))

df_muni_matched %>% 
  group_by(trees) %>% 
  summarise(pop_dens = median(median_pop_dens), 
            min_pop_dens = min(median_pop_dens), 
            max_pop_dens = max(median_pop_dens), 
            industry = median(median_industry), 
            min_industry = min(median_industry), 
            max_industry = max(median_industry))

df_muni_matched %>% 
  group_by(trees) %>% 
  summarise(indigenous = median(indigenous_area_percent), 
            min_indigenous = min(indigenous_area_percent), 
            max_indigenous = max(indigenous_area_percent), 
            gold = median(median_gold_p1000, na.rm = TRUE), 
            min_gold = min(median_gold_p1000), 
            max_gold = max(median_gold_p1000, na.rm = TRUE))

dfmatched %>% 
  group_by(trees, flag_urbanf) %>% 
  summarise(urban = length(flag_urbanf)) 

library(Hmisc)
png(file = "data/figures//fig_back2back.png", 
    bg = "white", type = c("cairo"), 
    width=1500, height=7000, res = 600)
par(mfrow = c(6, 1))
histbackback(df_muni_cover40$muni_area_km2, 
             df_muni_cover60$muni_area_km2, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "municipality size (km2)")
histbackback(df_muni_cover40$dist_statecapital_km, 
             df_muni_cover60$dist_statecapital_km, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "distance to state capital (km)")
histbackback(df_muni_cover40$median_pop_dens, 
             df_muni_cover60$median_pop_dens, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "population density")
histbackback(df_muni_cover40$indigenous_area_percent, 
             df_muni_cover60$indigenous_area_percent, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "indigenous area (%)")
histbackback(df_muni_cover40$median_industry, 
             df_muni_cover60$median_industry, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "industry  value contribution (%)")
histbackback(df_muni_cover40$forestcover_2019med_percent_muni, 
             df_muni_cover60$forestcover_2019med_percent_muni, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "forest cover 2019 (%)") 
dev.off()

#loss
png(file = "data/figures//fig_back2backloss.png", 
    bg = "white", type = c("cairo"), 
    width=1500, height=7000, res = 600)
par(mfrow = c(6, 1))
histbackback(df_muni_cover40$muni_area_km2, 
             df_muni_cover60less$muni_area_km2, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "municipality size (km2)")
histbackback(df_muni_cover40$dist_statecapital_km, 
             df_muni_cover60less$dist_statecapital_km, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "distance to state capital (km)")
histbackback(df_muni_cover40$median_pop_dens, 
             df_muni_cover60less$median_pop_dens, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "population density")
histbackback(df_muni_cover40$indigenous_area_percent, 
             df_muni_cover60less$indigenous_area_percent, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "indigenous area (%)")
histbackback(df_muni_cover40$median_industry, 
             df_muni_cover60less$median_industry, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "industry  value contribution (%)")
histbackback(df_muni_cover40$forestcover_2019med_percent_muni, 
             df_muni_cover60less$forestcover_2019med_percent_muni, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "forest cover 2019 (%)") 
dev.off()

# Supplemental material ---------------------------------------------------

#correlations with time varying covariates
#Pairs panel with human readable names
# 566 * 453
pairs_vars_forestkm <- c('gdp_percapita_reais', 
                         'log_gdp_percapita_reais', 
                         'gva_agri_percapita_reais', 
                         'log_gva_percapita_reais',
                         'loss_immediate_km2', 
                         'tot_loss3y_km2','tot_loss5y_km2') 

dfgam %>%
  select(all_of(pairs_vars_forestkm)) %>%
  rename(GDP = gdp_percapita_reais, 
         GDP_log = log_gdp_percapita_reais,
         GVA = gva_agri_percapita_reais,
         GVA_log = log_gva_percapita_reais, 
         forest_loss_2y = loss_immediate_km2, 
         forest_loss_3y = tot_loss3y_km2,
         forest_loss_5y = tot_loss5y_km2) %>%
  psych::pairs.panels()

pairs_vars_forest <- c( 'gdp_percapita_reais', 
                        'log_gdp_percapita_reais', 
                        'gva_agri_percapita_reais', 
                       'log_gva_percapita_reais', 
                        'loss_immediate_percent_86', 
                       'totloss_3y_percent_86','totloss_5y_percent_86') 
dfgam %>%
  select(all_of(pairs_vars_forest)) %>%
  rename(GDP = gdp_percapita_reais, 
         GDP_log = log_gdp_percapita_reais,
         GVA = gva_agri_percapita_reais,
         GVA_log = log_gva_percapita_reais, 
         forest_loss_2y = loss_immediate_percent_86, 
         forest_loss_3y = totloss_3y_percent_86,
         forest_loss_5y = totloss_5y_percent_86) %>%
  psych::pairs.panels()


pairs_vars_forest_salarykm <- c('min_salary_mean', 
                              'log_min_salary_mean',
                              'log_gdp_percapita_reais',
                              'log_gva_percapita_reais', 
                              'loss_immediate_km2', 
                              'tot_loss3y_km2','tot_loss5y_km2')
dfgam %>% 
  filter(!is.na(min_salary_mean)) %>%
  select(all_of(pairs_vars_forest_salarykm)) %>%
  rename(salary = min_salary_mean, 
         salary_log = log_min_salary_mean, 
         GDP_log = log_gdp_percapita_reais, 
         GVA_log = log_gva_percapita_reais, 
         forest_loss_2y = loss_immediate_km2, 
         forest_loss_3y = tot_loss3y_km2,
         forest_loss_5y = tot_loss5y_km2) %>%
  psych::pairs.panels()

pairs_vars_forest_salary <- c('min_salary_mean', 
                              'log_min_salary_mean',
                              'log_gdp_percapita_reais',
                              'log_gva_percapita_reais', 
                              'loss_immediate_percent_86', 
                              'totloss_3y_percent_86','totloss_5y_percent_86') 

dfgam %>% 
  filter(!is.na(min_salary_mean)) %>%
  select(all_of(pairs_vars_forest_salary)) %>%
  rename(salary = min_salary_mean, 
         salary_log = log_min_salary_mean, 
         GDP_log = log_gdp_percapita_reais, 
         GVA_log = log_gva_percapita_reais, 
         forest_loss_2y = loss_immediate_percent_86, 
         forest_loss_3y = totloss_3y_percent_86,
         forest_loss_5y = totloss_5y_percent_86) %>%
  psych::pairs.panels()

pairs_vars_forestloss <- c('loss_immediate_km2', 'loss_immediate_percent_86',
                              'tot_loss3y_km2', 'totloss_3y_percent_86', 
                           'tot_loss5y_km2', 'totloss_5y_percent_86') 

dfgam %>% 
  select(all_of(pairs_vars_forestloss)) %>%
  rename(loss_2y_km2 = loss_immediate_km2, 
         loss_2y_per = loss_immediate_percent_86, 
         loss_3y_km2 = tot_loss3y_km2,
         loss_3y_per = totloss_3y_percent_86, 
         loss_5y_km2 = tot_loss5y_km2,
         loss_5y_per = totloss_5y_percent_86) %>%
  psych::pairs.panels()

#Cross correlations

names(dfgam)
dfgam %>%
  group_by(state_namef, dist_statecapital_km) %>% 
  tk_acf_diagnostics(
    .date_var = year,
    .value = log_gdp_percapita_reais,
    .ccf_vars = log_gva_percapita_reais, 
    .lags = 11
  ) -> tidy_ccf_gdp_agri

#export as .png  250 * 1000
tidy_ccf_gdp_agri %>% 
  filter(lag <11) %>%
  ggplot(aes(x = lag, y = CCF_log_gva_percapita_reais, 
             color = state_namef, 
             group = state_namef)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.7, linetype=2) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), 
                     labels = c(0, 2, 4, 6, 8, 10)) +
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
    subtitle = "GDP and agriculture GVA",
    x = "lag (year)", 
    y = "correlation coefficient"
  )

dfgam %>%
  group_by(state_namef, dist_statecapital_km) %>% 
  tk_acf_diagnostics(
    .date_var = year,
    .value = log_gdp_percapita_reais,
    .ccf_vars = tot_loss_percent, 
    .lags = 11
  ) -> tidy_ccf_gdp_loss

#export as .png  250 * 1000
tidy_ccf_gdp_loss %>% 
  filter(lag <11) %>%
  ggplot(aes(x = lag, y = CCF_tot_loss_percent, 
             color = state_namef, 
             group = state_namef)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.7, linetype=2) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), 
                     labels = c(0, 2, 4, 6, 8, 10)) +
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
    subtitle = "GDP and forest loss",
    x = "lag (year)", 
    y = "correlation coefficient"
  )

dfgam %>%
  group_by(state_namef, dist_statecapital_km) %>% 
  tk_acf_diagnostics(
    .date_var = year,
    .value = log_gdp_percapita_reais,
    .ccf_vars = loss_immediate_percent, 
    .lags = 11
  ) -> tidy_ccf_gdp_immed


#export as .png  250 * 1000
tidy_ccf_gdp_immed %>% 
  filter(lag <11) %>%
  ggplot(aes(x = lag, y = CCF_loss_immediate_percent, 
             color = state_namef, 
             group = state_namef)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.7, linetype=2) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), 
                     labels = c(0, 2, 4, 6, 8, 10)) +
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
    subtitle = "GDP and forest loss (2 Y)",
    x = "lag (year)", 
    y = "correlation coefficient"
  )

dfgam %>%
  group_by(state_namef, dist_statecapital_km) %>% 
  tk_acf_diagnostics(
    .date_var = year,
    .value = log_gdp_percapita_reais,
    .ccf_vars = tot_loss5y_percent, 
    .lags = 11
  ) -> tidy_ccf_gdp_5y

#export as .png  250 * 1000
tidy_ccf_gdp_5y %>% 
  filter(lag <11) %>%
  ggplot(aes(x = lag, y = CCF_tot_loss5y_percent, 
             color = state_namef, 
             group = state_namef)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.7, linetype=2) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), 
                     labels = c(0, 2, 4, 6, 8, 10)) +
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
    subtitle = "GDP and forest loss (5 Y)",
    x = "lag (year)", 
    y = "correlation coefficient"
  )


pairs_vars_gva <- c('log_gva_percapita_reais', 'tot_transition_km2')
dfgam %>%
  select(all_of(pairs_vars_gva)) %>%
  rename(GVA = log_gva_percapita_reais, forest_loss = tot_transition_km2) %>%
  psych::pairs.panels()

pairs_vars_gdp <- c('log_gdp_percapita_reais', 'tot_transition_km2')
dfgam %>%
  select(all_of(pairs_vars_gdp)) %>%
  rename(GDP = log_gdp_percapita_reais, forest_loss = tot_transition_km2) %>%
  psych::pairs.panels()

pairs_vars_salary <- c('min_salary_mean', 'tot_transition_km2')
dfgam %>%
  select(all_of(pairs_vars_salary)) %>%
  rename(salary = min_salary_mean, forest_loss = tot_transition_km2) %>%
  filter(!is.na(salary)) %>%
  psych::pairs.panels()


