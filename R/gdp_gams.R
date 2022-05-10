#gdp GAM models
#packages
library(tidyverse)
library(mgcv)
library(stringi)
library(timetk)
library(gratia)
library(sf)

#guides
#bam
#https://jacolienvanrij.com/Tutorials/GAMM.html#model-terms-partial-effects
#http://jacolienvanrij.com/PupilAnalysis/SupplementaryMaterials-2.html
#gamm
#https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
#https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
#https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/

#extra memory to speed up models, pairs panel, gam.check etc
memory.limit(50000)

#Uses dfgam from "gdp_analysis.R"
dfgam <- readRDS("dfgam.rds") #13710 obs. 55 vars
#plot(dfgam$gva_industry_percent, dfgam$gdp_percapita_reais)
#length(unique(dfgam$muni_factor)) #763 municipalities
# 4956340 km2
#dfgam %>% group_by(state_name, muni_name) %>% 
#  summarise(area_km2 = max(muni_area_km2)) %>% pull(area_km2) %>% sum()

#correlations with time varying covariates
#Pairs panel with human readable names
pairs_vars <- c('gdp_percapita_reais', 'mine_area_km2_gold_percapita',
               'gva_agri_percapita_reais', 'gva_industry_percent', 
               'pop_dens_km2', 'tot_loss5y_percent', 
               'school_per1000', 'pg_per1000')
dfgam %>%
  select(all_of(pairs_vars)) %>%
  rename(GDP = gdp_percapita_reais, gold = mine_area_km2_gold_percapita, 
         agri = gva_agri_percapita_reais, industry = gva_industry_percent, 
         indigenous_land = indigenous_area_percent,
         pop = pop_dens_km2, forest_loss = tot_loss5y_percent, 
         schools = school_per1000, post_grad = pg_per1000) %>%
psych::pairs.panels()

#GAMM models...
#Basic
model_01_null <- gamm(log(gdp_percapita_reais) ~ 
                   s(year, by = state_namef, k=5, m=1, bs="tp") + 
                   s(dist_statecapital_km, by = state_namef) + 
                   s(state_namef, bs="re"), 
                 data = dfgam, 
                 method="REML")
hist(resid(model_01_null$gam, type = "deviance"))
summary(model_01_null$gam) #r2 = 0.75

#Null with correlation structure
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B", 
             maxIter = 99, msMaxIter = 99, keepData = TRUE)
model_01_ar1_null <- gamm(log(gdp_percapita_reais) ~ 
                        s(year, by = state_namef, k=5, m=1, bs="tp") + 
                        s(dist_statecapital_km, by = state_namef) + 
                        s(state_namef, bs="re"), 
                        correlation = corARMA(form = ~ year|muni_factor, p = 1),
                      data = dfgam, 
                      method="REML", 
                      control = ctrl)
saveRDS(model_01_ar1_null, "model_01_ar1_null.rds")
model_01_ar1_null <- readRDS("model_01_ar1_null.rds")
hist(resid(model_01_ar1_null$gam, type = "deviance"))
summary(model_01_ar1_null$gam) #0.744

#Develop 
model_01 <- gamm(log(gdp_percapita_reais) ~ year*dominant_groupsf +
                        s(year, by = state_namef, k=5, m=1, bs="tp") + 
                   s(gva_agri_percapita_reais, by = flag_gold) + 
                   s(indigenous_area_percent, k=4) + 
                   s(process_gold_p1000, k=4) + 
                   s(process_metal_p1000, k=4) + 
                   s(tot_loss5y_percent) + 
                   s(pop_dens_km2, k=4) + 
                   #s(state_namef, bs="re") +
                        s(dist_statecapital_km, by = state_namef) + 
                   s(school_per1000), 
                      data = dfgam, 
                      method="REML")
hist(resid(model_01$gam, type = "deviance")) #Much improved.
summary(model_01$gam) #0.931
plot(model_01$gam, scale = 0, all.terms = TRUE)

#Full model
#without AR
model_01 <- gamm(log(gdp_percapita_reais) ~ main_sectorf +
                   s(year, by = state_namef, k=5, m=1, bs="tp") + 
                   s(gva_agri_percapita_reais) + 
                   s(indigenous_area_percent, k=4) + 
                   s(gold_area_km2_percapita, k=4, by = flag_gold) +
                   s(tot_loss5y_percent) + 
                   s(pop_dens_km2, k=4) + 
                   s(school_per1000) + 
                   s(dist_statecapital_km, by = state_namef) + 
                   s(state_namef, bs="re"),
                     data = dfgam, 
                     method="REML")
saveRDS(model_01, "model_01.rds")
model_01 <- readRDS("model_01.rds")
hist(resid(model_01$gam, type = "deviance"))
summary(model_01$gam) #r2 93. everything significant!
appraise(model_01$gam) #problems with deviance residual > 1.5
plot(model_01$gam, scale = 0, all.terms = TRUE)

#with AR working. AR wont converge with re and dominant factors.
#Improved quantiles with state as re
model_01_ar1 <- gamm(log(gdp_percapita_reais) ~ 
                       #dominant_groupsf2 +
                       s(gva_agri_percapita_reais, by = state_namef) + 
                       s(indigenous_area_percent, k=4) + 
                       s(process_gold_p1000, k=4) + 
                       s(process_metal_p1000, k=4) + 
                       s(tot_loss5y_percent, by = state_namef) + 
                       s(pop_dens_km2, by = state_namef, k=4) + 
                       s(state_namef, bs="re") +
                       s(dist_statecapital_km, by = state_namef) + 
                       s(school_per1000), 
                     correlation = corARMA(form = ~ year|muni_factor, p = 1), 
                     data = dfgam, 
                     method="REML", 
                     control = ctrl)
saveRDS(model_01_ar1, "model_01_ar1.rds")
model_01_ar1 <- readRDS("model_01_ar1.rds")
hist(resid(model_01_ar1$gam, type = "deviance")) #histograms good
summary(model_01_ar1$lme) #check correlation structure
summary(model_01_ar1$gam) #r2 = 0.658....

#gam.check(model_01_ar1$gam) #problem with residual > 1
appraise(model_01_ar1$gam)
plot(model_01_ar1$gam, scale = 0, all.terms = TRUE)
#anova(model_01$lme, model_01_ar1$lme)

# VIF code below not working
# Variance Inflation Factor https://github.com/samclifford/mgcv.helper/blob/master/R/vif.gam.R
 vif.gam <- function(object){
  
  obj.sum <- mgcv::summary.gam(object)
  
  s2 <- object$sig2 # estimate of standard deviation of residuals
  X <- object$model # data used to fit the model
  n <- nrow(X) # how many observations were used in fitting?
  v <- -1 # omit the intercept term, it can't inflate variance
  varbeta <- obj.sum$p.table[v,2]^2 # variance in estimates
  selected_col <- row.names(obj.sum$p.table)[v]
  selected_col <- gsub("TRUE", "", selected_col)
  varXj <- apply(X=X[, selected_col],MARGIN=2, var) # variance of all the explanatory variables
  VIF <- varbeta/(s2/(n-1)*1/varXj) # the variance inflation factor, obtained by rearranging
  # var(beta_j) = s^2/(n-1) * 1/var(X_j) * VIF_j
  
  VIF.df <- tibble::tibble(variable=names(VIF),
                           vif=VIF)
  
  return(VIF.df)
}
# vif.gam(model_01_ar1$gam) #Error in `[.data.frame`(X, , selected_col) : undefined columns selected
#mod01 <- model_01_ar1$gam
#vif.gam(mod01) #Error in `[.data.frame`(X, , selected_col) : undefined columns selected

#Residuals
#AR1
res_gamm_ar1_lme <- resid(model_01_ar1$lme, type = "normalized")
res_gamm_ar1_gam <- resid(model_01_ar1$gam, type = "deviance")
hist(res_gamm_ar1_lme) #problem with residual > 10
hist(res_gamm_ar1_gam) #problem with residual > 2
df_ar1 <- model_01_ar1$lme$data[,1:15] %>% 
  separate(muni_factor, into = c("state_name", "muni_name"), 
           sep = "_", remove = FALSE)
df_ar1$m01_res_gamm_ar1_lme <- res_gamm_ar1_lme
df_ar1$m01_res_gamm_ar1_gam <- res_gamm_ar1_gam

#log(gdp) 8.9, 9, 10, 11, 12
#Pará_Jacareacanga, Maranhão_Davinópolis, Santo Antônio dos Lopes 
df_ar1 %>% filter(m01_res_gamm_ar1_lme > 10) %>% 
  pull(m01_res_gamm_ar1_lme) %>% length() #4
df_ar1 %>% filter(m01_res_gamm_ar1_lme > 10) %>% 
  arrange(desc(m01_res_gamm_ar1_lme))
#GAm residuals
#Maranhão_Davinópolis, Santo Antônio dos Lopes, 
#Pará_Canaã dos Carajás, Barcarena
df_ar1 %>% filter(m01_res_gamm_ar1_gam > 1) %>% 
  pull(m01_res_gamm_ar1_gam) %>% length() # 31
df_ar1 %>% filter(m01_res_gamm_ar1_gam > 1) %>% 
  arrange(desc(m01_res_gamm_ar1_gam))
#summary of high residual
df_ar1 %>% filter(m01_res_gamm_ar1_gam > 1) %>% 
  group_by(state_name) %>% 
  summarise(count_high = n(), 
            count_muni_high = length(unique(muni_name)), 
            max_high = max(m01_res_gamm_ar1_gam),
            median_high = median(m01_res_gamm_ar1_gam)) %>% 
  arrange(desc(median_high))

# Temporal autocorrelation
# Only 4 with less than 18 years. Mojuí dos Campos fewest (7 years).
df_ar1 %>% 
  group_by(state_name, muni_name) %>% 
  summarise(year_count = length(unique(year)), 
            year_min = min(year), 
            year_max = max(year)) %>% 
  arrange(year_count)

#Temporal autocorrelation
dfgam %>%
  group_by(state_namef, dist_statecapital_km) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = log_gdp_percapita_reais, 
    .lags = 11
  ) -> tidy_acf_gdp

tidy_acf_gdp %>% 
  filter(lag == 1) %>% pull(ACF) %>% median() #0.826

tidy_acf_gdp %>% 
  filter(lag == 1) %>%
  group_by(state_namef) %>% 
  summarise(median_acf = median(ACF),
            min_acf = min(ACF), 
            max_acf = max(ACF)) #Max 0.893

df_ar1 %>%
  group_by(state_namef, dist_statecapital_km) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = m01_res_gamm_ar1_lme, 
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
    title = "Partial ACF (PACF)",
    subtitle = "GAMM AR(1) residuals", 
    x = "lag (year)"
  )

#Spatial pattern in residuals
#AR1
#Basic reference vectors
bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
dfstates <- data.frame(bla_state_names, bla_state_siglas)
#Municipal polygons
ibge_muni <- "vector//brazil_ninestate_municipalities//ninestate_muni.shp"
sf_ninestate_muni <- st_read(ibge_muni)

#Map with polygons
sf_ninestate_muni %>% left_join(
  df_ar1 %>%
    group_by(state_name, muni_name) %>% 
    summarise(median_resid = median(m01_res_gamm_ar1_lme), 
              sd_resid = sd(m01_res_gamm_ar1_lme)) %>% 
    left_join(dfstates, 
              by = c("state_name" = "bla_state_names")), 
  by = c("SIGLA_UF"="bla_state_siglas" ,"NM_MUN"="muni_name")
) %>% 
  filter(!is.na(median_resid)) %>%
  ggplot() + geom_sf(aes(fill = sd_resid)) + 
  scale_fill_viridis_c("residual")

# Pará  Jacareacanga, Maranhão Davinópolis
df_ar1 %>%
  group_by(state_name, muni_name) %>% 
  summarise(median_resid = median(m01_res_gamm_ar1_lme), 
            sd_resid = sd(m01_res_gamm_ar1_lme)) %>% 
              arrange(desc(sd_resid))

#Semivariograms
#Distance matrix from locations of the mayors office
##763
#City points
ibge_city <- "vector//brazil_cities//BR_Localidades_2010_v1.shp"
sf_city <- st_read(ibge_city, options = "ENCODING=WINDOWS-1252") %>% 
  filter(NM_CATEGOR == "CIDADE", CD_GEOCODM %in% all_of(sf_ninestate_muni$CD_MUN))
moji <- data.frame(CD_GEOCODM = "1504752", NM_MUNICIP = "Mojuí dos Campos", 
                   LONG = -54.6431, LAT = -2.68472, ALT = 84)
pt1 <- st_point(c(-54.6431, -2.68472))
moji$geometry <- st_sfc(pt1)
sf_moji <- st_as_sf(moji)
#Add moji missing from 2010 data
sf_city %>% select(CD_GEOCODM, NM_MUNICIP, LONG, LAT, ALT) %>% 
  bind_rows(sf_moji) -> bla_city

bla_city %>% left_join(data.frame(sf_ninestate_muni) %>% 
                         select(!geometry), 
                       by = c("CD_GEOCODM"="CD_MUN")) %>% 
  right_join(df_ar1 %>%
               group_by(state_name, muni_name) %>% 
               summarise(median_resid = median(m01_res_gamm_ar1_lme), 
                         sd_resid = sd(m01_res_gamm_ar1_lme)) %>% 
               left_join(dfstates, 
                         by = c("state_name" = "bla_state_names")), 
             by = c("SIGLA_UF" = "bla_state_siglas", "NM_MUN" = "muni_name")) %>% 
  st_transform(crs=3395)-> sf_point_residuals

library(geoR)
coords<-matrix(0,nrow(sf_point_residuals),2) 
coords[,1]<-st_coordinates(sf_point_residuals)[,'X'] 
coords[,2]<-st_coordinates(sf_point_residuals)[,'Y']   
#Median
gb<-list(data=sf_point_residuals$median_resid, coords=coords)
myvar <- variog(gb,max.dist = 1000000)
mye <- variog.mc.env(gb, obj.var = myvar)
#scale to km
mye$u <- mye$u / 1000
myvar$u <- myvar$u/1000
myvar$bins.lim <- myvar$bins.lim/1000
myvar$max.dist <- myvar$max.dist/1000
myvar$uvec <- myvar$uvec/1000
plot(myvar, var.lines=TRUE, envelope.obj = mye, xlab = "distance (km)")

#SD
gb<-list(data=sf_point_residuals$sd_resid, coords=coords)
myvar <- variog(gb, max.dist = 1000000)
mye <- variog.mc.env(gb, obj.variog = myvar)
#scale to km
mye$u <- mye$u / 1000
myvar$u <- myvar$u/1000
myvar$bins.lim <- myvar$bins.lim/1000
myvar$max.dist <- myvar$max.dist/1000
myvar$uvec <- myvar$uvec/1000
plot(myvar, var.lines=TRUE, envelope.obj = mye, xlab = "distance (km)")

#Not used
#testing tweedie
library(statmod)
library(tweedie)
myesp <- sample(log(dfgam$gdp_percapita_reais), size=1000)
tweedie_01 <- tweedie.profile(myesp ~1)
tweedie_01$xi.max

#Simple Tweedie. #cant use select=TRUE

model_01_tw <- gamm(log(gdp_percapita_reais) ~
                      s(year, by = state_namef, k=5, m=1, bs="tp") + 
                      s(dist_statecapital_km, by = state_namef) + 
                      s(state_namef, bs="re"), 
                    data = dfgam, 
                    method="REML", 
                    family = Tweedie(p=1.9))
hist(resid(model_01_tw$gam, type = "deviance"))
summary(model_01_tw$gam)

ctrlPQL <- list(keepData = TRUE)
model_01_tw_ar1 <- gamm(log(gdp_percapita_reais) ~
                          s(year, by = state_namef, k=5, m=1, bs="tp") + 
                          s(dist_statecapital_km, by = state_namef) + 
                          s(state_namef, bs="re"), 
                        correlation = corARMA(form = ~ year|muni_factor, p = 1),
                        data = dfgam, 
                        method="REML", 
                        family = Tweedie(p=1.9), 
                        niterPQL = 99,
                        control = ctrlPQL)
hist(resid(model_01_tw_ar1$gam, type = "deviance"))
summary(model_01_tw_ar1$gam)
