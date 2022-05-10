#gdp GAM models
#packages
library(tidyverse)
library(mgcv)
library(stringi)
library(timetk)
library(gratia)
library(sf)
library(gridExtra)
library(cowplot)

#guides
#bam
#https://jacolienvanrij.com/Tutorials/GAMM.html#model-terms-partial-effects
#http://jacolienvanrij.com/PupilAnalysis/SupplementaryMaterials-2.html

#extra memory to speed up models, pairs panel, gam.check etc
memory.limit(80000)

#Uses dfgam from "gdp_analysis.R"
dfgam <- readRDS("dfgam.rds") #13710 obs. 55 vars
dfgam$log_gdp_percapita_reais <- log(dfgam$gdp_percapita_reais)
#identify start of each timeseries (for AR.start)
dfgam %>% 
arrange(muni_factor, year) %>% 
  group_by(muni_factor) %>%
  mutate(start_year = min(year)) %>% 
  mutate(start_event = year== start_year) %>% 
  ungroup() -> dfgam

dfgam_matched_model %>% 
  arrange(muni_factor, year) %>% 
  group_by(muni_factor) %>%
  mutate(start_year = min(year)) %>% 
  mutate(start_event = year== start_year) %>% 
  ungroup() -> dfgam_matched_model
hist(log(dfgam_matched_model$gva_agri_percapita_reais))
# get rho value for AR1 component
dfgam %>%
  group_by(state_namef, dist_statecapital_km) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = , 
    .lags = 11
  ) -> tidy_acf_gva



dfgam_matched_model %>%
  group_by(state_namef, dist_statecapital_km) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = log(gva_agri_percapita_reais), 
    .lags = 11
  ) -> tidy_acf_gva

tidy_acf_gva %>% 
  filter(lag == 1) %>% pull(ACF) %>% median() #0.696

tidy_acf_gva %>% 
  filter(lag == 1) %>%
  group_by(state_namef) %>% 
  summarise(median_acf = median(ACF),
            min_acf = min(ACF), 
            max_acf = max(ACF), 
            max_pacf = max(PACF)) #Max 0.874
#tweedie 1.99
gam_gva_tw <- gam(log_gva_percapita_reais~1 ,
     family=tw(),
     method = "fREML",
     data = dfgam_matched_model)


#random smooths adjust the trend of a numeric predictor 
#in a nonlinear way: s(Time, Subject, bs="fs", m=1).
myctrl <- list(keepData = TRUE, trace = TRUE)  
bam_000 <- bam(log_gdp_percapita_reais~ 
                #Spatial smooth
                s(long, lat) + 
                #Spatial proximity
                s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                 #Time
                 s(year, state_namef, bs='fs', m=1) +
                    #s(year, by = state_namef) +
                    s(yearf, bs = "re") +
                #Random 
                 #temporal smooth. 3.2 GB
                #s(year, muni_namef, bs='fs', m=1) + 
                 s(state_namef, bs="re") + 
                 s(muni_factor, bs="re") + 
                #time varying covariates
                s(tot_loss5y_percent) +
                s(school_per1000) +
                s(process_gold_p1000) + 
                 s(gva_industry_percent) +
                s(gva_agri_percapita_reais) +
               ti(gva_agri_percapita_reais, gva_industry_percent), 
              #AR1 residual errors
              rho=0.893, AR.start = dfgam$start_event, 
              family=Tweedie(1.99),
              method = "fREML",
              discrete = TRUE,
              data = dfgam, 
              control = myctrl)   
saveRDS(bam_000, "bam_000.rds")
bam_000 <- readRDS("bam_000.rds")
#Residuals
res_bam_ar1_000 <- resid(bam_000, type = "deviance")
hist(res_bam_ar1_000) #
summary(bam_000) #0.967
#appraise(bam_000) # do not use locks process/memory
plot(bam_000, scale = 0, all.terms = TRUE)

my_check <- function(x, mod_name = NA){
  myres <-  resid(x, type = "deviance") 
  my_r2 <- summary(x)$r.sq
  my_devexp <- summary(x)$dev.expl
  my_formula <- x$formula
  my_formula_text <- stri_wrap(my_formula, 80, simplify=TRUE)
  my_formula_lines <- paste(my_formula_text[-1], collapse="\n")
  my_formula_title <- paste(mod_name, my_formula_lines, sep=":   ")
  hist_label <- paste("r2adj:",round(my_r2,2),"dev_exp:", round(my_devexp,2))
  val_limit <- max(abs(myres)) 
  #plot
  dfgam %>% 
    ggplot(aes(x=myres)) + 
    geom_histogram() + 
    scale_x_continuous(limits=c(-val_limit, val_limit)) +
    labs(subtitle = hist_label,
         x="residual", y="count") -> fig_hist
  ymax <-  max(layer_scales(fig_hist)$y$range$range)
  fig_hist +
    geom_jitter(aes(x=myres, y= (ymax + (0.05*ymax))), 
                width=0, height=60) -> fig_hist
  #
  
  my_fitted <- x$fitted.values
  my_y <- bam_000$y
  cor_res <- cor.test(my_fitted, my_y)
  cor_label <- paste("correlation:", round(cor_res$estimate,2))
  dfgam %>% 
    ggplot(aes(x=my_fitted, y=my_y)) + 
    geom_point() + labs(x="fitted", y="observed") + 
    coord_equal() + 
    labs(subtitle = cor_label)-> fig_corr
  
  #grid.arrange(fig_hist, fig_corr, nrow = 1)
  plot_row <- plot_grid(fig_hist, fig_corr, align = "h")
  # now add the title
  title <- ggdraw() + 
    draw_label(
      my_formula_title,
      #fontface = 'bold', 
      size = 8,
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  plot_grid(
    title, plot_row,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.33, 1)
  )
}
my_check(bam_000, mod_name ="bam_000")


dfgam$res_bam_ar1_000 <- res_bam_ar1_000
dfgam$fit_bam_ar1_000 <- fit_bam_ar1_000



#Temporal autocorrelation
dfgam %>%
  group_by(state_namef, dist_statecapital_km) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = res_bam_ar000, 
    .lags = 11
  ) -> tidy_acf

tidy_acf %>% 
  filter(lag==1, ACF > 0) %>% 
  group_by(state_namef) %>% 
  summarise(median_upper = median(.white_noise_upper),
            median_pacf = median(PACF), 
            quant95_pacf = quantile(PACF, probs = 0.95),
            max_pacf = max(PACF)
            ) # max values all > 0.75

#export as .png  250 * 1000
tidy_acf %>% 
  filter(state_namef == "Mato Grosso") %>%
  ggplot(aes(x = lag, y = ACF, color = state_namef, 
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
    title = "AutoCorrelation (ACF)",
    subtitle = "GAMM AR(1) residuals", 
    x = "lag (year)"
  )
#PACF
tidy_acf %>% 
  filter(state_namef == "Mato Grosso") %>%
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

#Spatial autocorrelation
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

dfstates <- data.frame(bla_state_names, bla_state_siglas)

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=downloads
ibge_muni <- "vector//brazil_ninestate_municipalities//ninestate_muni.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% filter(SIGLA_UF %in% bla_state_siglas)
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
  right_join(dfgam %>%
               group_by(state_name, muni_name) %>% 
               summarise(median_resid = median(res_bam_ar000), 
                         sd_resid = sd(res_bam_ar000)) %>% 
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

#repeat with salary
#1.337
gam_loss_01tw <- gam(min_salary_mean ~ 1, 
                     family= tw(),
                     method = "fREML",
                     data = dfgam_matched_model)
# get rho value for AR1 component
dfgam_matched_model %>%
  group_by(state_namef, dist_statecapital_km) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = min_salary_mean, 
    .lags = 11
  ) -> tidy_acf_salary

tidy_acf_salary %>% 
  filter(lag == 1) %>% pull(ACF) %>% median() #0.38

tidy_acf_salary %>% 
  filter(lag == 1) %>%
  group_by(state_namef) %>% 
  summarise(median_acf = median(ACF),
            min_acf = min(ACF), 
            max_acf = max(ACF), 
            max_pacf = max(PACF)) #Max 0.843



#Below nnotes and testing
#residals not great for below
bam_00 <- bam(log_gdp_percapita_reais~ 
                #spatial smooth
                s(long, lat) + 
                #Spatial proximity
                s(dist_statecapital_km, by = state_namef) +
                #random temporal smooth. 3.2 GB
                s(year, muni_namef, bs='fs', m=1) + 
                #time varying covariates
                s(tot_loss5y_percent, by  = state_namef) +
                s(school_per1000, by  = state_namef) +
                s(process_gold_p1000) +
                s(gva_agri_percapita_reais, by  = state_namef), 
              #AR1 residual errors
              rho=0.9, AR.start = dfgam$start_event,
              method = "fREML",
              discrete = TRUE,
              data = dfgam, 
              control = myctrl)         
hist(resid(bam_00, type = "deviance")) #
summary(bam_00) #0.618
plot(bam_00, scale = 0, all.terms = TRUE)
saveRDS(bam_00, "bam_00.rds")
bam_00 <- readRDS("bam_00.rds")

#Include spatial and temporal autocorrelation
#get tweedie p. takes few minutes even for simple example
#gam_null_tw <- gam(log(gdp_percapita_reais) ~ 1,
#                   family = tw,
#                  data = dfgam)
#gam_null_tw$family$family #p=1.99
#appraise for simle tweedie bam gobbles all memory and locks
#bam_01 <- bam(log_gdp_percapita_reais~ s(gva_agri_percapita_reais), 
#              family=Tweedie(1.99), 
#              method = "REML",
#              data = dfgam)
#hist(resid(bam_01, type = "deviance"))
#summary(bam_01) #0.563
#plot(bam_01, scale = 0, all.terms = TRUE)
#appraise(bam_01)