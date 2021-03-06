#sample size?
library(plyr)
library(tidyverse)
library(mgcv)
memory.limit(30000)

# load data
#file from run_analysis.R
#Years from 2006 - 2019
dfgam_matched_model <- readRDS("data/dfgam_matched_model.rds") #4998 obs. 42 vars

#Jacknife randomization to test if results depend on number of municipalities
#Identify municipalities
dfgam_matched_model %>% filter(cover_group == "less cover") %>% 
  pull(muni_factor) %>% unique() %>% as.character() -> muni_less
dfgam_matched_model %>% filter(cover_group == "more cover") %>% 
  pull(muni_factor) %>% unique() %>% as.character() -> muni_more
dfgam_matched_model %>% filter(cover_group == "more cover\nwith loss") %>% 
  pull(muni_factor) %>% unique() %>% as.character() -> muni_moreloss
#sizes to use
sample_size <- length(unique(muni_less)) #41
n_iterate <- 999
#Dataframe with equal sample sizes randomized 999 times
rbind(
#less
data.frame(run_id = rep(1:n_iterate, each = sample_size), 
           muni_id = rep(muni_less, n_iterate), 
           cover_group = "less cover"),
#more
data.frame(run_id = rep(1:n_iterate, each = length(muni_more)), 
           muni_id = rep(muni_more,n_iterate), 
           cover_group = "more cover") %>% 
   group_by(run_id) %>% slice_sample(n = sample_size) %>% 
   ungroup(),
#more with loss
data.frame(run_id = rep(1:n_iterate, each = length(muni_moreloss)), 
           muni_id = rep(muni_moreloss,n_iterate), 
           cover_group = "more cover\nwith loss") %>% 
  group_by(run_id) %>% slice_sample(n = sample_size) %>% 
  ungroup() 
) %>% 
  arrange(run_id, cover_group) -> dfruns
#join randomized sample with data
dfruns %>% select(run_id, muni_id) %>%
  crossing(year = 2006:2019) %>%
  left_join(dfgam_matched_model %>% 
              mutate(muni_id = as.character(muni_factor))) %>% 
  arrange(run_id, cover_group) %>%
  data.frame() -> dftest_random # 1720278 rows 41*3 *999*14 = 1720278

#function holding models
mygams <- function(x) {
  #run model for each randomized sample
  dftest <- x
myctrl <- list(trace = TRUE)  

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
                    rho=0.874, AR.start = dftest$start_event, 
                    family=Tweedie(1.99),
                    method = "fREML",
                    discrete = TRUE,
                    data = dftest, 
                    control = myctrl)   
#get p values
sumout_gva <- summary(bam_loss_gva)
dft_gva <- as.data.frame(sumout_gva$p.pv)
myest <- as.numeric(sumout_gva$p.coeff)#estimate
myse <- as.numeric(sumout_gva$se[1:3])#SE of estimate
rm("bam_loss_gva")
dfout_gva <- data.frame(response = "GVA", 
                        var_name = row.names(dft_gva), pval = dft_gva[,1], 
                        est = myest, est_se = myse)

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
                   rho=0.893, AR.start = dftest$start_event, 
                   family=Tweedie(1.99),
                   method = "fREML",
                   discrete = TRUE,
                   data = dftest, 
                   control = myctrl)   
#get p values
sumout_gdp <- summary(bam_loss_01)
dft_gdp <- as.data.frame(sumout_gdp$p.pv)
myest_gdp <- as.numeric(sumout_gdp$p.coeff)#estimate
myse_gdp <- as.numeric(sumout_gdp$se[1:3])#SE of estimate
rm("bam_loss_01")
dfout_gdp <- data.frame(response = "GDP", 
                        var_name = row.names(dft_gdp), pval = dft_gdp[,1], 
                        est = myest_gdp, est_se = myse_gdp)

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
                   rho=0.843, AR.start = dftest$start_event, 
                   family=Tweedie(1.337),
                   method = "fREML",
                   discrete = TRUE,
                   data = dftest, 
                   control = myctrl)   
sumout_sal <- summary(bam_loss_02)
dft_sal <- as.data.frame(sumout_sal$p.pv)
myest_sal <- as.numeric(sumout_sal$p.coeff)#estimate
myse_sal <- as.numeric(sumout_sal$se[1:3])#SE of estimate
rm("bam_loss_02")
dfout_sal <- data.frame(response = "salary", 
                        var_name = row.names(dft_sal), pval = dft_sal[,1], 
                        est = myest_sal, est_se = myse_sal)

dfout <- rbind(dfout_gdp, dfout_gva, dfout_sal)
dfout
}
#test with first 10 iterations
#rid_10 <- 1:((41*3*14)*10)#9:44-9:47
#dfcheck_10 <- plyr::ddply(dftest_random[rid_10, ], .(run_id) ,.fun = mygams)
#test with first 100 iterations
#rid_100 <- 1:((41*3*14)*100)
#dfcheck_100 <- plyr::ddply(dftest_random[rid_100, ], .(run_id) ,.fun = mygams)

#run models for each randomized sample 10
dfcheck <- plyr::ddply(dftest_random, .(run_id) ,.fun = mygams)
saveRDS(dfcheck, "data/dfcheck.RDS")
dfcheck <- readRDS("data/dfcheck.RDS")

#summary
dfcheck %>% 
  filter(!var_name== "(Intercept)") %>% 
  mutate(var_name = if_else(var_name=="cover_groupmore cover", 
                            "more cover", "more cover\nwith loss")) %>%
  mutate(flag_sig = if_else(pval <0.05,1,0)) %>% 
  group_by(var_name, response) %>% 
  summarise(total_sample = n(), 
            total_sig = sum(flag_sig)) %>% 
  mutate(prop_sig = total_sig/total_sample)
  
#dfcheck_10 %>% 
dfcheck %>% 
  filter(!var_name== "(Intercept)") %>% 
  mutate(var_name = if_else(var_name=="cover_groupmore cover", 
                            "more cover", "more cover\nwith loss")) %>%
  ggplot(aes(x=pval)) + 
  geom_histogram(bins=50) + 
  geom_vline(xintercept = 0.05, colour="blue", size=1) +
  facet_grid(response~var_name)
