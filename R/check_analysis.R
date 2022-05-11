#sample size?
library(plyr)
library(tidyverse)
library(mgcv)

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
  data.frame() -> dftest # 1720278 rows 41*3 *999*14 = 1720278

#function holding model
mygam_sal <- function(x) {
  #run model for each randomized sample
  dftest <- x
myctrl <- list(trace = TRUE)  
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
sumout <- summary(bam_loss_02)
dft <- as.data.frame(sumout$p.pv)
rm("bam_loss_02")
dfout <- data.frame(var_name = row.names(dft), pval = dft[,1])
dfout
}
#test with first 100 iterations
#rid_100 <- 1:((41*3*14)*100)
#dfcheck_100 <- plyr::ddply(dftest[rid_100, ], .(run_id) ,.fun = mygam)
dfcheck_sal <- plyr::ddply(dftest, .(run_id) ,.fun = mygam_sal)
dfcheck_100 %>% 
  filter(!var_name== "(Intercept)") %>% 
  mutate(var_name = if_else(var_name=="cover_groupmore cover", 
                            "more cover", "more cover\nwith loss")) %>%
  ggplot(aes(x=pval)) + 
  geom_histogram() + 
  facet_wrap(~var_name)
