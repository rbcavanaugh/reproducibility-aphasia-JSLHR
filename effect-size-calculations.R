###########################################################
# Load relevant package -----------------------------------
###########################################################

library(tidyverse)
library(SingleCaseES)
library(here)
library(brms)
library(tidybayes)


###########################################################
# Create example data for testing -------------------------
###########################################################

source(here("R", "07-effect-size-function-examples.R"))    

# df = read.csv(here("cleaned-data", "data-for-models-br.csv"), stringsAsFactors = T) %>%
#   drop_na(response)

# write = df %>% nest_by(participant, session, .keep = TRUE)
# 
# parts = unique(write$participant)
# for(i in 1:length(parts)){
#   dir.create(here("study-data", parts[[i]]))
# }
# 
# for(i in 1:nrow(write)){
#   file = paste0(write$participant[[i]], "-session", write$session[[i]], ".csv")
#   participant = write$participant[[i]]
#   write.csv(write$data[[i]], row.names = FALSE, file = here("study-data", participant, file))
# }


files <- list.files(here("study-data"),
                 full.names = TRUE,
                 pattern = ".csv",
                 recursive = TRUE)

df <- files %>%
  map_dfr(read_csv,
          show_col_types = FALSE)



df_smd = df %>%
  filter(!is.na(spt2017)) %>%
  group_by(participant, phase, condition, itemType, session,
           spt2017, trials, phoneme) %>%
  summarize(correct = sum(response),
            trials = unique(trials)) %>%
  ungroup() %>%
  drop_na(itemType) %>%
  nest_by(participant, condition, itemType) %>%
  summarize(SMD_br(data))


  
  df_pmg = df %>%
  filter(!is.na(spt2017)) %>%
  group_by(participant, phase, condition, itemType, session,
           spt2017, trials, phoneme) %>%
  summarize(correct = sum(response),
            trials = unique(trials)) %>%
  drop_na(itemType) %>%
  group_by(participant, condition, itemType, phoneme) %>%
  summarize(PMG(outcome = correct, phase = spt2017, nitems = trials,
                bl_phase = "pre", tx_phase = "post"))


df_tau = df %>%
  filter(phase == "baseline" | phase == "treatment") %>%
  group_by(participant, phase, condition, itemType, session) %>%
  summarize(correct = sum(response)) %>%
  group_by(participant, condition, itemType) %>%
  summarize(Tau_custom(outcome = correct, phase = phase, 
                       bl_phase = "baseline", tx_phase = "treatment",
                       session = session))

df_tau2 = df %>%
  filter(phase == "baseline" & BR != "exclude" | phase == "treatment") %>%
  group_by(participant, phase, condition, itemType, session, BR) %>%
  summarize(correct = sum(response)) %>%
  group_by(participant, condition, itemType) %>%
  summarize(Tau_custom(outcome = correct, phase = phase, 
                       bl_phase = "baseline", tx_phase = "treatment",
                       session = session))

bl_sessions = df %>%
  filter(phase == "baseline") %>%
  select(participant, itemType, condition, session) %>%
  distinct() %>%
  group_by(participant, condition, itemType) %>%
  filter(session == (max(session))) %>%
  rename(max_bl = session)


df_itts_group = df %>%
  filter(phase == "baseline" | phase == "treatment") %>%
  left_join(bl_sessions, by = c("participant", "itemType", "condition")) %>%
  mutate(baseline_slope = session,
         level_change = ifelse(phase == "baseline", 0, 1),
         slope_change = (session - (max_bl+2))*level_change) %>%
  select(response, participant, condition, itemType, phase,
         phoneme, item, baseline_slope, level_change, slope_change) 

trials = df %>%
  filter(phase == "baseline" | phase == "treatment") %>%
  left_join(bl_sessions, by = c("participant", "itemType", "condition")) %>%
  mutate(baseline_slope = session,
         level_change = ifelse(phase == "baseline", 0, 1),
         slope_change = (session - (max_bl+2))*level_change,
         trial = 1) %>%
  select(response, participant, condition, itemType, phase, 
         trial, baseline_slope, level_change, slope_change, item) %>%
  group_by(baseline_slope, level_change, slope_change, participant, condition, itemType) %>%
  summarize(trials = sum(trial))


df_itts_group = df %>%
  filter(phase == "baseline" | phase == "treatment") %>%
  left_join(bl_sessions, by = c("participant", "itemType", "condition")) %>%
  mutate(baseline_slope = session,
         level_change = ifelse(phase == "baseline", 0, 1),
         slope_change = (session - (max_bl+2))*level_change,
         obs = 1) %>%
  select(response, participant, condition, itemType, phase,
         trials, baseline_slope, level_change, slope_change, obs) %>%
  group_by(baseline_slope, level_change, slope_change, participant, condition, itemType) %>%
  summarize(correct = sum(response),
            trials = sum(obs)) 


  mod_tx_bl <- brm(correct|trials(trials) ~ 0 + Intercept + baseline_slope + level_change + slope_change + 
               (1 + baseline_slope + level_change + slope_change | participant),
             data = df_itts_group %>% filter(condition == "blocked", itemType == "tx"),
             family = beta_binomial(),
             iter = 3000,
             warmup = 1000,
             cores = 4, chains = 4,
             prior = c(
               prior(normal(-1, 2.5), class = b, coef = Intercept),
               prior(normal(0, 2.5), class = b)
             ),
             init=0,
            # refresh = 0,
             backend = "cmdstan",
             seed = 4,
             file = "models/mod_tx_bl",
             file_refit = "on_change"
  )
  
  
  mod_gx_bl <-brm(correct|trials(trials) ~ 0 + Intercept + baseline_slope + level_change + slope_change + 
                    (1 + baseline_slope + level_change + slope_change | participant),
                   data = df_itts_group %>% filter(condition == "blocked", itemType == "gx"),
                   family = beta_binomial(),
                   iter = 3000,
                   warmup = 1000,
                   cores = 4, chains = 4,
                   init = 0,
                   prior = c(
                     prior(normal(-1, 2.5), class = b, coef = Intercept),
                     prior(normal(0, 2.5), class = b)
                   ),
                   backend = "cmdstan",
                   control = list(adapt_delta = 0.85),
                   seed = 4,
                   file = "models/mod_gx_bl",
                   file_refit = "on_change"
  )
  
  
  mod_tx_ra <- brm(correct|trials(trials) ~ 0 + Intercept + baseline_slope + level_change + slope_change + 
                     (1 + baseline_slope + level_change + slope_change | participant),
                   data = df_itts_group %>% filter(condition == "random", itemType == "tx"),
                   family = beta_binomial(),
                   iter = 3000,
                   warmup = 1000,
                   cores = 4, chains = 4,
                   #control = list(adapt_delta = 0.9),
                   init = 0,
                   prior = c(
                     prior(normal(-1, 2.5), class = b, coef = Intercept),
                     prior(normal(0, 2.5), class = b)
                   ),
                   backend = "cmdstan",
                   seed = 4,
                   file = "models/mod_tx_ra",
                   file_refit = "on_change"
  )
  
  
  mod_gx_ra <- brm(correct|trials(trials) ~ 0 + Intercept + baseline_slope + level_change + slope_change + 
                     (1 + baseline_slope + level_change + slope_change | participant),
                   # (1 |item), 
                   data = df_itts_group %>% filter(condition == "random", itemType == "gx"),
                   family = beta_binomial(),
                   iter = 3000,
                   warmup = 1000,
                   cores = 4, chains = 4,
                   control = list(adapt_delta = 0.9),
                   init = 0,
                   prior = c(
                     prior(normal(-1, 2), class = b, coef = Intercept),
                     prior(normal(0, 2), class = b)
                   ),
                   backend = "cmdstan",
                   seed = 4,
                   file = "models/mod_gx_ra",
                   file_refit = "on_change"
  )
  
  

getES = function(fit, itemType, condition){
  
  data = fit$data %>%
    group_by(level_change, participant) %>% 
    mutate(last_session = max(baseline_slope)) %>%
    filter(baseline_slope == last_session) %>%
    select(-correct) %>%
    distinct()
  
  # more conservative estimation
  # data = fit$data %>%
  #   group_by(level_change, participant) %>% 
  #   mutate(last_session = max(baseline_slope)) %>%
  #   filter(baseline_slope == last_session) %>%
  #   select(-response, -item) %>%
  #   distinct() %>%
  #   group_by(participant) %>%
  #   mutate(baseline_slope = max(baseline_slope))
  
 # print(head(data, 20))
  
  linepred = data %>%
    add_linpred_draws(fit, re_formula = ~(baseline_slope + level_change + slope_change | participant) + (1|item)) %>%
    ungroup() %>%
    mutate(timepoint = ifelse(level_change == 0, "entry", "exit")) %>%
    select(timepoint, .draw, .linpred, participant) %>%
    pivot_wider(names_from = "timepoint", values_from = .linpred) %>%
    mutate(ES = exit-entry) %>%
    group_by(participant) %>%
    point_interval(ES) %>%
    mutate(unit = "logit", itemType = itemType, condition = condition)
  
  linepredOR = data %>%
    add_linpred_draws(fit) %>%
    ungroup() %>%
    mutate(timepoint = ifelse(level_change == 0, "entry", "exit")) %>%
    select(timepoint, .draw, .linpred, participant) %>%
    pivot_wider(names_from = "timepoint", values_from = .linpred) %>%
    mutate(ES = exit-entry) %>%
    group_by(participant) %>%
    point_interval(ES) %>%
    mutate(unit = "OR", itemType = itemType, condition = condition,
           ES = exp(ES), .lower = exp(.lower), .upper = exp(.upper))
  
  epred = data %>%
    add_epred_draws(fit) %>%
    ungroup() %>%
    mutate(timepoint = ifelse(level_change == 0, "entry", "exit")) %>%
    select(timepoint, .draw, .epred, participant, trials) %>%
    pivot_wider(names_from = "timepoint", values_from = .epred) %>% 
    mutate(ES = (exit-entry)/trials) %>% 
    group_by(participant) %>%
    point_interval(ES) %>%
    mutate(unit = "pred", itemType = itemType, condition = condition)
  
  return(bind_rows(linepred, linepredOR, epred))
}
# 

es_tx_bl = getES(mod_tx_bl, "tx", "blocked")
es_tx_ra = getES(mod_tx_ra, "tx", "random")
es_gx_bl = getES(mod_gx_bl, "gx", "blocked")
es_gx_ra = getES(mod_gx_ra, "gx", "random")


smd_pmg = 
  df_br_pmg %>%
  select(participant, condition, itemType, SMD, PMG, raw_change = raw_change_exit)

tau = 
  df_tau %>%
  select(participant, condition, itemType, Tau = Est)

bglmm = 
  bind_rows(es_tx_bl, es_tx_ra, es_gx_bl, es_gx_ra) %>%
  select(participant, ES, unit, itemType, condition) %>%
  pivot_wider(names_from = unit, values_from = ES) %>%
  rename(glmm_logit = logit, glmm_OR = OR, glmm_percent = pred)

es = smd_pmg %>%
  left_join(tau, by = c("participant", "itemType", "condition")) %>%
  left_join(bglmm, by = c("participant", "itemType", "condition")) %>%
  select(-raw_change)

GGally::ggpairs(es,
                columns = 4:10,
                mapping = aes(color = itemType))

GGally::ggpairs(df_br_pmg,
                columns = 5:4,
                mapping = aes(color = itemType))

GGally::ggpairs(df_br_pmg,
                columns = c(6, 8, 10),
                mapping = aes(color = itemType))


bglmm2 = 
  bind_rows(es_tx_bl, es_tx_ra, es_gx_bl, es_gx_ra) %>%
  mutate(exceed = ifelse(unit == "OR" & .lower > 1, TRUE, 
                         ifelse(unit != "OR" & .lower > 0, TRUE, FALSE)))

sum(bglmm2$exceed)/3
