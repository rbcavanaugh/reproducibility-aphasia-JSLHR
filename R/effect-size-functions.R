###########################################################
# Load relevant package -----------------------------------
###########################################################

library(tidyverse)
library(SingleCaseES)

###########################################################
# Standardized Mean Difference {SingleCaseES} -------------
###########################################################
# note that I submitted an issue to have bl_sd added to the package
# function but for now this is the best way to do it to 
# return it with the batch functions here. 
# current issue with the SingleCaseES function is that it
# returns an error with BLVAR is 0, instead of NA and a warning

SMD_br <-  function(data){ # outcome, phase, bl_phase, tx_phase
  
  phonemes = unique(data$phoneme)
  
  if(length(phonemes)>2){
    cat("Error: Too many phonemes in dataset")
  }
  
  phoneme1 = data %>% filter(phoneme == phonemes[[1]])
  phoneme2 = data %>% filter(phoneme == phonemes[[2]])
  
  baseline_mean1 = phoneme1 %>% filter(spt2017=="pre") %>% summarize(m=mean(correct)) %>% pull(m)
  baseline_mean2 = phoneme2 %>% filter(spt2017=="pre") %>% summarize(m=mean(correct)) %>% pull(m)
  
  baseline_var1 = phoneme1 %>% filter(spt2017=="pre") %>% summarize(st=sd(correct)) %>% pull(st)
  baseline_var2 = phoneme2 %>% filter(spt2017=="pre") %>% summarize(st=sd(correct)) %>% pull(st)
  
  tx_mean1 = phoneme1 %>% filter(spt2017=="post") %>% summarize(m=mean(correct)) %>% pull(m)
  tx_mean2 = phoneme2 %>% filter(spt2017=="post") %>% summarize(m=mean(correct)) %>% pull(m)
  
  # baseline_mean = data %>% filter(spt2017=="pre") %>% summarize(m=mean(correct)) %>% pull(m)
  # baseline_var = data %>% filter(spt2017=="pre") %>% summarize(st=sd(correct)) %>% pull(st)
  # tx_mean = data %>% filter(spt2017=="post") %>% summarize(m=mean(correct)) %>% pull(m)
  # 
  note = NA
  # 
  # if(baseline_var != 0){
  #   SMD_tot = (tx_mean-baseline_mean)/baseline_var
  # } else {
  #   SMD_tot = NA
  # }
  
  if(baseline_var1 == 0 & baseline_var2 == 0){
    smd_phoneme1 = NA
    smd_phoneme2 = NA
    SMD = NA
    note = "no variance in either baseline"
  } else if(baseline_var1==0){
    baseline_var1 = baseline_var2
    note = "used phoneme2 variance"
  } else if (baseline_var2==0){
    baseline_var2 = baseline_var1
    note = "used phoneme1 variance"
  }
  
  if(baseline_var1 > 0 | baseline_var2 > 0){
    smd_phoneme1 = (tx_mean1 - baseline_mean1) / sqrt(baseline_var1)
    smd_phoneme2 = (tx_mean2 - baseline_mean2) / sqrt(baseline_var2)
    SMD = mean(smd_phoneme1, smd_phoneme2)
  }
  
  df_smd = data.frame(
    SMD1 = smd_phoneme1,
    SMD2 = smd_phoneme2,
    VAR1 = baseline_var1,
    VAR2 = baseline_var2,
    SMD = SMD,
    note = note
  )
  
  return(df_smd)
  
}


###########################################################
# TAU and TAU-U {SingleCaseES} ----------------------------
###########################################################

# we can create a function to calculate Tau or Tau-U depending on baseline trend

Tau_custom <- function(outcome, phase, session, bl_phase, tx_phase){
  
  dat = data.frame(outcome=outcome, phase=phase, session = session) %>%
    arrange(session) %>%
    mutate(session = row_number())
  
  A_data = dat[dat$phase==bl_phase,"outcome"]
  B_data = dat[dat$phase==tx_phase, "outcome"]
  
  # run a linear model and calcualte the trend
  trend = coef(lm(outcome ~ session, data = dat[dat$phase==bl_phase,]))[[2]]
  
  # if the trend is > 0.33 (Lee & Cherney, 2018)
  # Calculate Tau-U
  tau_u = Tau_U(
    A_data,
    B_data
  )
  tau = Tau(
    A_data,
    B_data
  )
  
  
  if(trend >= 0.33){
    es = tau_u
    measure = "Tau-U"
    # Otherwise just plain old tau. 
  } else {
    es = tau
    measure = "Tau"
  }
  
  es = es[1, c(1:2)]
  es$trend = round(trend, 3)
  es$measure = measure
  es$tau_u = tau_u[1, 2]
  es$tau = tau[1, 2]
  
  return(es)
  
}


###########################################################
# Proportion of Maximal Gain {custom function} ------------
###########################################################
# The proportion of items gained of the possible items 
# taking into account baseline performance. 
# raw change - possible remaining change. 

PMG = function(phase, outcome, nitems, bl_phase, tx_phase, exclude_missing = FALSE){
  
  
  dat = data.frame(
    phase = phase,
    outcome = outcome,
    nitems = nitems
  )
  
  # check for NA values
  if(sum(is.na(dat$outcome))>=1){
    stop("Error: NA values detected in A or B data. Check data or
         set `exclude_missing` to TRUE.")
  }
  
  # make sure number of trials is not less than any one observation
  if(any(dat$outcome > nitems)){
    stop("Error: outcome scores cannot be greater than the number of trials.")
  }
  
  # only accept a single value for n trials
  if(length(unique(nitems))>1){
    stop("Error: More than one value provided for nitems")
  }
  
  # calculate means
  mean_bl = mean(dat[dat$phase==bl_phase,"outcome"])
  mean_tx = mean(dat[dat$phase==tx_phase, "outcome"])
  nitems = unique(nitems)
  
  # pmg is the difference in raw score minus the number of items
  # available to be learned (nitems - baseline score)
  pmg_exit = (mean_tx-mean_bl)/(nitems-mean_bl)
  
  
  # return a table with PMG, the baseline mean, and raw change score
  pmg_table = data.frame(
    PMG = pmg_exit,
    nitems = nitems,
    baseline_score = mean_bl,
    potential_change = nitems-mean_bl,
    raw_change_exit = mean_tx-mean_bl
    
  )
  
  return(pmg_table)
}


getES = function(fit, itemType, condition){
  
  data = fit$data %>%
    group_by(level_change, participant) %>% 
    mutate(last_session = max(baseline_slope)) %>%
    filter(baseline_slope == last_session) %>%
    select(-response) %>%
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
    add_linpred_draws(fit) %>%
    ungroup() %>%
    mutate(timepoint = ifelse(level_change == 0, "entry", "exit")) %>%
    select(timepoint, item, .draw, .linpred, participant) %>%
    pivot_wider(names_from = "timepoint", values_from = .linpred) %>%
    mutate(ES = exit-entry) %>%
    group_by(participant) %>%
    point_interval(ES) %>%
    mutate(unit = "logit", itemType = itemType, condition = condition)
  # 
  # linepredOR = data %>%
  #   add_linpred_draws(fit) %>%
  #   ungroup() %>%
  #   mutate(timepoint = ifelse(level_change == 0, "entry", "exit")) %>%
  #   select(timepoint, item, .draw, .linpred, participant) %>%
  #   pivot_wider(names_from = "timepoint", values_from = .linpred) %>%
  #   mutate(ES = exit-entry) %>%
  #   group_by(participant) %>%
  #   point_interval(ES) %>%
  #   mutate(unit = "OR", itemType = itemType, condition = condition,
  #          ES = exp(ES), .lower = exp(.lower), .upper = exp(.upper))
  
  epred = data %>%
    add_epred_draws(fit) %>%
    ungroup() %>%
    mutate(timepoint = ifelse(level_change == 0, "entry", "exit")) %>%
    select(timepoint, .draw, item, .epred, participant) %>%
    pivot_wider(names_from = "timepoint", values_from = .epred) %>%
    mutate(ES = exit-entry) %>% 
    group_by(participant) %>%
    point_interval(ES) %>%
    mutate(unit = "pred", itemType = itemType, condition = condition)
  
  return(bind_rows(linepred, epred))
}
# 



