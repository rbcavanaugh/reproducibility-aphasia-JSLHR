###########################################################
# Load relevant package -----------------------------------
###########################################################

# Uncomment if running this script standalone

# library(tidyverse)
# library(SingleCaseES)

###########################################################
# Standardized Mean Difference {SingleCaseES} -------------
###########################################################
# current issue with the SingleCaseES function is that it
# returns an error with BLVAR is 0, instead of NA and a warning
# The authors of the package are in the process of updating this
# but for now here is a custom function. It is likely that the 
# batch calculator for SMD from the SingleCaseES package will
# be a better choice in the near future. 

# returns a dataframe with 8 columns
# SMD: SMD calculated per Wambaugh et al., 2017 using the last 5 baseline 
# sessions and last two treatment sessions
# SMD_all: SMD calculated using all baseline sessions and the last two treatmetn
# sessions. 
# change: the raw change score from the last five baseline sessions to last
# two treatment sessions
# change_all: the raw change using all the baseline sessions
# sd: the standard deviation for the last-5 baseline sessions
# sd_all: the standard deviation for all baseline sessions
# note & note_all: notes whether or not there was zero variance in the baseline phase

# ---------------------
# Note, this function was note used in the manuscript revision.
# ---------------------


SMD_br <-  function(phase, outcome, bl_phase, tx_phase){
  
  # create a dataframe of the input data
  dat = data.frame(
    phase = phase,
    outcome = outcome
  )
  
  # check for NA values
  if(sum(is.na(dat$outcome))>=1){
    stop("Error: NA values detected in A or B data. Check data or
         set `exclude_missing` to TRUE.")
  }
  
  # calculate means and standard deviation 
  mean_bl = mean(dat[dat$phase==bl_phase,"outcome"])
  mean_tx = mean(dat[dat$phase==tx_phase, "outcome"])
  sd_bl = sd(dat[dat$phase==bl_phase,"outcome"])

    note = NA

  if(sd_bl > 0){
    SMD = (mean_tx - mean_bl) / sd_bl
  } else {
    SMD = NA
    note = "No baseline variability to calculate SMD"
  }
  
  df_smd = data.frame(
    SMD = SMD,
    change = mean_tx - mean_bl,
    sd = sd_bl,
    note = note
  )
  
  return(df_smd)
  
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
  sd_bl = sd(dat[dat$phase==bl_phase,"outcome"])
  nitems = unique(nitems)
  
  # pmg is the difference in raw score minus the number of items
  # available to be learned (nitems - baseline score)
  pmg = (mean_tx-mean_bl)/(nitems-mean_bl)
  
  
  # return a table with PMG, the baseline mean, and raw change score
  pmg_table = data.frame(
    PMG = pmg,
    nitems = nitems,
    baseline_score = mean_bl,
    potential_change = nitems-mean_bl,
    raw_change_exit = mean_tx-mean_bl
    
  )
  
  return(pmg_table)
}


###########################################################
# TAU and TAU-U {SingleCaseES} ----------------------------
###########################################################

# we can create a function to calculate Tau or Tau-U depending on baseline trend

Tau_custom <- function(outcome, phase, session, bl_phase, tx_phase, cutoff = 0.33){
  
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
  
  
  if(trend >= cutoff){
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
# GLMM LOGIT AND PERCENTAGE {custom function} ------------
###########################################################

# To do this efficiently, we wrote a function which takes arguments for the model object,
# and an argument we called "adjust" that can be TRUE if we would like to extrapolate the
# baseline slope through the end of treatment and FALSE otherwise. The default is FALSE.
# 
# The function works be selecting rows for each participant and item in the data
# and estimating the posterior distribution for the values in each row.
# 
# Then the data is transformed and the posterior distribution
# at the beginning of treatment (or at the end of treatment without the 
#                                level change and slope change parameters) is subtracted from the posterior
# distribution at the end of treatment. 
# 
# The resulting posterior distribution characterized the magnitude of change, 
# the mean or median can be used as a point estimate and the middle 95%
# of the distribution is the 95% credible interval. 

glmmES = function(fit, itemType, condition, adjust = FALSE){
  
  # start with the data that went into the model,
  # for each participant and phase (here we just used level_change
  # because they are equivalent) make a new variable called last_session
  # which is the highest value of the baseline slope coefficient
  # then filter for only rows where the baseline slope is 
  # equal to the highest value in the phase (in other words
  # this selects the last baseline and last treatment session).
  # Remove the response column, reduce the data frame to only the
  # unique rows
  data = fit$data %>%
    group_by(level_change, participant) %>% 
    mutate(last_session = max(baseline_slope)) %>%
    filter(baseline_slope == last_session) %>%
    select(-response) %>%
    distinct() 
  # If adjust is TRUE, then for each participant, 
  # set the baseline slope to always equal the highest value
  # of baseline slope. 
  # in other words, we end up with a row where baseline slope
  # represents the last treatment session, but level and slope
  # change are still zero.
  if(adjust){
    data = data %>%
      group_by(participant) %>%
      mutate(baseline_slope = max(baseline_slope))
  }
  
  # calculate an effect size in logits or log-odds
  # start with the data frame we just created and add
  # model draws from the linear/link-level predictor
  # change timepoint to entry if level change is 
  # 0 and exit if 1. Select only the needed columns
  # and take the data from long to wide. using pivot_wider
  # Then subtract the value for the draw for entry from 
  # the draw for exit. 
  # then for each participant, calculate the point
  # estimate and interval using point_interval
  # the last line just adds a column indicating this effect size is in logits
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
  
  # This block does the same thing, except using the expectation of the
  # posterior (i.e., in percent correct terms) 
  epred = data %>%
    add_epred_draws(fit) %>%
    ungroup() %>%
    mutate(timepoint = ifelse(level_change == 0, "entry", "exit")) %>%
    select(timepoint, .draw, item, .epred, participant) %>%
    pivot_wider(names_from = "timepoint", values_from = .epred) %>%
    mutate(ES = exit-entry) %>% 
    group_by(participant) %>%
    point_interval(ES) %>%
    mutate(unit = "percent", itemType = itemType, condition = condition)
  
  return(bind_rows(linepred, epred))
}
# 



