#data = read.csv(here::here("cleaned-data", "data-for-models-br.csv"))


library(tidyverse)
library(here)
library(markdown)
library(shiny)
library(GGally)
options(dplyr.summarise.inform = FALSE)

set_shiny_plot_height <- function(session, output_width_name){
  function() { 
    session$clientData[[output_width_name]]
  }
}

#data = read.csv(here("data.csv")) %>%
data = read.csv(here("shiny", "data.csv")) %>%
  filter(phase == "baseline" | phase == "treatment") %>%
  mutate(
    spt2017 = ifelse(spt2017 != "fu", spt2017, NA),
    spt2017 = factor(ifelse(!is.na(spt2017), "Used", "Not Used"), levels = c("Used", "Not Used"))
  ) %>%
  mutate_if(is.character, as.factor)

formatround = function(b){
  tmp = format(round(b, 2), nsmall = 2)
  return(tmp)
}

es_raw = read.csv(here("shiny", "effect-sizes.csv"))
  
#es = read.csv(here("effect-sizes.csv")) %>%
es = read.csv(here("shiny", "effect-sizes.csv")) %>%
  mutate(
    rankSMD = rank(SMD, ties = "max", na.last = FALSE),
    rankPMG = rank(PMG, ties = "max"),
    rankTAU = rank(Tau, ties = "max"),
    rankLOG = rank(glmm_logit, ties = "max"),
    rankPER = rank(glmm_percent, ties = "max"),
    
    rankSMD_a = rank(SMD_all, ties = "max", na.last = FALSE),
    rankPMG_a = rank(PMG_all, ties = "max"),
    rankTAU_a = rank(Tau_4, ties = "max"),
    rankLOG_a = rank(glmm_logit_a, ties = "max"),
    rankPER_a = rank(glmm_percent_a, ties = "max"),
    
    rankSMDph = rank(SMD_phoneme, ties = "max", na.last = FALSE),
    rankSMDph_a = rank(SMD_phoneme_all, ties = "max", na.last = FALSE),
    
    rankTAU_last5 = rank(Tau_last5, ties = "max", na.last = FALSE),
    rankTAU_a_last5 = rank(Tau_last5_4, ties = "max", na.last = FALSE),
    
    rankRAW = rank(raw_change, ties = "max"),
    rankRAW_a = rank(raw_change_all, ties = "max"),
    
    rankBL = rank(baseline_score, ties = "max"),
    rankBL_a = rank(baseline_score_all, ties = "max"),
    
    ranksd = rank(sd, ties = "max"),
    ranksd_a = rank(sd_all, ties = "max"),
    ranksdph = rank(sd_phoneme, ties = "max", na.last = FALSE),
    ranksdph_a = rank(sd_phoneme_all, ties = "max", na.last = FALSE)
    
  ) %>%
  mutate(rankSMD = ifelse(is.na(SMD), 0, rankSMD),
         rankSMD_a = ifelse(is.na(SMD_all), 0, rankSMD_a),
         rankSMDph = ifelse(is.na(SMD_phoneme), 0, rankSMDph),
         rankSMDphall = ifelse(is.na(SMD_phoneme_all), 0, rankSMDph_a),
         across(4:27, formatround),
         SMD_phoneme = ifelse(imputed>0.5, paste0(SMD_phoneme, "*"), SMD_phoneme),
         SMD_phoneme_all = ifelse(imputed_all>0.5, paste0(SMD_phoneme_all, "*"), SMD_phoneme_all)
  )

            

get_table <- function(p, c, i, adjust = FALSE, all = FALSE, tau = 0.33, collapse = TRUE) {
  d = es %>%
    filter(participant == p,
           condition == c,
           itemType == i)
  
  pmean = ifelse(!all, d$rankRAW, d$rankRAW_a)  
  if(collapse){
    p1 = ifelse(!all, d$rankSMD, d$rankSMD_a)
    psd = ifelse(!all, d$ranksd, d$ranksd_a)
  } else {
    p1 = ifelse(!all, d$rankSMDph, d$rankSMDph_a)
    psd = ifelse(!all, d$ranksdph, d$ranksdph_a)
  }
  
  p2 = ifelse(!all, d$rankPMG, d$rankPMG_a)
  # p3 = ifelse(tau == 0.33,
  #             d$rankTAU, d$rankTAU_a)
  
  if(tau == 0.33){
    if(all){
      p3 = d$rankTAU
    } else {
      p3 = d$rankTAU_last5
    }
  } else {
    if(all){
      p3 = d$rankTAU_a
    } else {
      p3 = d$rankTAU_a_last5
    }
  }
  
  
  p4 = ifelse(!adjust, d$rankLOG, d$rankLOG_a)
  p5 = ifelse(!adjust, d$rankPER, d$rankPER_a)
  pbl  = ifelse(!all, d$rankBL, d$rankBL_a)
  
  
  rc  = ifelse(!all, d$raw_change, d$raw_change_all)
  
  if(collapse){
    smd = ifelse(!all, d$SMD, d$SMD_all)
    sd = ifelse(!all, d$sd, d$sd_all)
  } else {
    smd = ifelse(!all, d$SMD_phoneme, d$SMD_phoneme_all)
    sd = ifelse(!all, d$sd_phoneme, d$sd_phoneme_all)
  }
  
  pmg = ifelse(!all, d$PMG, d$PMG_all)
  if(tau == 0.33){
      if(all){
        tau = d$Tau
      } else {
        tau = d$Tau_4
      }
  } else {
      if(all){
        tau = d$Tau_last5
      } else {
        tau = d$Tau_last5_4
      }
  }
  log = ifelse(!adjust, d$glmm_logit, d$glmm_logit_a)
  per = ifelse(!adjust, d$glmm_percent, d$glmm_percent_a)
  bl  = ifelse(!all, d$baseline_score, d$baseline_score_all)
  
  
  fmean = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; font-weight: normal; border-radius: 4px; padding-right: 2px; background-color: rgb(211,211,211, 0.4); width: {pmean/80*100}%\">{pmean}/80</span>"
  )
  fbl = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext;  font-weight: normal; border-radius: 4px; padding-right: 2px; background-color: rgb(211,211,211, 0.4); width: {pbl/80*100}%\">{pbl}/80</span>"
  )
  fvar = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext;  font-weight: normal; border-radius: 4px; padding-right: 2px; background-color: rgb(211,211,211, 0.4); width: {psd/80*100}%\">{psd}/80</span>"
  )
  f1 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: rgb(173,216,230, 0.5); width: {p1/80*100}%\">{p1}/80</span>"
  )
  f2 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: rgb(173,216,230, 0.5); width: {p2/80*100}%\">{p2}/80</span>"
  )
  f3 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: rgb(173,216,230, 0.5); width: {p3/80*100}%\">{p3}/80</span>"
  )
  f4 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: rgb(173,216,230, 0.5); width: {p4/80*100}%\">{p4}/80</span>"
  )
  f5 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: rgb(173,216,230, 0.5); width: {p5/80*100}%\">{p5}/80</span>"
  )
  
  
  dat = tibble(
    `Effect Size` = c(
      "<em>d</em><sub>BR</sub>",
      "PMG",
      "Tau-U",
      "GLMM<sub>logit</sub>",
      "GLMM<sub>percent</sub>",
      "",
      "<span style=\"font-weight:normal;\">Mean Change</span>",
      #"<span style=\"font-weight:normal;\">Baseline Mean</span>",
      "<span style=\"font-weight:normal;\"><em>sd</em><sub>baseline</sub></span>"
    ),
    `Value` = c(smd, pmg, tau, log, per, "", rc, sd),
    `Percentile Rank` = c(f1, f2, f3, f4, f5, "", fmean, fvar)
  )
  
  t = kable(dat,
            format = "html",
            escape = FALSE) %>%
    kable_styling(full_width = T, bootstrap_options = "condensed") %>%
    column_spec(1, width = "250px", bold = TRUE) %>%
    column_spec(2,
                width = "200px",
                bold = FALSE,
                tooltip = "Blue bar indicates percentage rank relative to all 80 series") %>%
    column_spec(3, width = "400px")
  
  return(t)
  
}

# plotting function

get_plotDat <- function(v, number) {
  if (number == "1") {
    p = v$participant1
    i = v$itemType1
    c = v$condition1
  } else {
    p = v$participant2
    i = v$itemType2
    c = v$condition2
  }
  
  if (isTruthy(v$collapse)) {
    plot_dat = data %>%
      filter(participant %in% p,
             condition == c,
             itemType == i) %>%
      group_by(participant, phase, session, spt2017) %>%
      summarize(accuracy = mean(response),
                count = sum(response))
    
    if (v$outcome == "accuracy") {
      yl = "Percent Accuracy"
      
      plot_mapping = aes(x = session,
                         y = accuracy,
                         fill = phase)
      limits = c(0, 1)
    } else {
      yl = "Number correct"
      plot_mapping = aes(x = session,
                         y = count,
                         fill = phase)
      limits = c(0, 20)
      
    }
    
  } else {
    plot_dat = data %>%
      filter(participant %in% p,
             condition == c,
             itemType == i) %>%
      group_by(participant, phoneme, phase, session, spt2017) %>%
      summarize(accuracy = mean(response),
                count = sum(response))
    
    
    if (v$outcome == "accuracy") {
      yl = "Percent Accuracy"
      plot_mapping = aes(
        x = session,
        y = accuracy,
        fill = phase,
        shape = phoneme
      )
      
      
      
      limits = c(0, 1)
    } else {
      yl = "Number correct"
      plot_mapping = aes(
        x = session,
        y = count,
        fill = phase,
        shape = phoneme
      )
      limits = c(0, 10)
      
      
    }
    
  }
  
  l = list(plot_dat, plot_mapping, limits, c(p, i, c), yl)
  return(l)
  
  
}




sced_plot <- function(v, cap = FALSE, all = FALSE) {
  plotdat = v[[1]]
  print(head(plotdat))
  
  if(all){
    plotdat = plotdat %>%
      mutate(
        spt2017 = as.character(spt2017),
        spt2017 = factor(
          ifelse(phase == "baseline", "Used", spt2017),
          levels = c("Used", "Not Used"))
      ) 
  }
  
  plot_mapping = v[[2]]
  limits = v[[3]]
  title = paste0(v[[4]][1], "; ", v[[4]][2], "; ", v[[4]][3])
  
  last_session = max(plotdat$session)
  
  vert_lines = plotdat %>%
    ungroup() %>%
    select(session, phase) %>%
    filter(phase != "baseline") %>%
    distinct() %>%
    group_by(phase) %>%
    summarize(li = min(session) - 1)
  
  p = plotdat %>%
    ggplot(mapping = plot_mapping) +
    geom_line(alpha = 0.3) +
    geom_point(size = 3.0, alpha = 0.8, aes(color = spt2017)) +
    geom_vline(data = vert_lines, aes(xintercept = li), linetype = "dashed") +
    scale_x_continuous(
      breaks = seq(1, last_session, 4),
      labels = seq(1, last_session, 4),
      limits = c(1, last_session)
    ) +
    scale_y_continuous(limits = limits, labels = ifelse(v[[5]] == "Percent Accuracy", scales::percent, scales::number)) +
    labs(y = v[[5]], color = "Used to ") +
    theme_minimal(base_size = 16) +
    theme(
      axis.line = element_line(),
      legend.position = "bottom",
      strip.text.y = element_text(angle = 360)
    )
  
  if (cap) {
    p = p + guides(fill = "none", shape = "none", color = "none")
  } else {
    p = p + guides(fill = "none", color = "none", shape = "none")
  }
  
  
  return(p)
}
