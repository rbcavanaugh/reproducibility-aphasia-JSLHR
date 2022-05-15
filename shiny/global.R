#data = read.csv(here::here("cleaned-data", "data-for-models-br.csv"))


library(tidyverse)
library(here)
options(dplyr.summarise.inform = FALSE)


data = read.csv(here("data.csv")) %>%
#data = read.csv(here("shiny", "data.csv")) %>%
  mutate(
    spt2017 = ifelse(spt2017 != "fu", spt2017, NA),
    spt2017 = factor(ifelse(!is.na(spt2017), "Used", "Not Used"), levels = c("Used", "Not Used"))
  ) %>%
  mutate_if(is.character, as.factor)

es = read.csv(here("effect-sizes.csv")) %>%
#es = read.csv(here("shiny", "effect-sizes.csv")) %>%
  mutate(
    rankSMD = rank(SMD, ties = "max"),
    rankPMG = rank(PMG, ties = "max"),
    rankTAU = rank(Tau, ties = "max"),
    rankLOG = rank(glmm_logit, ties = "max"),
    rankPER = rank(glmm_percent, ties = "max"),
    rankLOG_a = rank(glmm_logit_a, ties = "max"),
    rankPER_a = rank(glmm_percent_a, ties = "max"),
    rankRAW = rank(raw_change, ties = "max"),
    rankSMD_a = rank(SMD_all, ties = "max"),
    rankPMG_a = rank(PMG_all, ties = "max"),
    rankRAW_a = rank(raw_change_all, ties = "max"),
    rankTAU_a = rank(Tau_4, ties = "max")
  ) %>%
  mutate(across(4:14, round, 2))


get_table <- function(p, c, i, adjust = FALSE, all = FALSE, tau = 0.33) {
  d = es %>%
    filter(participant == p,
           condition == c,
           itemType == i)
  
  p0 = ifelse(!all, d$rankRAW / 80, d$rankRAW_a / 80)  
  p1 = ifelse(!all, d$rankSMD / 80, d$rankSMD_a / 80)
  p2 = ifelse(!all, d$rankPMG / 80, d$rankPMG_a / 80)
  p3 = ifelse(tau == 0.33, d$rankTAU / 80, d$rankTAU_a / 80)
  p4 = ifelse(!adjust, d$rankLOG / 80, d$rankLOG_a / 80)
  p5 = ifelse(!adjust, d$rankPER / 80, d$rankPER_a / 80)
  
  rc = ifelse(!all, d$raw_change, d$raw_change_all)
  smd = ifelse(!all, d$SMD, d$SMD_all)
  pmg = ifelse(!all, d$PMG, d$PMG_all)
  tau = ifelse(tau==0.33, d$Tau, d$Tau_4)
  log = ifelse(!adjust, d$glmm_logit, d$glmm_logit_a)
  per = ifelse(!adjust, d$glmm_percent, d$glmm_percent_a)
  
  f0 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: lightblue; width: {p0*100}%\">{rc}</span>"
  )
  f1 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: lightblue; width: {p1*100}%\">{smd}</span>"
  )
  f2 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: lightblue; width: {p2*100}%\">{pmg}</span>"
  )
  f3 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: lightblue; width: {p3*100}%\">{tau}</span>"
  )
  f4 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: lightblue; width: {p4*100}%\">{log}</span>"
  )
  f5 = glue::glue(
    "<span style=\"display: inline-block; direction: ltr; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; background-color: lightblue; width: {p5*100}%\">{per}</span>"
  )
  
  
  dat = tibble(
    `Effect Size` = c(
      "Mean Change",
      "<em>d</em><sub>BR</sub>",
      "PMG",
      "Tau-U",
      "GLMM<sub>logit</sub>",
      "GLMM<sub>percent</sub>"
    ),
    `Value` = c(f0, f1, f2, f3, f4, f5)
  )
  
  t = kable(dat,
            format = "html",
            escape = FALSE,
            col.names = NULL) %>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = "150px", bold = TRUE) %>%
    column_spec(2,
                width = "200px",
                bold = TRUE,
                tooltip = "Blue bar indicates percentage rank relative to all 80 series")
  
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




sced_plot <- function(v, cap = FALSE) {
  plotdat = v[[1]]
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
    labs(y = v[[5]]) +
    theme_minimal(base_size = 16) +
    theme(
      axis.line = element_line(),
      legend.position = "bottom",
      strip.text.y = element_text(angle = 360)
    )
  
  if (cap) {
    p = p + guides(fill = "none", shape = "none")
  } else {
    p = p + guides(fill = "none", color = "none", shape = "none")
  }
  
  
  return(p)
}
