#data = read.csv(here::here("cleaned-data", "data-for-models-br.csv"))
  

library(tidyverse)
library(here)

#data = read.csv(here(""data.csv")) %>%  

data = read.csv(here("shiny", "data.csv")) %>%
  mutate(spt2017 = ifelse(spt2017 != "fu", spt2017, NA),
         spt2017 = factor(ifelse(!is.na(spt2017), "Used", "Not Used"), levels = c("Used", "Not Used"))) %>%
  mutate_if(is.character, as.factor)

#es = read.csv(here("effect-sizes.csv")) %>%
  
es = read.csv(here("shiny", "effect-sizes.csv")) %>%
  mutate(across(4:8, round, 2)) %>%
  mutate(rankSMD=rank(SMD, ties = "max"),
         rankPMG=rank(PMG, ties = "max"),
         rankTAU=rank(Tau, ties = "max"),
         rankLOG=rank(glmm_logit, ties = "max"),
         rankPER=rank(glmm_percent, ties = "max")) %>%
  mutate(SMD = glue::glue("{SMD} ({rankSMD}/80)"),
         PMG = glue::glue("{PMG} ({rankPMG}/80)"),
         Tau = glue::glue("{Tau} ({rankTAU}/80)"),
         glmm_logit = glue::glue("{glmm_logit} ({rankLOG}/80)"),
         glmm_percent = glue::glue("{glmm_percent} ({rankPER}/80)")
         )


get_table <- function(p, c, i){
  
  d = es %>%
    filter(participant == p,
           condition == c,
           itemType == i)
  
  p1 = d$rankSMD/80/1.1
  p2 = d$rankPMG/80/1.1
  p3 = d$rankTAU/80/1.1
  p4 = d$rankLOG/80/1.1
  p5 = d$rankPER/80/1.1
  
 l = listGroup(
    listGroupItem(paste0("dBR: ", d$SMD),
                  style = glue::glue("background-color: rgba(75, 119, 190, {p1});")),
    listGroupItem(paste0("PMG: ", d$PMG),
                  style = glue::glue("background-color: rgba(75, 119, 190, {p2});")),
    listGroupItem(paste0("Tau-U: ", d$Tau),
                  style = glue::glue("background-color: rgba(75, 119, 190, {p3});")),
    listGroupItem(paste0("Logit: ", d$glmm_logit),
                  style = glue::glue("background-color: rgba(75, 119, 190, {p4});")),
    listGroupItem(paste0("Percent: ", d$glmm_percent),
                  style = glue::glue("background-color: rgba(75, 119, 190, {p5});"))
  )
  
 return(l)
  
}

# plotting function

get_plotDat <- function(v, number){
  
  if(number=="1"){
    p = v$participant1
    i = v$itemType1
    c = v$condition1
  } else {
    p = v$participant2
    i = v$itemType2
    c = v$condition2
  }
  
  if(isTruthy(v$collapse)){
    plot_dat = data %>%
      filter(participant %in% p,
             condition == c,
             itemType == i) %>%
      group_by(participant, phase, session, spt2017) %>%
      summarize(accuracy = mean(response),
                count = sum(response)) 
    
    if(v$outcome == "accuracy"){
      
      plot_mapping = aes(x = session,
                         y = accuracy,
                         fill = phase
      )
      limits = c(0,1)
    } else {
      plot_mapping = aes(x = session,
                         y = count,
                         fill = phase
      )
      limits = c(0,20)
      
    }
    
  } else {
    plot_dat = data %>%
      filter(participant %in% p,
             condition == c,
             itemType == i) %>%
      group_by(participant, phoneme, phase, session, spt2017) %>%
      summarize(accuracy = mean(response),
                count = sum(response)) 
    
    
    if(v$outcome == "accuracy"){
      
      plot_mapping = aes(x = session,
                         y = accuracy,
                         fill = phase,
                         shape = phoneme
      )
      
      limits = c(0,1)
    } else {
      plot_mapping = aes(x = session,
                         y = count,
                         fill = phase,
                         shape = phoneme
      )
      limits = c(0,10)
      
    }
    
  }
  
  l = list(
    plot_dat, plot_mapping, limits
  )
  return(l)
  
  
}




sced_plot <- function(v){
  plotdat = v[[1]]
  plot_mapping = v[[2]]
  limits = v[[3]]
  
  
  last_session = max(plotdat$session)
  
  vert_lines = plotdat %>%
    ungroup() %>%
    select(session, phase) %>%
    filter(phase != "baseline") %>%
    distinct() %>%
    group_by(phase) %>%
    summarize(li = min(session)-1) 
  p = plotdat %>%    
    ggplot(mapping = plot_mapping) +
    geom_line(alpha = 0.3) + 
    geom_point(size = 3.0, alpha = 0.8, aes(color = spt2017)) +
    geom_vline(data = vert_lines, aes(xintercept=li), linetype = "dashed") +
    scale_x_continuous(breaks = seq(1,last_session, 2),
                       labels = seq(1,last_session, 2),
                       limits = c(1, last_session)) +
    scale_y_continuous(limits = limits) +
    theme_minimal(base_size = 16) +
    theme(axis.line=element_line(),
          legend.position = "right",
          strip.text.y = element_text(angle = 360)) +
    guides(fill = "none", color = "none")
  
  return(p)
}
