library(tidyverse)

# data for participant 10
pred_dat = tibble::tribble(
  ~session, ~phase, ~spt2017, ~sum_correct, ~slope_change,
  1,       0,       NA,            3,             0,
  2,       0,    "pre",            1,             0,
  3,       0,    "pre",            3,             0,
  4,       0,    "pre",            4,             0,
  5,       0,    "pre",            3,             0,
  6,       0,    "pre",            3,             0,
  8,       1,       NA,           10,             0,
  10,      1,       NA,           11,             2,
  12,      1,       NA,           13,             4,
  14,      1,       NA,           15,             6,
  16,      1,       NA,           18,             8,
  18,      1,       NA,           17,            10,
  20,      1,       NA,           19,            12,
  22,      1,       NA,           19,            14,
  24,      1,   "post",           18,            16,
  26,      1,   "post",           19,            18
)

# regular old glm model aggregated binomial
mod = glm(cbind(sum_correct, 20-sum_correct) ~ session + phase + slope_change, 
          family = binomial,
          data = pred_dat
)

# the fitted line from the model on the response scale (percept)
pred_dat$preds = predict(mod, type = "response")

# make another dataframe, this time with no slope change to show
# a dashed line that has the baseline slope and level change
# incorporated but no slope change
new_dat = pred_dat %>%
  mutate(slope_change = 0)

new_dat$preds = predict(mod, newdata = new_dat, type = "response")

# make another dataframe, this time with no slope change or level change
# to show the continuing baseline slope to the point of level change
new_dat2 = pred_dat %>%
  mutate(phase = 0, slope_change =0)

new_dat2$preds = predict(mod, newdata = new_dat2, type = "response")

# only show it for sessions before 9
new_dat2 = new_dat2 %>%
  filter(session<9)

# here's the plot
# I commented out the "ggbrace" line that creates the level change
# brace and label so you don't
# have to install that additional package. If you want to add those, 
# see here: https://github.com/NicolasH2/ggbrace and then 
# uncomment the line starting with ggbrace::
pred_dat %>%
  ggplot(aes(x = session, y = sum_correct/20, group = phase)) +
  geom_point(size = 4) + 
  geom_line(alpha = 0.25) +
  geom_vline(aes(xintercept = 7), linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,30,5)) +
  labs(title = "Participant 10, treated words, blocked condition",
       y="Percent Correct") +
  guides(alpha = "none") +
  # fitted line
  geom_line(inherit.aes = FALSE, data = pred_dat,
            aes(x=session, y = preds, group = phase), color = "darkred") + 
  # baseline slope plus level change line
  geom_line(inherit.aes = FALSE, data = new_dat,
            aes(x=session, y = preds, group = phase), color = "darkred", alpha = 0.3, linetype = "dashed")+
  # extended baseline slope dashed line
  geom_line(inherit.aes = FALSE, data = new_dat2,
            aes(x=session, y = preds, group = phase), color = "darkred", alpha = 0.3, linetype = "dashed")+
  #ggbrace::geom_brace(aes(c(8,9), c(0.19, 0.5),
  # label = "level change"), inherit.data=F, rotate = 90, labelsize = 4, color = "darkred") +
  annotate(color = "darkred",
           geom = "curve", x = 4, y = .35, xend = 2, yend = .15, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 1, y = .375, label = "baseline slope",
           hjust = "left", color = "darkred") +
  annotate(color = "darkred",
           geom = "curve", x = 4, y = .35, xend = 2, yend = .15, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 1, y = .375, label = "baseline slope",
           hjust = "left", color = "darkred") +
  annotate(color = "darkred",
           geom = "curve", x = 15, y = .65, xend = 15, yend = .76, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"), ends = "both")
  ) +
  annotate(color = "darkred",
           geom = "text", x = 15.75, y = .77, hjust = "left",
           label = "slope change"
  ) 

