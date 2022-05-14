load(here("manuscript", "tables-plots.RData"))
load(here("manuscript", "tables-plots2.RData"))


# plot 1

p1.man = p1 +
  theme_grey(base_size = 13) + 
  labs(caption = NULL,
       x = "Probe Session Number",
       title = NULL) + 
  annotate("text", x=1, y=.975, label = "baseline", size = 4, hjust =.3, fontface = "italic") +
  annotate("text", x=8, y=.975, label = "treatment", size = 4, hjust =0.2, fontface = "italic") +
  scale_x_continuous(breaks = seq(0,26, 2)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1), limits = c(0,1))

p1.man

ggsave(p1.man, filename = here("manuscript", "p1.png"), bg = "white", width = 8, height = 5, dpi = 400)

p2.man = p2 + 
  labs(caption = NULL,
       x = "Probe Session Number",
       title = NULL) + 
  annotate("text", x=1, y=.975, label = "baseline", size = 4, hjust =.3, fontface = "italic") +
  annotate("text", x=8, y=.975, label = "treatment", size = 4, hjust =0.2, fontface = "italic") +
  scale_x_continuous(breaks = seq(0,26, 2)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1), limits = c(0,1))

p2.man

ggsave(p2.man, filename = here("manuscript", "p2.png"), bg = "white", width = 8, height = 5, dpi = 400)


