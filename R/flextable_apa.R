theme_apa <- function(ft) {
  ft %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 12, part = "all") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::rotate(rotation = "lrtb", align = "top", part = "header") %>%
    flextable::rotate(rotation = "lrtb", align = "top", part = "body") %>%
    flextable::border_remove() %>%
    flextable::hline_top(
      border = officer::fp_border(width = 2),
      part = "all"
    ) %>%
    flextable::hline_bottom(
      border = officer::fp_border(width = 2),
      part = "all"
    ) %>%
    flextable::autofit()
}

library(flextable)
library(officer)

ft <- flextable(cols)
ft <- theme_apa(ft)

ft

ft2 <- flextable(head(df, 5))
ft2 <- theme_apa(ft2)

save_as_image(ft2, path = here("table1.png"))



ft3 = flextable(P10 %>%
  select(phase, baseline_slope, level_change, slope_change) %>%
  distinct() %>%
  arrange(baseline_slope)
)

save_as_image(ft3, path = here("table3.png"))
