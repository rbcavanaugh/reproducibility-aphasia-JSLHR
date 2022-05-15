This web app visualizes performance for each participant in Wambaugh et al., (2017) across two condition (blocked and random) and two item types (treated, untreated generalization).

Effect sizes are calculated using the script batch-effect-sizes.Rmd at https://github.com/rbcavanaugh/reproducibility-small-N. 

Notes: 

- The blue bars on the right hand side indicate the *percentile rank* for each effect size. For example, if a PMG score was the 40th largest PMG score (out of 80), the percentile rank would 0.5 (50th percentile) and the blue bar would be halfway across the column. 

- The "Shuffle" button selects two random participants and conditions but does not change the item type or the degrees of freedom.

- Adjusting for the baseline slope for the GLMM models changes the effect size calculating. The default (no adjustment) results in an effect size that describes change predicted from the end of baseline to the end of treatment. Adjusting for the baseline trend results in an effect size that describes the change in performance at the end of treatment less performance at the same probe session without the level change and slope change parameters (i.e., assuming that the baseline trend continued at the same rate during the same number of treatment sessions, but in the absense of treatment).

- The plot is colored by the data used by Wambaugh et al., to calculate *d*~BR~. This data was also used to calculate PMG. 

- Tau-U was calculated for the case where baseline trend was > 0.33. This can be changed to 0.4.


The source code is available at https://github.com/rbcavanaugh/reproducibility-small-N/shiny. 
Please report any issues at https://github.com/rbcavanaugh/reproducibility-small-N/issues
