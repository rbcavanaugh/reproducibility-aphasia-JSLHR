This web app visualizes performance for each participant in Wambaugh et al., (2017) and effect sizes for each participant, condition, and item-type calculated in the paper *Reproducibility in small-N treatment research in aphasia and related disorders: a tutorial*.

In Wambaugh et al., (2017) there were 20 participants, two conditions (blocked vs. random) and two item types (treated vs. generalization) resulting in 80 total effect size calculations. 

**Comparing effect sizes:**

The blue bars on the right hand side indicate the *percentile rank* for each effect size. For example, if a score was the 40th largest score for that effect size (out of 80), the percentile rank would 0.5 or the 50th percentile and the blue bar would be halfway across the column. 

If all bars are vertically aligned, this indicates that the 5 effect size measures are in rough agreement about the magnitude of change *relative to the 80 total effect sizes* calculated in Wambaugh et al., (2017). Variability in the alignment of the bars indicates potential disagreement about the magnitude of change *relative to the 80 total effect sizes*.

**Researcher degrees of freedom:**

- *d*<sub>BR</sub>~ & PMG: You can elect to use all baseline observations or the last 5 observations of the baseline phase.

- Tau-U: You can choose the cutoff for adjusting for baseline trends, either 0.4 or 0.33.

- GLMM: You can choose to extrapolate the baseline trend through the end of treatment (i.e., assuming that the baseline trend would have continued in the absense of treatment) or calculate the difference in performance between the end of treatment and the end of baseline. 

The source code is available at https://github.com/rbcavanaugh/reproducibility-small-N/shiny. 
Please report any issues at https://github.com/rbcavanaugh/reproducibility-small-N/issues
