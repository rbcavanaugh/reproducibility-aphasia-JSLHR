This web app visualizes performance for each participant in Wambaugh et al., (2017) and effect sizes for each participant, condition, and item-type calculated in the paper *Reproducibility in small-N treatment research: a tutorial through the lens of aphasiology*. 

In Wambaugh et al., (2017) there were 20 participants, two conditions (blocked vs. random) and two item types (treated vs. generalization). 

**Comparing effect sizes:**

The blue bars on the right hand side indicate the *percentile rank* for each effect size. For example, if a score was the 40th largest score for that effect size (out of 80), the percentile rank would be 0.5 or the 50th percentile and the blue bar would be halfway across the column. 

If all bars are vertically aligned, this indicates that the 5 effect size measures are in rough agreement about the magnitude of change *relative to the 80 total effect sizes* calculated in Wambaugh et al., (2017). Variability in the alignment of the bars indicates potential disagreement about the magnitude of change *relative to the 80 total effect sizes*.

**Researcher degrees of freedom:**

- *d*<sub>BR</sub>,  PMG & Tau-U: You can elect to use all baseline observations or the last 5 observations of the baseline phase.

- *d*<sub>BR</sub>: You can choose to calculate collapsing across phonemes or calculating for each of the two phonemes within a condition, and averaging them per Wambaugh et al., 2015. For the latter, if there was zero baseline variability in one phoneme, the standard deviation of the other phoneme within the condition and item type was used to calculate *d*<sub>BR</sub> (noted with an asterisk "*"). The standard deviation reported for the within-phoneme option is the mean of the two standard deviations, provided the standard deviation for both phonemes was greater than 0.

- Tau-U: You can choose the cutoff for adjusting for baseline trends, either 0.4 or 0.33. 

- GLMM: You can choose to extend the baseline trend through the end of treatment (i.e., assuming that the baseline trend would have continued in the absence of treatment) or calculate the difference in performance between the end of treatment and the end of baseline. 