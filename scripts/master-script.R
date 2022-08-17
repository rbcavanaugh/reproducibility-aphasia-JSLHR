# Script to render all files in order
library(here)

# Render Supplemental Materials First.
# Includes calculation of all effect sizes for bayesian mixed-effects approach
# And figure 3.

# S1. Supplemental
rmarkdown::render(here("scripts", "S1-reproducible-effect-sizes.Rmd"))

# S2. Supplemental
rmarkdown::render(here("scripts", "S1-reproducible-effect-sizes.Rmd"))

# Render Quarto manuscript
# Can't run rmarkdown::render() the same way with quarto documents
# Instead, quarto documents are run by clicking on "render" when they are
# open or by running 'quarto render name-of-doc.qmd' in the terminal.
# to render the document from an R-script, we can use the 'system()' command
# which runs a string in the terminal.

system("quarto render manuscript/reproducibility-replication-manuscript-Qmd-REV.qmd")