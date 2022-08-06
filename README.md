# Reproducibility in small-N treatment research in aphasia and related disorders: a tutorial

**This repository contains data, analytical code, supplemental materials, and a pre-print for the above titled manuscript currently under review**

Robert Cavanaugh,<sup>1,2</sup> Yina M. Quique,<sup>3</sup> Alexander M. Swiderski,<sup>1,2,4</sup> Lydia Kallhoff,<sup>5</sup> Lauren Terhorst,<sup>6</sup> Julie Wambaugh,<sup>5</sup> William D. Hula,<sup>2, 1</sup> William S. Evans<sup>1</sup>

1. University of Pittsburgh, Department of Communication Sciences and Disorders
2. VA Pittsburgh Healthcare System, Audiology and Speech Pathology Program
3. Center for Education in Health Sciences, Northwestern University & Shirley Ryan Ability Lab
4. Carnegie Mellon University, Center for Neural Basis of Cognition
5. University of Utah, Department of Communication Sciences and Disorders
6. University of Pittsburgh, Department of Occupational Therapy

The authors have no disclosures.

Key words: aphasia treatment reproducibility replication

Corresponding Author:  
Robert Cavanaugh M.S. CCC-SLP  
rob.cavanaugh@pitt.edu

Note: Shiny app is located here: https://rb-cavanaugh.shinyapps.io/SCED/

## Directory organization

- /manuscript contains Rmarkdown file with reproducible manuscript
- /R contains script with effect size functions
- /study-data contains data used in the tutorial
- /supplemental-materials contains two Rmarkdown files for S1 and S2
- /models contains saved {brms} model objects (note: large files)
- /shiny contains code for shiny app

## Running the code

These materials use Rprojects and the {here} package to manage file paths. More
information is located here: https://www.tidyverse.org/articles/2017/12/workflow-vs-script/

1. Begin by opening the reproducibility-aphasia-JSLHR.Rproj file in RStudio

2. You will need to have the following packages installed

```r
# main packages used
install.packages(c("here", "tidyverse", "brms", "officer", "flextable",
"GGally", "SingleCaseES", "lme4", "emmeans", "tidybayes", "insight"))
```

```r
# figure 2 also uses the ggbrace package:
devtools::install_github("nicolash2/ggbrace")
```

```r
# Shiny app relies on these packages:
install.packages(c("shiny", "shinyjs", "bslib", "knitr", "kableExtra", "formattable"))
```

3. Run cripts in the /R, /supplemental-materials, and /manuscript folders
