# Reproducibility in small-N treatment research: a tutorial using examples from aphasiology

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
rob.cavanaugh [@] pitt.edu

Note: Shiny app is located here: https://rb-cavanaugh.shinyapps.io/reproducibile-ES/

## Directory organization

This directory follows the TIER 4.0 Guidelines, as applicable (https://www.projecttier.org/tier-protocol/protocol-4-0/)

- /manuscript contains the Quarto file with reproducible manuscript
- /scripts contains master script to run all scripts along with a script 
with effect size functions and two Rmarkdown files for S1 and S2. 
- /data contains data used in the tutorial
- /output contains saved {brms} model objects (note: large files)
- /shiny contains code for shiny app

DESCRIPTION file documents packages used
LICENSE documents CC-BY license for the repository

## Running the code

These materials use Rprojects and the {here} package to manage file paths. More
information is located here: https://www.tidyverse.org/articles/2017/12/workflow-vs-script/

1. Begin by opening the reproducibility-aphasia-JSLHR.Rproj file in RStudio. 

- You can also create a free account at https://rstudio.cloud to minimize the need
to install R/Rstudio. 
- you can download the repository from .osf or github. Alternatively, in RStudio, 
select file --> new project --> version control --> git, and then paste the
github link: https://github.com/rbcavanaugh/reproducibility-aphasia-JSLHR.git.
- Note that you will need the latest version of RStudio to render the Quarto manuscript,
or you will need to install Quarto separately. Quarto renders documents in RStudio
very similar to RMarkdown (https://quarto.org).

2. You will need to install the packages used in the manuscript. 

The repository includes a renv.lock file to allow anyone to reproduce the 
packages used in the study. Details on renv can be found here: https://rstudio.github.io/renv/articles/renv.html. 

After opening the R project, you can install all of the same package versions
that were used in the project by running `renv::restore()` in the R console. 

If you prefer to simply install the newest versions of the packages used,
run `renv::init()` instead. 

If you prefer to install the packages without renv, run

```r
renv::deactivate()
install.packages("devtools")
devtools::install_deps()
```

Note that some packages used for the shiny app are not included by default. They
can be installed by running the following code in the R console.

```r
# Shiny app relies on these additional packages
install.packages(c("shiny", "shinyjs", "bslib", "knitr", "kableExtra", "formattable"))
```

3. Run scripts 

- Run each line in scripts/master_script.R to re-run all scripts included in
the project. The supplemental material scripts can also be run independently. 
The manuscript relies on the supplemental scripts to be run first. 
