To run through the following workflow you'll need a few things.

#### R or RStudio
A copy of R or RStudio with basic programming knowledge.

#### PReMiuM R Package
The PReMiuM package is available to download directly through CRAN as a stable version with the `install.packages("PReMiuM")` command or with developmental features (may be less stable) from the GitHub repo with the `devtools::install_github("silvialiverani/PReMiuM-R-package/PReMiuM")` command.

#### Tidyverse Suite of R Packages
Using the **tidyverse** is not a requirement, but for data manipulation they provide standards that can be used across any type of R project and Hadley Wickham (the tidyverse core contributor) diligently researches best software development practices. Install via `install.packages("tidyverse")` and check out the following free ebook by Hadley Wickham here [_R for Data Science_](http://r4ds.had.co.nz/).

#### Assumptions
All of the files in this workflow can be found with the repo for EnviroTyping on [GitHub](https://github.com/TACC/EnviroTyping).  Directory paths have been assigned either relative to the file within the repo such as retrieving an Rds object dataset `toy.data <- read_rds("../../toy_data.rds")` where its assumed the `setwd("~/GitHub/EnviroTyping/sandbox/interim_datasets/")` has been used.  The path in the _setwd_ function is a standard Unix home folder (i.e. Mac) with a GitHub directory which has the EnviroTyping repository cloned within it.  You may need to change your path to reflect the location you have cloned the EnviroTyping repo on your local or remote machine.
