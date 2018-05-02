To run through the following workflow you'll need a few things.

#### R or RStudio
You will need a copy of R to run through the project. R can be downloaded [here](https://cran.r-project.org/mirrors.html).  RStudio is an interactive development environment (IDE) created for developing in R and is highly recommended. RStudio can be downloaded [here](https://www.rstudio.com/products/rstudio/download/). If you need a basic of overview of R there are a couple free online tutorials that are recommended:

**Data Camp** - [Introduction to R](https://www.datacamp.com/courses/free-introduction-to-r)

**Coursera** - [R Programming](https://www.coursera.org/learn/r-programming)

#### PReMiuM R Package
The PReMiuM package is available to download directly through CRAN as a stable version with the `install.packages("PReMiuM")` command or with developmental features (may be less stable) from the GitHub repo with the `devtools::install_github("silvialiverani/PReMiuM-R-package/PReMiuM")` command.  You might need to `install.packages("devtools")` for this to work.

#### Tidyverse Suite of R Packages
Using the **tidyverse** is not a requirement, but for data manipulation they provide standards that can be used across any type of R project and Hadley Wickham (the tidyverse core contributor) diligently researches best software development practices. Install via `install.packages("tidyverse")` and check out the following free ebook by Hadley Wickham here [_R for Data Science_](http://r4ds.had.co.nz/).  Running the `library(tidyverse)` command will load several packages used commonly like **tidyr**, **dplyr**, **readr**, and **purrr**.

#### Assumptions
All of the files in this workflow can be found with the repo for EnviroTyping on [GitHub](https://github.com/TACC/EnviroTyping).  Directory paths have been assigned either relative to the file within the repo such as retrieving an Rds object dataset `toy.data <- read_rds("../../toy_data.rds")` where its assumed the `setwd("~/GitHub/EnviroTyping/G2F_Hybrid/small_data/")` has been used.  The path in the _setwd_ function is a standard Unix home folder (i.e. Mac) with a GitHub directory which has the EnviroTyping repository cloned within it.  You may need to change your path to reflect the location you have cloned the EnviroTyping repo on your local or remote machine.

#### Using HPC
Often the analysis will to complex for a regular laptop or even powerful desktop to run in a reaonsable period of time.  In this situation you may want to use a high performance computer.  Check out the [Getting Started with High Performance Computing](/../tutorials/hpc.md) guide for information on how you can create a free account to start using the Stampede2 cluster at the Texas Advanced Computing Center ([TACC](https://portal.tacc.utexas.edu/user-guides/stampede2)).
