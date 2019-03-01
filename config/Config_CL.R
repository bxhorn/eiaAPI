# Header ----
#
# Name:        Config_CL.R
#
# Title:       Configure project workspace CL
#
# Version:     1.0
# Date:        2019-Jan-25
# Author:      Brad Horn
# License:     GPL (>=2)
#
# Description: Script for CL project configuration (Crude Oil Trading)
#
# Details:     The script tasks:
#
#                   - load required libraries,
#                   - sets project file paths
#                   - run custom source functions,
#                   - set workspace options,
#                   - configure devices and
#                   - load API authentication keys.
#
#              The configuration supports collaboration and repeatable results across
#              machines and over time.
#
# Dev.Notes:   Sourced the config file at the start of all project sessions.  API
#              authentication keys are user specific and need to be updated for each
#              project collaborator.
#
#              The config file opens with two lines to remove all data objects from
#              memory at the start of any session so all data is controled by source
#              code.  These lines can be taggled on/off using commenting.  The intent
#              is to ensure all data is source code generated and not created manually.
#
# Depends      R programming language (3.5) and all listed base or third-party R
#              packages.  Custom functions, if any, can be found in the project lib
#              directory.
#
# References:  see package description files and vignettes as needed.
##-------------------------------------------------------------------------------------------##

# 0.Memory Management ----
# clear all environment data, but not function objects, and reset memory
#rm(list = setdiff(ls(), lsf.str()))
#gc(reset = TRUE)

# 1.Libraries ----
# define package list
pkgs <- c(

                                      # TIDYVERSE
     "tidyquant",                       # package of packages for financial analysis
     "pillar",                          # tibble formats
                                      # DATA I/O
     "openxlsx",                        # interface for MS Excel
     "Quandl",                          # API wrapper for Quandl.com
     "RCurl",                           # communicate with HTTP servers for data I/O
     "rvest",                           # easily harvest (scrape) web pages
     "RJSONIO",                         # read/write JSON data (JavaScript Object Notation)
                                      # PROGRAMMING
     "magrittr",                        # pipe operators
     "purrr",                           # functional programming tools
     "purrrlyr",                        # intersection of purrr and dplyr
                                      # DATA MODELING
     "broom",                           # convert model output to class tidy format tbl_df
     "modelr",                           # model functions that work with a pipe
     "forecast",                        # forecasting for timeseries and linear models
     "nlme",                            # linear and non-linear mixed effect models
     "e1071",                           # ssupport vector machines
                                      # DATA TABLES AND REPORITNG
     "kableExtra",                      # export code for data tables in LaTex or HTML
     "knitr",                           # dynamic report generation in R
     "rmarkdown",                       # dyanmic docuemnts in R
                                      # DATA VISUALIZATION
     "grid",                            # low level  graphics system
     "gridExtra",                       # misc graphics functions for base R
     "ggthemes",                        # plot themese for ggplot
     "GGally",                          # extension of ggplot
     "scales",                          # scale and color functions for ggplot
     "bdscale",                         # remove weekends/holidays from ggplot axes
     "RColorBrewer",                    # color ramps
                                      # NETWORK VISUALIZATION
     "ape",                             # draw and analyze phylogenetic trees
     "phytools",                        # phylogenetic toolset
     "ggtree"                           # draw and annotate phylogentetic trees
)

# load packages
for (i in pkgs) {
      suppressMessages(library(i, character.only = TRUE))
}
rm(i)

# resolve package conflicts
collapse <- dplyr::collapse
combine <- dplyr::combine
complete <- tidyr::complete
extract <- tidyr::extract
expand <- tidyr::expand
discard <- purrr::discard
map <- purrr::map
pluck <- purrr::pluck
set_names <- purrr::set_names
getResponse <- forecast::getResponse
kurtosis <- PerformanceAnalytics::kurtosis
skewness <- PerformanceAnalytics::skewness
##-------------------------------------------------------------------------------------------##

# 2.Project Paths ----
cache.path <- file.path(getwd(), "cache/")
config.path <- file.path(getwd(), "config/")
data.path <- file.path(getwd(), "data/")
image.path <- file.path(getwd(), "image/")
lib.path <- file.path(getwd(), "lib/")
report.path <- file.path(getwd(), "report/")
source.path <- file.path(getwd(), "src/")
#archive <- "/mnt/timecapsule/BXH/"
##-------------------------------------------------------------------------------------------##

# 3.Configure Project Workspace ----
# options
options(prompt = "R> ")
options(width = 95)
options(digits = 5, scipen = 4)
options(tibble.print_max = 150)
options(tibble.print_min = 125)
options(tibble.width = NULL)
options(pillar.bold = TRUE)
options(pillar.subtle = FALSE)
options(pillar.sigfig = 4)
options(stringsAsFactors = FALSE)
options(papersize = "a4")
options(repos = "https://cran.rstudio.com")
Sys.setenv(TZ = "Asia/Qatar")
Sys.setenv(R_HISTSIZE = '100000')
##-------------------------------------------------------------------------------------------##

# 4.Configure Graphic Devices ----
# store basic plot parameters (op = old.parameters)
op <- par(no.readonly = TRUE)
# customize plot parameters for mapping
par(mai = c(1.02, 0.82, 0.82, 1.02), mar = c(5.1, 4.1, 4.1, 5.1))
##-------------------------------------------------------------------------------------------##

# 5.Load Security Keys ----
# Autheticate Quandl data access
Quandl.api_key('iHoiX3aUUU7sxn25xSwD')
# EIA data
key <- "702dd5c99ca504ffa0c10e479e34f234"
##-------------------------------------------------------------------------------------------##

# 6.Source Custom Functions ----
source(paste0(lib.path, "get_futures.R"))
source(paste0(lib.path, "get_term_structure.R"))
source(paste0(lib.path, "eiaAPI.R"))
source(paste0(lib.path, "plot_theme.R"))
source(paste0(lib.path, "multiplot.R"))
