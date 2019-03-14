# Header ----
#
# Name:         WPS_go.R
# Title:        Generate eia Weekly Petroleum Status Report
# Version:      1.0
# Date:         2019-Mar-14
# Author:       Brad Horn
# License:      GPL (>=2)
#
# Description:  Wrapper script to batch process the generation of a custom report
#               using data from the eia Weekly Petroleum Status Report.  The script
#               sources files to configure the workspace, to batch process eia data
#               retrieval, to generate data plots and to render an R Mardown file in
#               HTML format with a custom file name.
#
# Details:      Manuualy update the YAML header in the R Markdown file with eia report
#               date and week number
#
# Dev.Notes:    NA
#
# Depends:      See the configuration script Config_CL.R in project config folder;
#               See source file WPS_map.R for core data inputs and tables.
#               See source file WPS_plot.R for data visualizations.
#               See R Markdown file WPS_report.Rmd for HTML file genenration
#               See the custom functions in eiaAPI.R for API interface functions.
#               API use requires a security key obtained through eia registration.
#
# References:   See https://www.eia.gov/opendata/register.php for API key registration
#               See https://www.eia.gov/opendata/commands.php for API command syntax
##----------------------------------------------------------------------------------------##

# 0.Configure Workspace ----
source("/home/bxhorn/Dropbox/Trading/R_Projects/eiaAPI/config/Config_CL.R")

# confirm latest data available
call_eia("PET.WTESTUS1.W", key = key)
(test <- get("PET.WTESTUS1.W") %>%
     tail(., n = 1) %>%
     select(Date) %>%
     mutate(Week = week(Date)))
##----------------------------------------------------------------------------------------##

# 1.Get Data and Create Plots
source("/home/bxhorn/Dropbox/Trading/R_Projects/eiaAPI/src/WPS_map.R")
source("/home/bxhorn/Dropbox/Trading/R_Projects/eiaAPI/src/WPS_plot.R")
##----------------------------------------------------------------------------------------##

# 2. Render HTML Report
temp.file <- paste0("WPS.", gsub("-","", pull(test[1,1])), ".html")
rmarkdown::render('WPS_report.Rmd', output_file = temp.file)
##----------------------------------------------------------------------------------------##
