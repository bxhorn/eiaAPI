# Header ----
#
# Name:         mapWPS.R
# Title:        Map all data elements in the eia Weekly Petroleum Status Report
# Version:      1.0
# Date:         2019-Feb-12
# Author:       Brad Horn
# License:      GPL (>=2)
#
# Description:  Map all data series IDs for each element in the WPS report. Use the
#               map to re-create the weekly report using automated API calls to eia.
#               Downloaded production, storage and price data is then subject to tidy
#               work handling, table and plot creation.
#
# Details:      NA
#
# Dev.Notes:    NA
#
# Depends:      See the configuration script Config_Trading.R in project config folder;
#               See the custom package eiaAPI for eia API interface functions. An eia
#               API security key is required to use the functions.
#
# References:   See https://www.eia.gov/opendata/register.php for API key registration
#               See https://www.eia.gov/opendata/commands.php for API command syntax
##----------------------------------------------------------------------------------------##

# 0.Configure Workspace ----
# update path as needed
source("/home/bxhorn/Dropbox/Trading/R_Projects/NearbyPrices/config/Config_Trading.R")

##----------------------------------------------------------------------------------------##

# 1.Browse API (Category Query) ----
# unfold the data hierarchy by layer

# petroleum
browse.EIA(cat.ID = 714755, key = key)
# summary
browse.EIA(cat.ID = 714756, key = key)
# weekly supply estimates
browse.EIA(cat.ID = 235079, key = key)
# by data sereis
browse.EIA(cat.ID = 235678, key = key)

# extract stock level data only and get bulk IDs
stock.ids <- browse.EIA(cat.ID = 235678, key = key) %>%
     filter(substr(name, 1, 6) == "Stocks") %>%
     pull(category_id) %>%
     map_dfr(function(x) browse.EIA(cat.ID = x, key = key))

# prices
browse.EIA(cat.ID = 714757, key = key)
# spot prices
browse.EIA(cat.ID = 241335, key = key) %>%
     filter(f == "D") %>%
     select("series_id")
##----------------------------------------------------------------------------------------##

# 2. Call API (Series Query) ----





# Junk ----
