# Header ----
#
# Name:         mapWPS.R
# Title:        map and get all data in the eia Weekly Petroleum Status Report
# Version:      1.0
# Date:         2019MFeb-12
# Author:       Brad Horn
# License:      GPL (>=2)
#
# Description:  Map each element in the WPS report to data series IDs. Use the IDs
#               to get the series data from eia.  Data includes production, storage
#               stocks and spot prices.  Data is transformed for tidy work flow and
#               the creation of WPS summary tables and plots.
#
# Details:      NA
#
# Dev.Notes:    NA
#
# Depends:      See the configuration script Config_Trading.R in project config folder;
#               See the custom package eiaAPI for API interface functions. The API
#               requires a security key, which is a key function input.
#
# References:   See https://www.eia.gov/opendata/register.php for API key registration
#               See https://www.eia.gov/opendata/commands.php for API command syntax
##----------------------------------------------------------------------------------------##

# 0.Configure Workspace ----
# update path as needed
source("/home/bxhorn/Dropbox/Trading/R_Projects/eiaAPI/config/Config_Trading.R")

##----------------------------------------------------------------------------------------##

# 1.Browse API (Category Query) ----
# unfold the data hierarchy by layer

# petroleum
browse_eia(cat.ID = 714755, key = key)
# summary
browse_eia(cat.ID = 714756, key = key)
# weekly supply estimates
browse_eia(cat.ID = 235079, key = key)
# by data sereis
browse_eia(cat.ID = 235678, key = key)

# extract stock level data only and get bulk IDs
stock.ids <- browse_eia(cat.ID = 235678, key = key) %>%
     filter(substr(name, 1, 6) == "Stocks") %>%
     pull(category_id) %>%
     map_dfr(function(x) browse_eia(cat.ID = x, key = key))

# prices
browse_eia(cat.ID = 714757, key = key)
# spot prices
browse_eia(cat.ID = 241335, key = key) %>%
     filter(f == "D") %>%
     select("series_id")
##----------------------------------------------------------------------------------------##

# 2. Call API (Series Query) ----
# US petroleum balance sheet (table 1)
cl_total <- "PET.WCRSTUS1.W"
cl_commercial <- "PET.WCESTUS1.W"
cl_SPR <- "PET.WCSSTUS1.W"
rb_total <- "PET.WGTSTUS1.W"
rb_reformulated <- "PET.WGRSTUS1.W"
rb_conventional <- "PET.WG4ST_NUS_1.W"
rb_blending.components <- "PET.WBCSTUS1.W"
ethanol <- "PET.W_EPOOXE_SAE_NUS_MBBL.W"
kerosene.jetfuel <- "PET.WKJSTUS1.W"
ho_total <- "PET.WDISTUS1.W"
ho_ULSD <- "PET.WD0ST_NUS_1.W"
ho_15to500ppm <- "PET.WD1ST_NUS_1.W"
ho_hisulfur <- "PET.WDGSTUS1.W"
rfo <- "PET.WRESTUS1.W"
propane <- "PET.WPRSTUS1.W"
other.oils <- "PET.W_EPPO6_SAE_NUS_MBBL.W"
unfinished.oils <- "PET.WUOSTUS1.W"
us.total.stocks.spr <- "PET.WTTSTUS1.W"
us.total.stochs <- "PET.WTESTUS1.W"

series.ID <- c(cl_total, cl_commercial, cl_SPR,
               rb_total, rb_reformulated, rb_conventional,
               rb_blending.components, ethanol, kerosene.jetfuel,
               ho_total, ho_ULSD, ho_15to500ppm,
               ho_hisulfur, rfo, propane,
               other.oils, unfinished.oils, us.total.stocks.spr,
               us.total.stochs)
series.name <- c("cl_total", "cl_commercial", "cl_SPR",
               "rb_total", "rb_reformulated", "rb_conventional",
               "rb_blending.components", "ethanol", "kerosene.jetfuel",
               "ho_total", "ho_ULSD", "ho_15to500ppm",
               "ho_hisulfur", "rfo", "propane",
               "other.oils", "unfinished.oils", "us.total.stocks.spr",
               "us.total.stochs")
# get the data from eia and send to cache
map(series.ID, function(x) call_eia(x, key = key, cache.data = TRUE,
                                    cache.metadata = TRUE, cache.path = cache.path))
# transform the data and rename
map2(series.ID, series.name, function(x, y){
     temp.dat <- get(x) %>%
          set_colnames(c("date", y)) %>%
          mutate(year = year(date),
                 month = month(date),
                 week = week(date))
     assign(y, temp.dat, envir = .GlobalEnv)
})

# US stocks of crude by PAD district (table 4)
cl_padd1 <- "PET.WCESTP11.W"
cl_padd2 <- "PET.WCESTP21.W"
cl_cushing <- "PET.W_EPC0_SAX_YCUOK_MBBL.W"
cl_padd3 <- "PET.WCESTP31.W"
cl_padd4 <- "PET.WCESTP41.W"
cl_padd5 <- "PET.WCESTP51.W"
# package eia IDs
series.ID <- c(cl_padd1, cl_padd2, cl_cushing, cl_padd3, cl_padd4, cl_padd5)
series.name <- c("cl_padd1", "cl_padd2", "cl_cushing", "cl_padd3",
               "cl_padd4", "cl_padd5")
# get the data from eia and send to cache
map(series.ID, function(x) call_eia(x, key = key, cache.data = TRUE,
                                    cache.metadata = TRUE, cache.path = cache.path))
# transform the data and rename
map2(series.ID, series.name, function(x, y){
     temp.dat <- get(x) %>%
          set_colnames(c("date", y)) %>%
          mutate(year = year(date),
                 month = month(date),
                 week = week(date))
     assign(y, temp.dat, envir = .GlobalEnv)
})

# US stocks of motor gasoline (table 5)
rb_padd1 <- "PET.WGTSTP11.W"
rb_padd2 <- "PET.WGTSTP21.W"
rb_padd3 <- "PET.WGTSTP31.W"
rb_padd4 <- "PET.WGTSTP41.W"
rb_padd5 <- "PET.WGTSTP51.W"
# package eia IDs
series.ID <- c(rb_padd1, rb_padd2, rb_padd3, rb_padd4, rb_padd5)
series.name <- c("rb_padd1", "rb_padd2", "rb_padd3", "rb_padd4", "rb_padd5")
# get the data from eia and send to cache
map(series.ID, function(x) call_eia(x, key = key, cache.data = TRUE,
                                    cache.metadata = TRUE, cache.path = cache.path))
# transform the data and rename
map2(series.ID, series.name, function(x, y){
     temp.dat <- get(x) %>%
          set_colnames(c("date", y)) %>%
          mutate(year = year(date),
                 month = month(date),
                 week = week(date))
     assign(y, temp.dat, envir = .GlobalEnv)
})


# US stocks of distillates (table 6)
ho_padd1 <- "PET.WDISTP11.W"
ho_padd2 <- "PET.WDISTP21.W"
ho_padd3 <- "PET.WDISTP31.W"
ho_padd4 <- "PET.WDISTP41.W"
ho_padd5 <- "PET.WDISTP51.W"
# package eia IDs
series.ID <- c(ho_padd1, ho_padd2, ho_cushing, ho_padd3, ho_padd4, ho_padd5)
series.name <- c("ho_padd1", "ho_padd2", "ho_padd3", "ho_padd4", "ho_padd5")
# get the data from eia and send to cache
map(series.ID, function(x) call_eia(x, key = key, cache.data = TRUE,
                                    cache.metadata = TRUE, cache.path = cache.path))
# transform the data and rename
map2(series.ID, series.name, function(x, y){
     temp.dat <- get(x) %>%
          set_colnames(c("date", y)) %>%
          mutate(year = year(date),
                 month = month(date),
                 week = week(date))
     assign(y, temp.dat, envir = .GlobalEnv)
})

# US imports of crude oil and products (table 7)
cl_imports.net <- "PET.WCRNTUS2.W"
cl_imports <- "PET.WCRIMUS2.W"
cl_exports <- "PET.WCREXUS2.W"
rb_imports <- "PET.WGTIMUS2.W"					     # total motor gasoline
rb_exports <- "PET.W_EPM0F_EEX_NUS-Z00_MBBLD.W"             # finished gasoline
ho_imports <- "PET.WDIIMUS2.W"
ho_exports <- "PET.WDIEXUS2.W"
# package eia IDs
series.ID <- c(cl_imports.net, cl_imports, cl_exports, rb_imports, rb_exports,
               ho_imports, ho_exports)
series.name <- c("cl_imports.net", "cl_imports", "cl_exports", "rb_imports",
                 "rb_exports", "ho_imports", "ho_exports")
# get the data from eia and send to cache
map(series.ID, function(x) call_eia(x, key = key, cache.data = TRUE,
                                    cache.metadata = TRUE, cache.path = cache.path))
# transform the data and rename
map2(series.ID, series.name, function(x, y){
     temp.dat <- get(x) %>%
          set_colnames(c("date", y)) %>%
          mutate(year = year(date),
                 month = month(date),
                 week = week(date))
     assign(y, temp.dat, envir = .GlobalEnv)
})

# US production metrics (table 9)
cl_production <- "PET.WCRFPUS2.W"
refinery.runs <- "PET.WCRRIUS2.W"
refinery.util <- "PET.WPULEUS3.W"
# package eia IDs
series.ID <- c(cl_production, refinery.runs, refinery.util)
series.name <- c("cl_production", "refinery.runs", "refinery.util")
# get the data from eia and send to cache
map(series.ID, function(x) call_eia(x, key = key, cache.data = TRUE,                                  cache.metadata = TRUE, cache.path = cache.path))
# transform the data and rename
map2(series.ID, series.name, function(x, y){
     temp.dat <- get(x) %>%
          set_colnames(c("date", y)) %>%
          mutate(year = year(date),
                 month = month(date),
                 week = week(date))
     assign(y, temp.dat, envir = .GlobalEnv)
})

# US spot prices (table 11)
ho_spot.USG <- "PET.EER_EPD2DXL0_PF4_RGC_DPG.D"
ho_spot.NYH <- "PET.EER_EPD2DXL0_PF4_Y35NY_DPG.D"
no2_spot.NYH <- "PET.EER_EPD2F_PF4_Y35NY_DPG.D"
rb_spot.USG <- "PET.EER_EPMRU_PF4_RGC_DPG.D"
rb_spot.NYH <- "PET.EER_EPMRU_PF4_Y35NY_DPG.D"
cl_brent_spot <- "PET.RBRTE.D"
cl_wti_spot <- "PET.RWTC.D"
# package eia IDs
series.ID <- c(ho_spot.USG, ho_spot.NYH, no2_spot.NYH, rb_spot.USG, rb_spot.NYH,
               cl_brent_spot, cl_wti_spot)
series.name <- c("ho_spot.USG", "ho_spot.NYH", "no2_spot.NYH", "rb_spot.USG",
                 "rb_spot.NYH", "cl_brent_spot", "cl_wti_spot")
# get the data from eia and send to cache
map(series.ID, function(x) call_eia(x, key = key, cache.data = TRUE,
                                    cache.metadata = TRUE, cache.path = cache.path))
# transform the data and rename
map2(series.ID, series.name, function(x, y){
     temp.dat <- get(x) %>%
          set_colnames(c("date", y)) %>%
          mutate(year = year(date),
                 month = month(date),
                 week = week(date))
     assign(y, temp.dat, envir = .GlobalEnv)
})

# Junk ----
