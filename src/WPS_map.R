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
#               the creation of WPS summary tables.
#
# Details:      NA
#
# Dev.Notes:    NA
#
# Depends:      See the configuration script Config_CL.R in project config folder;
#               See source file eiaWPS_plot.R for data plots and visualization.
#               See the custom functions eiaAPI for API interface functions. API use
#               requires a security key, which is a key function input.
#
# References:   See https://www.eia.gov/opendata/register.php for API key registration
#               See https://www.eia.gov/opendata/commands.php for API command syntax
##----------------------------------------------------------------------------------------##

# 0.Configure Workspace ----
# update path as needed
# source("/home/bxhorn/Dropbox/Trading/R_Projects/eiaAPI/config/Config_CL.R")

##----------------------------------------------------------------------------------------##

# 1.Browse API (Category Query) ----
# manually unfold the eia data hierarchy by layer

# petroleum
# browse_eia(cat.ID = 714755, key = key)
# summary
# browse_eia(cat.ID = 714756, key = key)

# weekly supply estimates
# browse_eia(cat.ID = 235079, key = key)
# by data series
# browse_eia(cat.ID = 235678, key = key)
# extract stock level data only and get bulk IDs
# stock.ids <- browse_eia(cat.ID = 235678, key = key) %>%
#      filter(substr(name, 1, 6) == "Stocks") %>%
#      pull(category_id) %>%
#      map_dfr(function(x) browse_eia(cat.ID = x, key = key))

# prices
# browse_eia(cat.ID = 714757, key = key)
# spot prices
# browse_eia(cat.ID = 241335, key = key) %>%
#      filter(f == "D") %>%
#      select("updated")
##---------------------------------------------------------------------------------------##

# 2. Call API (Series Query) ----
# US Petroleum Balance Sheet (table 1) ----
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
us.total.stocks <- "PET.WTESTUS1.W"

series.ID <- c(cl_total, cl_commercial, cl_SPR,
               rb_total, rb_reformulated, rb_conventional,
               rb_blending.components, ethanol, kerosene.jetfuel,
               ho_total, ho_ULSD, ho_15to500ppm,
               ho_hisulfur, rfo, propane,
               other.oils, unfinished.oils, us.total.stocks.spr,
               us.total.stocks)
series.name <- c("cl_total", "cl_commercial", "cl_SPR",
               "rb_total", "rb_reformulated", "rb_conventional",
               "rb_blending.components", "ethanol", "kerosene.jetfuel",
               "ho_total", "ho_ULSD", "ho_15to500ppm",
               "ho_hisulfur", "rfo", "propane",
               "other.oils", "unfinished.oils", "us.total.stocks.spr",
               "us.total.stocks")
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


# define WPS report issue
current.week <- pull(tail(cl_total, n = 2)[2, "date"])
prior.week <- pull(tail(cl_total, n = 2)[1, "date"])

# create status table1
current.other <- pull(tail(ethanol, n = 2)[2, "ethanol"]) +
     pull(tail(kerosene.jetfuel, n = 2)[2, "kerosene.jetfuel"]) +
     pull(tail(rfo, n = 2)[2, "rfo"]) +
     pull(tail(propane, n = 2)[2, "propane"]) +
     pull(tail(other.oils, n = 2)[2, "other.oils"])
prior.other <- pull(tail(ethanol, n = 2)[1, "ethanol"]) +
     pull(tail(kerosene.jetfuel, n = 2)[1, "kerosene.jetfuel"]) +
     pull(tail(rfo, n = 2)[1, "rfo"]) +
     pull(tail(propane, n = 2)[1, "propane"]) +
     pull(tail(other.oils, n = 2)[1, "other.oils"])

current.stocks <- c(pull(tail(cl_commercial, n = 2)[2, "cl_commercial"]),
                   pull(tail(rb_total, n = 2)[2, "rb_total"]),
                   pull(tail(ho_total, n = 2)[2, "ho_total"]),
                   current.other,
                   pull(tail(cl_SPR, n = 2)[2, "cl_SPR"]),
                   pull(tail(us.total.stocks.spr, n = 2)[2, "us.total.stocks.spr"]))/1000
prior.stocks <- c(pull(tail(cl_commercial, n = 2)[1, "cl_commercial"]),
                 pull(tail(rb_total, n = 2)[1, "rb_total"]),
                 pull(tail(ho_total, n = 2)[1, "ho_total"]),
                 prior.other,
                 pull(tail(cl_SPR, n = 2)[1, "cl_SPR"]),
                 pull(tail(us.total.stocks.spr, n = 2)[1, "us.total.stocks.spr"]))/1000

tbl1 <- tibble(US.Stocks = c("Crude Oil", "Gasoline", "Distillates", "All Other Oils", "SPR", "Total"),
                current = current.stocks,
                prior = prior.stocks,
                change = current.stocks - prior.stocks)

# distribution analysis of key changes
total_delta <- us.total.stocks.spr %>%
     filter(year >= 1990) %>%
     mutate(delta = us.total.stocks.spr - Lag(us.total.stocks.spr, k = 1))
cl_delta <- cl_commercial %>%
     filter(year >= 1990) %>%
     mutate(delta = cl_commercial - Lag(cl_commercial, k = 1))
##---------------------------------------------------------------------------------------##

# US Crude Oil Supply & Demand Balance (table 1b) ----
cl_production <- "PET.WCRFPUS2.W"
refinery.runs <- "PET.WCRRIUS2.W"
cl_imports <- "PET.WCRIMUS2.W"
cl_exports <- "PET.WCREXUS2.W"

series.ID <- c(cl_production, refinery.runs, cl_imports, cl_exports)
series.name <- c("cl_production", "refinery.runs", "cl_imports", "cl_exports")
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

# create status table table1b
current.supply <- c(pull(tail(cl_production, n = 2)[2, "cl_production"]),
                    pull(tail(cl_imports, n = 2)[2, "cl_imports"]),
                    -1 * pull(tail(cl_exports, n = 2)[2, "cl_exports"]),
                    -1 * pull(tbl1[1, "change"] * 1000)/7,
                    pull(tail(refinery.runs, n = 2)[2, "refinery.runs"]))

prior.stock.change <- (pull(tail(cl_commercial, n = 3)[2, "cl_commercial"]) -
     pull(tail(cl_commercial, n = 3)[1, "cl_commercial"]))

prior.supply <- c(pull(tail(cl_production, n = 2)[1, "cl_production"]),
                    pull(tail(cl_imports, n = 2)[1, "cl_imports"]),
                    -1 * pull(tail(cl_exports, n = 2)[1, "cl_exports"]),
                    -1 * prior.stock.change /7,
                    pull(tail(refinery.runs, n = 2)[1, "refinery.runs"]))
current.adj <- sum(current.supply[1:4]) - current.supply[5]
prior.adj <- sum(prior.supply[1:4]) - prior.supply[5]

tbl2 <- tibble(Mass.Balance = c("Production", "Imports", "Exports", "Stock.Change",
                                "Adjustment", "Refinery.Inputs"),
               current = c(current.supply[1:4], current.adj, current.supply[5]),
               prior = c(prior.supply[1:4], prior.adj, prior.supply[5])) %>%
     mutate(change = current - prior)
##---------------------------------------------------------------------------------------##

# US Stocks of Crude Oil by PADD (table 4) ----
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

# create status table4
current.cl_stocks <- c(pull(tail(cl_cushing, n = 2)[2, "cl_cushing"]),
                       pull(tail(cl_padd1, n = 2)[2, "cl_padd1"]),
                       pull(tail(cl_padd2, n = 2)[2, "cl_padd2"]),
                       pull(tail(cl_padd3, n = 2)[2, "cl_padd3"]),
                       pull(tail(cl_padd4, n = 2)[2, "cl_padd4"]),
                       pull(tail(cl_padd5, n = 2)[2, "cl_padd5"]),
                       pull(tail(cl_total, n = 2)[2, "cl_total"]))
prior.cl_stocks <- c(pull(tail(cl_cushing, n = 2)[1, "cl_cushing"]),
                     pull(tail(cl_padd1, n = 2)[1, "cl_padd1"]),
                     pull(tail(cl_padd2, n = 2)[1, "cl_padd2"]),
                     pull(tail(cl_padd3, n = 2)[1, "cl_padd3"]),
                     pull(tail(cl_padd4, n = 2)[1, "cl_padd4"]),
                     pull(tail(cl_padd5, n = 2)[1, "cl_padd5"]),
                     pull(tail(cl_total, n = 2)[1, "cl_total"]))
tbl4 <- tibble(Crude.Stocks = c("Cushing", "PADD1", "PADD2", "PADD3",
                                "PADD4", "PADD5", "Total"),
               current = current.cl_stocks,
               prior = prior.cl_stocks) %>%
     mutate(change = current - prior)
##---------------------------------------------------------------------------------------##

# US Stocks of Motor Gasoline by PADD (table 5) ----
rb_padd1 <- "PET.WGTSTP11.W"
rb_padd1b <- "PET.WGTST1B1.W"
rb_padd2 <- "PET.WGTSTP21.W"
rb_padd3 <- "PET.WGTSTP31.W"
rb_padd4 <- "PET.WGTSTP41.W"
rb_padd5 <- "PET.WGTSTP51.W"
# package eia IDs
series.ID <- c(rb_padd1, rb_padd1b, rb_padd2, rb_padd3, rb_padd4, rb_padd5)
series.name <- c("rb_padd1", "rb_padd1b", "rb_padd2", "rb_padd3", "rb_padd4", "rb_padd5")
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

# create status table5
current.rb_stocks <- c(pull(tail(rb_padd1, n = 2)[2, "rb_padd1"]),
                       pull(tail(rb_padd1b, n = 2)[2, "rb_padd1b"]),
                       pull(tail(rb_padd2, n = 2)[2, "rb_padd2"]),
                       pull(tail(rb_padd3, n = 2)[2, "rb_padd3"]),
                       pull(tail(rb_padd4, n = 2)[2, "rb_padd4"]),
                       pull(tail(rb_padd5, n = 2)[2, "rb_padd5"]),
                       pull(tail(rb_total, n = 2)[2, "rb_total"]))
prior.rb_stocks <- c(pull(tail(rb_padd1, n = 2)[1, "rb_padd1"]),
                     pull(tail(rb_padd1b, n = 2)[1, "rb_padd1b"]),
                     pull(tail(rb_padd2, n = 2)[1, "rb_padd2"]),
                     pull(tail(rb_padd3, n = 2)[1, "rb_padd3"]),
                     pull(tail(rb_padd4, n = 2)[1, "rb_padd4"]),
                     pull(tail(rb_padd5, n = 2)[1, "rb_padd5"]),
                     pull(tail(rb_total, n = 2)[1, "rb_total"]))
tbl5 <- tibble(Gasoline.Stocks = c("PADD1", "PADD1b", "PADD2", "PADD3",
                                "PADD4", "PADD5", "Total"),
               current = current.rb_stocks,
               prior = prior.rb_stocks) %>%
     mutate(change = current - prior)
##---------------------------------------------------------------------------------------##

# US Stocks of Distillates (table 6) -----
ho_padd1 <- "PET.WDISTP11.W"
ho_padd1b <- "PET.WDIST1B1.W"
ho_padd2 <- "PET.WDISTP21.W"
ho_padd3 <- "PET.WDISTP31.W"
ho_padd4 <- "PET.WDISTP41.W"
ho_padd5 <- "PET.WDISTP51.W"
# package eia IDs
series.ID <- c(ho_padd1, ho_padd1b, ho_padd2, ho_padd3, ho_padd4, ho_padd5)
series.name <- c("ho_padd1", "ho_padd1b", "ho_padd2", "ho_padd3", "ho_padd4", "ho_padd5")
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


# create status table5
current.ho_stocks <- c(pull(tail(ho_padd1, n = 2)[2, "ho_padd1"]),
                       pull(tail(ho_padd1b, n = 2)[2, "ho_padd1b"]),
                       pull(tail(ho_padd2, n = 2)[2, "ho_padd2"]),
                       pull(tail(ho_padd3, n = 2)[2, "ho_padd3"]),
                       pull(tail(ho_padd4, n = 2)[2, "ho_padd4"]),
                       pull(tail(ho_padd5, n = 2)[2, "ho_padd5"]),
                       pull(tail(ho_total, n = 2)[2, "ho_total"]))
prior.ho_stocks <- c(pull(tail(ho_padd1, n = 2)[1, "ho_padd1"]),
                     pull(tail(ho_padd1b, n = 2)[1, "ho_padd1b"]),
                     pull(tail(ho_padd2, n = 2)[1, "ho_padd2"]),
                     pull(tail(ho_padd3, n = 2)[1, "ho_padd3"]),
                     pull(tail(ho_padd4, n = 2)[1, "ho_padd4"]),
                     pull(tail(ho_padd5, n = 2)[1, "ho_padd5"]),
                     pull(tail(ho_total, n = 2)[1, "ho_total"]))
tbl6 <- tibble(Distillate.Stocks = c("PADD1", "PADD1b", "PADD2", "PADD3",
                                "PADD4", "PADD5", "Total"),
               current = current.ho_stocks,
               prior = prior.ho_stocks) %>%
     mutate(change = current - prior)
##---------------------------------------------------------------------------------------##

# US imports of crude oil and products (table 7) -----
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
##---------------------------------------------------------------------------------------##

# US production metrics (table 9) -----
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
##---------------------------------------------------------------------------------------##

# US spot prices (table 11) ----
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
##---------------------------------------------------------------------------------------##

