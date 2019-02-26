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
# US petroleum balance sheet (table 1) ----
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

current.week <- pull(tail(cl_total, n = 2)[2, "date"])
prior.week <- pull(tail(cl_total, n = 2)[1, "date"])


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

tabl1 <- tibble(US.Stocks = c("Crude Oil", "Gasoline", "Distillates", "All Other Oils", "SPR", "Total"),
                current = current.stocks,
                prior = prior.stocks,
                change = current.stocks - prior.stocks)

# US crude oil supply & demand balance (table 1b) ----
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

# create status table (table1)
current.supply <- c(pull(tail(cl_production, n = 2)[2, "cl_production"]),
                    pull(tail(cl_imports, n = 2)[2, "cl_imports"]),
                    -1 * pull(tail(cl_exports, n = 2)[2, "cl_exports"]),
                    -1 * pull(tbl1[1, "change"])/7,
                    pull(tail(refinery.runs, n = 2)[2, "refinery.runs"]))

prior.stock.change <- (pull(tail(cl_commercial, n = 3)[2, "cl_commercial"]) -
     pull(tail(cl_commercial, n = 3)[1, "cl_commercial"]))/1000

prior.supply <- c(pull(tail(cl_production, n = 2)[1, "cl_production"]),
                    pull(tail(cl_imports, n = 2)[1, "cl_imports"]),
                    -1 * pull(tail(cl_exports, n = 2)[1, "cl_exports"]),
                    -1 * prior.stock.change/7,
                    pull(tail(refinery.runs, n = 2)[1, "refinery.runs"]))
current.adj <- sum(current.supply[1:4]) - current.supply[5]
prior.adj <- sum(prior.supply[1:4]) - prior.supply[5]

tbl2 <- tibble(Mass.Balance = c("Production", "Imports", "Exports", "Stock.Change",
                                "Adjustment", "Refinery.Inputs"),
               current = c(current.supply[1:4], current.adj, current.supply[5]),
               prior = c(prior.supply[1:4], prior.adj, prior.supply[5])) %>%
     mutate(change = current - prior)



# US stocks of crude by PAD district (table 4) ----
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



# 3.Graph Analysis ----
# US balances ----
temp.var <- us.total.stocks
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.color <- c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#000000")
temp.max <- ceiling(max(temp.dat[, 2]) / 5e4) * 5e4
temp.min <- floor(min(temp.dat[, 2]) / 5e4) * 5e4
temp.step <- (temp.max - temp.min) / 10
temp.subtitle <- paste0("Week ", as.character(tail(temp.dat.19, n = 1)$week), "   ",
                        tail(temp.dat.19, n = 1)$date)

p1 <- ggplot(temp.dat, aes(x = week, y = us.total.stocks, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = us.total.stocks), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Total Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_commercial
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 5e4) * 5e4
temp.min <- floor(min(temp.dat[, 2]) / 5e4) * 5e4
temp.step <- (temp.max - temp.min) / 10
p2 <- ggplot(temp.dat, aes(x = week, y = cl_commercial, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_commercial), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- rb_total
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 5e3) * 5e3
temp.min <- floor(min(temp.dat[, 2]) / 5e3) * 5e3
temp.step <- (temp.max - temp.min) / 10
p3 <- ggplot(temp.dat, aes(x = week, y = rb_total, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = rb_total), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- ho_total
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 5e3) * 5e3
temp.min <- floor(min(temp.dat[, 2]) / 5e3) * 5e3
temp.step <- (temp.max - temp.min) / 10
p4 <- ggplot(temp.dat, aes(x = week, y = ho_total, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = ho_total), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Distillate Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

home <- getwd()
setwd(image.path)
png(file = "petroleum_balances.png", width = 950, height = 900)
multiplot(p1, p2, p3, p4, layout = matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))
dev.off()
setwd(home)

# US Supply and Demand ----
temp.var <- cl_production
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p1a <- ggplot(temp.dat, aes(x = week, y = cl_production, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_production), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Production"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls) Per Day") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_imports %>%
     mutate(sma4 = SMA(cl_imports, n = 4))
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, "sma4"]) / 1e2) * 1e2
temp.min <- floor(min(temp.dat[, "sma4"]) / 1e2) * 1e2
temp.step <- (temp.max - temp.min) / 10

p1b <- ggplot(temp.dat, aes(x = week, y = sma4, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = sma4), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Imports"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls) Per Day") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_exports #%>%
     #mutate(sma4 = SMA(cl_imports, n = 4))
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e2) * 1e2
temp.min <- floor(min(temp.dat[, 2]) / 1e2) * 1e2
temp.step <- (temp.max - temp.min) / 10

p1c <- ggplot(temp.dat, aes(x = week, y = cl_exports, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_exports), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Exports"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls) Per Day") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))


temp.var <- refinery.runs
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p1d <- ggplot(temp.dat, aes(x = week, y = refinery.runs, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = refinery.runs), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Refinery Runs"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls) Per Day") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

home <- getwd()
setwd(image.path)
png(file = "cl_balances.png", width = 950, height = 900)
multiplot(p1a, p1b, p1c, p1d, layout = matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))
dev.off()
setwd(home)
