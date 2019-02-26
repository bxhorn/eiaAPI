# Header ----
#
# Name:         get_term_structure.R
# Title:        Download futures prices and create historical term structure data
# Version:      1.0
# Date:         2019-Jan-25
# Author:       Brad Horn
# License:      GPL (>=2)
#
# Description:  Create historical term structure data using futures settlement prices
#               by contract month.
#
# Details:      The function downloads all futures contracts given a user specified
#               range of delivery months (both retrospective and forward looking).
#               Time series are created for continuous futures prices.  The user
#               specifies the number of continuous futures price series using the
#               input variable nterm.
#
# Value:        Function output includes:
#
#                                  (1) xx_NBY.settle: a single data object with all
#                                  settlement prices by maturity, where "xx" is a
#                                  commodity code,
#
#                                  (2) xx_NBY1, xx_NBY2, etc: multiple objects with
#                                  OHLS data (open-high-low-settle, volume and OI) by
#                                  maturity and commodity
#
#                                  (3) xx_expiry: expiration dates for all retrospective
#                                  and forward looing futures contracts. by commodity
#
#               All data outputs are saved in R binary format (in cache directory)
#               and in MS Excel format (in data directory)
#
# Inputs:       Function input arguments include:
#
#                   mkt.code       Character string.  Examples include "CME", "ICE"
#                                  and others.  See reference link below for other
#                                  exchange codes used by Quandl.
#
#                   fut.code       Character string.  Examples include "CL" and "HU"
#                                  among others. See reference link below for other
#                                  commodity futures codes used by Quandl.
#
#                   fut.name       Character string.  Full name of futures market.
#                                  Used in plot tiltes.  Examples include "WTI Crude
#                                  Oil" and "NYMEX Natural Gas Futures"s.
#
#                   fut.start.mo   ISO date object. Created via ymd() function. For
#                   fut.end.mo     example ymd("1994-01-01", tz = "America/New_York").
#                                  Defines the delivery months of the first and last
#                                  futures contracts to download.
#
#                   data.start     ISO date object formated with ymd() function.
#                                  Default = ymd("1900-01-01", tz = "America/New_York")
#
#                   nterm          Integer.  Number of forward delivery months in the
#                                  forward curve to be created.  Default nterm = 36.
#                                  Applies to settlement prices only (not OHLS).
#
#                   OHLS           Interger. Number of nearby contracts created with
#                                  all settlement OHLS data.  Default = 12. OHLS data
#                                  includes open, high, low, settle, change, volume,
#                                  and open interest (priorOI).
#
#                   calendar       Character string.  Path and file name for the text
#                                  delimited file with the forward looking expiration
#                                  dates of futures contracts.  The text file columns
#                                  are "Contract" and "Expiry". The first column is a
#                                  vector of futures contract codes by month and
#                                  using Quandl format codes (i.e. CLF1990). The next
#                                  or second column has the expiration date by futures
#                                  in the format 1999-05-31.
#
#                   cache:         Character string. The path where downloaded data
#                                  and function output will be stored. Defaults to
#                                  the standard project cache.path as defined in the
#                                  project configuration file.  The default path can
#                                  be replaced with any valid path.
#
# Dev.Notes:    N/A
#
# Depends       See the script Config_Trading.R in project config directory.
#               See function script get_futures.R in the project lib directory.
#
# References:   https://www.quandl.com/data/SRF-Reference-Futures/documentation
##----------------------------------------------------------------------------------------##

get_term_structure <- function(mkt.code,
                               fut.code,
                               fut.name,
                               fut.start.mo,
                               fut.end.mo,
                               data.start = ymd("1900-01-01"),
                               nterm = 36,
                               OHLS = 12,
                               calendar,
                               cache = cache.path){

     #1. Quandl Download ----
     # define Quandl data service and source
     dat.code <- "SRF"
     # define target contracts for download
     quandl.code <- paste(dat.code, "/",mkt.code, "_", fut.code, sep = "")
     month.code <- c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")
     fut.seq <- seq(fut.start.mo, fut.end.mo, by = "months")
     fut.df <- tibble(month.num = month(fut.seq),
                      year = year(fut.seq)) %>%
          mutate(month.code = month.code[month.num],
                 futures.code = paste(quandl.code, month.code, year, sep = ""))
     # define empty object to collect expiration dates
     expiry <<- tibble()
     # download data
     contracts <- as.list(as.character(t(fut.df$futures.code)))
     map(contracts, get_futures)
     ##----------------------------------------------------------------------------------##

     #2. Manage Expiration Calendar ----
     # load forward looking calendar
     cal.name <- paste0(fut.code, ".cal")
     cal.dat <- read_delim(calendar, delim = "\t", col_names = FALSE) %>%
          rename(Contract = X1, Expiry = X2)
     assign(cal.name, cal.dat)

     # update historical expiry data with forward looking calendar
     temp.name <- paste0(fut.code, "_expiry")
     assign(temp.name, rbind(expiry[!expiry$Contract %in% get(cal.name)$Contract,],
                             get(cal.name)))

     # save binary to cache and and MS Excel workbook to data
     save(list = temp.name, file = paste0(cache.path, temp.name, ".Rdata"))
     wb <- write.xlsx(get(temp.name), file = paste0(data.path, temp.name, ".xlsx"))
     saveWorkbook(wb, paste0(data.path, temp.name, ".xlsx"), overwrite = TRUE)
     ##----------------------------------------------------------------------------------##

     # 3. Load Futures Contracts Data ----
     # define target contracts to load
     month.code <- c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")
     fut.seq <- seq(fut.start.mo, fut.end.mo, by = "months")
     fut.df <- tibble(month.num = month(fut.seq),
                      year = year(fut.seq)) %>%
          mutate(month.code = month.code[month.num],
                 data.file = paste(fut.code, month.code, year, ".Rdata", sep = ""))

     # load price data for all contracts
     # creates long data object by contract month
     price.dat <<- tibble()
     for (i in 1:dim(fut.df)[1]) {
          temp.file <- as.character(fut.df[i, "data.file"])
          print(paste0("Loading: ", temp.file))
          load(paste0(cache.path, temp.file))
          temp.dat <- get(strsplit(temp.file, ".", fixed = TRUE)[[1]][1]) %>%
               mutate(., contract = substr(temp.file, 1, 7))
          price.dat <<- rbind(price.dat, temp.dat)
     }
     # purge binary data from memory
     rm(list = grep(paste0("^", fut.code, ".", "\\d{4}"), ls(), value = TRUE))
     ##-----------------------------------------------------------------------------------##

     # 4. Define Term Structure Components ----
     # creates wide data objects for each variable
     price.dat.settle <- price.dat %>%
          select(date, contract, settle) %>%
          spread(., key = contract, value = settle) %>%
          select(date, unique(price.dat$contract))

     price.dat.open <- price.dat %>%
          select(date, contract, open) %>%
          spread(., key = contract, value = open) %>%
          select(date, unique(price.dat$contract))

     price.dat.high <- price.dat %>%
          select(date, contract, high) %>%
          spread(., key = contract, value = high) %>%
          select(date, unique(price.dat$contract))

     price.dat.low <- price.dat %>%
          select(date, contract, low) %>%
          spread(., key = contract, value = low) %>%
          select(date, unique(price.dat$contract))

     price.dat.change <- price.dat %>%
          select(date, contract, change) %>%
          spread(., key = contract, value = change) %>%
          select(date, unique(price.dat$contract))

     price.dat.volume <- price.dat %>%
          select(date, contract, volume) %>%
          spread(., key = contract, value = volume) %>%
          select(date, unique(price.dat$contract))

     price.dat.priorOI <- price.dat %>%
          select(date, contract, prior.OI) %>%
          spread(., key = contract, value = prior.OI) %>%
          select(date, unique(price.dat$contract))

     # save binary data to cache folder; save MS Excel workbook to data folder
     save(list = "price.dat.settle", file = paste0(cache.path, fut.code, "_all.prices.Rdata"))
     wb <- write.xlsx(price.dat.settle, file = paste0(data.path, fut.code, "_all.prices.xlsx"))
     saveWorkbook(wb, paste0(data.path, fut.code, "_all.prices.xlsx"), overwrite = TRUE)
     ##-----------------------------------------------------------------------------------##

     # 5. Define Term Structure Time Series Data ----
     # create countinuous futures contracts 1:nterm by trade day
     # continous futures are named 1NBY, 2NBY for first nearby, second nearby, etc
     OHLS.vars <- c("open", "high", "low", "settle", "change", "volume", "priorOI")
     expiry <- get(paste0(fut.code, "_expiry"))
     for (i in 1:length(OHLS.vars)) {
          temp.var <- OHLS.vars[i]
          temp.name <- paste0(fut.code, "_NBY.", temp.var)
          assign(temp.name, tibble())
          temp.dat <- get(paste0("price.dat.", temp.var))
          j <- 1
          while (j <= (dim(expiry)[1] - nterm - 1)) {
               if (j == 1) {
                    row.start <- data.start
               }else{
                    row.start <- expiry$Expiry[j - 1]
               }
               row.end <- expiry$Expiry[j]
               if (row.end > temp.dat$date[dim(temp.dat)[1]]) {
                    break()
               }
               print(paste0(temp.var, " - ", expiry$Contract[j]))
               col.index <- (j + 1):(j + nterm)
               temp.dat2 <- temp.dat %>%
                    select(date, col.index) %>%
                    filter(date > row.start & date <= row.end)
               names(temp.dat2) <- c("date", paste0("NBY", 1:nterm))
               assign(temp.name, rbind(get(temp.name), temp.dat2))
               j <- j + 1
          }

          # save binary data to cache folder; save MS Excel workbook to data folder
          save(list = temp.name, file = paste0(cache.path, temp.name, ".Rdata"))
          wb <- write.xlsx(get(temp.name), file = paste0(data.path, temp.name, ".xlsx"))
          saveWorkbook(wb, paste0(data.path, temp.name, ".xlsx"), overwrite = TRUE)
     }
     ##-----------------------------------------------------------------------------------##

     # 6. Create OHLS Data ----
     # create open-high-low-settle data by maturity
     start.NBY <- 1
     end.NBY <- OHLS
     series.NBY <- paste0("NBY", start.NBY:end.NBY)
     load("~/Dropbox/Trading/R_Projects/NearbyPrices/cache/CL_NBY.settle.Rdata")
     date.NBY <- get(paste0(fut.code, "_NBY.settle"))$date


     for (i in series.NBY) {
          print(i)
          # declare variables
          open <- pluck(get(paste0(fut.code, "_NBY.open")), i)
          high <- pluck(get(paste0(fut.code, "_NBY.high")), i)
          low <- pluck(get(paste0(fut.code, "_NBY.low")), i)
          settle <- pluck(get(paste0(fut.code, "_NBY.settle")), i)
          change <- pluck(get(paste0(fut.code, "_NBY.change")), i)
          volume <- pluck(get(paste0(fut.code, "_NBY.volume")), i)
          priorOI <- pluck(get(paste0(fut.code, "_NBY.priorOI")), i)
          # create tbl_df
          temp.NBY <- tibble(date = date.NBY, open = open, high = high, low = low,
                             settle = settle, change = change, volume = volume,
                             priorOI = priorOI)
          temp.name <- paste0(fut.code, "_", i)
          assign(temp.name, temp.NBY)

          # save binary data to cache folder; save MS Excel workbook to data folder
          save(list = temp.name, file = paste0(cache.path, temp.name, ".Rdata"))
          wb <- write.xlsx(temp.name, file = paste0(data.path, temp.name, ".xlsx"))
          saveWorkbook(wb, paste0(data.path, temp.name, ".xlsx"), overwrite = TRUE)
     }
}
##---------------------------------------------------------------------------------------##