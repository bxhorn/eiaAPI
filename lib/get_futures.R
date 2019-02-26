# Header ----
#
# Name:         get_futures.R
# Title:        Get Futures Prices
# Version:      1.0
# Date:         2019-Jan-25
# Author:       Brad Horn
# License:      GPL (>=2)
#
# Description:  Extract futures data; manage and transform for tidy work flow; and
#               load into the project cache. The data source is Steven's Refernece
#               Futures (SRF) data via Quandl.
#
# Details:      Function input arguments:
#
#                   futures.codes:   Character string.  Futures contract codes input
#                                    as a formal list object in R. Each list element
#                                    or futures contract must be compliant with the
#                                    Quandl SRF code format. See rReference below for
#                                    futures contract codes.
#
#                   cache:           Character string. The path where downloaded data
#                                    will be stored. Defaults to the standard project
#                                    cache.path as defined in the project config.R file.
#                                    The default can be replaced using any valid path.
#
#               Downloaded data is converted to an advanced data.frame object known as
#               a tibble (tbl_df).  Data by futures contract is then saved an R binary
#               file (*.Rdata). The file expiry.Rdata is also created and contains a
#               list of of historical expiration dates by contract.
#
# Dev.Notes:    Use of the function requires a subscription to the Quandl SRF service
#               plus a Quandl authorization code entered using the quandl.api_key()
#               function. Authentication is part of work space configuration, not part
#               of the function
#
# Depends       Configuration script config.R in project config directory.  See
#               project work flow article for standard project template and structure
#               for repeatable results across machines and over time.
#
# References:   https://www.quandl.com/data/SRF-Reference-Futures/documentation
#               http://applied-r.com/r-work-flow/
##----------------------------------------------------------------------------------------##

# 1. Function Defintion ----
get_futures <- function(futures.code, cache = cache.path) {

     # print download target or status
     print(paste0("Downloading: ", futures.code))
     # quandl API call for targwt futures contract
     fut.dat <- tq_get(futures.code, get = "quandl")
     # manage and transform data
     fut.dat <- fut.dat %>%
          mutate(change = settle - lag(settle, 1),
                 prior.OI = prev.day.open.interest) %>%
          select(date, open, high, low, settle, change, volume, prior.OI)

     # define contract expiry date and bind to master list
     temp <- tibble(Contract = substr(futures.code, 9, 15),
                    Expiry = fut.dat[dim(fut.dat)[1], "date", drop = TRUE])

     expiry <<- rbind(expiry, temp)

     # save futures data in R binary format
     assign(substr(futures.code, 9, 15), fut.dat)
     save(list = substr(futures.code, 9, 15),
          file = paste0(cache, substr(futures.code, 9, 15), ".Rdata"))
}

# 2. Function Test ----
# contracts <- list("SRF/CME_CLU1993", "SRF/CME_CLV1993")
# map(contracts, get_futures)

