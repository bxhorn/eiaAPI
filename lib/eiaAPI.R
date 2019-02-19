# Header ----
#
# Name:        eiaAPI
#
# Title:       Tools for interfacing with eia's open data API.
#
# Version:     1.0.0
# Date:        2017-Jul-01
# Author:      Bradley Horn
# Maintainer:  Bradley Horn <bxhorn@gmail.com>
# License:     GPL (>=2)
#
# Description: Comprehensive tools for searching and retrieving open data from eia. R
#              functions simplify API interface and command syntax, expand eia data
#              search capabilities, and make eia data more accessible. Data downloads
#              include single or mutltiple time series, bulk data blocks, incremental
#              data updates, and spatial data objects.  All functions return data in
#              a tidy format using modern data frames or "tibble"  objects (tbl_df).
#
# Note:        https://www.eia.gov/opendata/register.php for API key registration
#
# Depends:     base R, RCurl, RJSONIO, lubridate, dplyr, tibble, stringr
#
# Dev.Notes:   1) call.EIA needs to be able to take multiple series IDs seperated by
#              semicolons (up to 100).  To this end, need to assess the length of the
#              ID input paramter,
#
# References:  https://www.eia.gov/opendata/commands.php for API commands
##-------------------------------------------------------------------------------------------##

# 0.Initialize Code ----
# configure work space
#source("/home/bxhorn/Dropbox/Trading/R_Projects/NearbyPrices/config/Config_Trading.R")
##-------------------------------------------------------------------------------------------##

# Browse Category ID's and Category Tree Structure (API Category Query)

# Description:
#              The browse search method is an API function to find data series using
#              category ID's. The API Category Query can be used recursively to unfold
#              the category tree until a specific data series or groups of series is
#              found. Once the desired series are found, data can be downloaded using
#              retrieval calls to the API.

# Usage:
#              browse.EIA(cat.ID = 371, key = NULL)

# Arguments:
# cat.ID       An eia API category number.  371 is the top of the category tree
# key          An API registration key supplied by eia

# Author(s):
#              Bradley Horn

# Value:
#              A modern data.frame (tbl_df or tibble) with eia data category IDs and
#              names.  When the query has hit a terminal node in the category tree,
#              then the function returns data time series IDs, names, update frequency,
#              units of measure, and the most recent date that data was updated

# See Also:
#              search.EIA, call.EIA

# Examples:
#              show the top of the category tree
#                  browse.EIA(cat.ID = 371, key)
#              show data series for one of the lowest category levels
#                  browse.EIA(cat.ID = 761, key)


# Source Code:
browse.EIA <- function(cat.ID = 371,
                       key = NULL) {

     # confirm EIA key
     if (is.null(key)) {
          stop("Invalid or missing api_key ... see https://www.eia.gov/opendata/register.php",
               call. = FALSE)
     }
     # browse root level categories or a user defined category / subcategory
     if (cat.ID == 371) {
          url <- paste0("http://api.eia.gov/category/?api_key=", key, "&category_id=371")
     } else{
          url <- paste0("http://api.eia.gov/category/?api_key=", key, "&category_id=", cat.ID)
     }

     # download JSON data
     txt <- getURLContent(url)
     dat <- fromJSON(txt, asText = TRUE, simplify = TRUE)

     # assess category browse level and format return data
     if (class(dat$category$childcategories) == "list") {
          dat.df <- do.call("rbind", lapply(dat$category$childcategories, function(x) head(x)))
          cat.id <- matrix(unlist(strsplit(dat.df[, 1], "[.]")), ncol = 2, byrow = TRUE)[,1]
          dat.tbl <- tibble(category_id = as.numeric(cat.id),
                            name = dat.df[, 2])
     } else {
          dat.df <- do.call("rbind", lapply(dat$category$childseries, function(x) head(x)))
          dat.tbl <- as_tibble(dat.df)
     }
     if ( sum(dim(dat.tbl)) == 0) {
          stop("No Result Found. Try A Different Category ID", call. = FALSE)
     }
     return(dat.tbl)
}
##-------------------------------------------------------------------------------------------##

# Search EIA Data Series By Keyword or Series ID or Date

search.EIA <- function(x = NULL,
                       search.type = keyword,
                       filters.keep = NULL,
                       filters.remove = NULL,
                       start.date = NULL,
                       end.date = NULL,
                       rows = 100) {

     switch(search.type,
            series.ID = {
                 # configure search term(s) for EIA api
                 x <- str_replace_all(x, "[\\.\\_\\-]", "")

                 # download JSON data
                 url <- paste0("http://api.eia.gov/search/?search_term=series_id&search_value=",
                               x, "&rows_per_page=", rows)
                 txt <- getURLContent(url)

                 # format return data
                 dat <- fromJSON(txt, asText = TRUE, simplify = TRUE)
                 dat.df <- as.data.frame(do.call("rbind", lapply(dat$response$docs,
                                                                 function(x) head(x))))
                 dat.tbl <- as.tibble(dat.df)
            },
            keyword = {
                 # configure search term(s) for EIA api
                 x <- gsub(" ", "+", paste(x, collapse = " "))

                 # download JSON data
                 url <- paste0("http://api.eia.gov/search/?search_term=name&search_value=",
                               x, "&rows_per_page=", rows)
                 txt <- getURLContent(url)

                 # format return data
                 dat <- fromJSON(txt, asText = TRUE, simplify = TRUE)
                 dat.df <- as.data.frame(do.call("rbind", lapply(dat$response$docs,
                                                                 function(x) head(x))))
                 dat.tbl <- as.tibble(dat.df)
            },
            date = {
                 if (is.null(start.date) | is.null(end.date)) {
                      stop("Missing start or end date", call. = FALSE)
                 }
                 # configure search term(s) for EIA api
                 x <- paste0("[", start.date, "%20TO%20", end.date, "]")

                 # download JSON data
                 url <- paste0("http://api.eia.gov/search/?search_term=last_updated&search_value=",
                               x, "&rows_per_page=", rows)
                 txt <- getURLContent(url)
                 if (grepl("solr connection failed", txt)) {
                      stop("Connection Failed. Check date format. Format should be: 2017-06-15T00:00:00Z",
                           call. = FALSE)
                 }

                 # format return data
                 dat <- fromJSON(txt, asText = TRUE, simplify = TRUE)
                 dat.df <- as.data.frame(do.call("rbind", lapply(dat$response$docs,
                                                                 function(x) head(x))))
                 dat.tbl <- as.tibble(dat.df)
            }
     )

     # filter return data given keep vs remove strings
     if (!is.null(filters.keep)) {
          filters.keep <- gsub(" ", "|", paste(filters.keep, collapse = " "))
          dat.tbl <- dat.tbl %>%
               filter(str_detect(name, filters.keep))
     }
     if (!is.null(filters.remove)) {
          filters.remove <- gsub(" ", "|", paste(filters.remove, collapse = " "))
          dat.tbl <- dat.tbl %>%
               filter(!str_detect(name, filters.remove))
     }
     return(dat.tbl)
}

# test function: series.ID
# search.EIA(x = "PET.MB", search.type = "series.ID")
# search.EIA(x = "PET.MB", search.type = "series.ID", filters.keep = "Annual")

# test function: keyword
# search.EIA(x = c("crude oil", "Alaska"), search.type = "keyword", rows = 500)
# search.EIA(x = c("crude oil", "Alaska"), search.type = "keyword",
#            filters.remove = c("Quarterly", "Annual"), rows = 500)

# test function: date
# date1 <- "2017-06-14T00:00:00Z"
# date2 <- "2017-06-18T23:59:59Z"
# search.EIA(search.type = "date", start.date = date1, end.date = date2, rows = 1500)


##-------------------------------------------------------------------------------------------##

call_eia <- function(series.ID = NULL,
                     key = NULL,
                     cache.data = FALSE,
                     cache.metadata = FALSE,
                     cache.path = getwd()) {

     # confirm series.ID and key
     if (is.null(series.ID)) {
          stop("Error - series.ID is NULL.  Enter one or more series ID's.")
     }
     if (!is.character(series.ID)) {
          stop("Error - series.ID must be a character string or vector")
     }
     if (is.null(key)) {
          stop("Invalid or missing api_key. See https://www.eia.gov/opendata/register.php",
               call. = FALSE)
     }
     n.series <- length(series.ID)
     # define query URL
     if (n.series > 1) {
          series.ID <- gsub(", ",";",toString(series.ID))
     }
     url <- paste0("http://api.eia.gov/series/?series_id=", series.ID, "&api_key=",
                   key, "&out=json")
     # download JSON data
     txt <- getURLContent(url)
     dat <- fromJSON(txt, asText = TRUE, simplify = TRUE, nullValue = NA)
     # format query data and configure based on series length
     if (n.series == 1) {
          dat.df <- as.data.frame(
               do.call("rbind", lapply(dat$series[[1]]$data, function(x) head(x))),
               stringsAsFactors = FALSE)
          dat.tbl <- tibble(Date = datetime.EIA(series.ID, dat.df),
                            value = as.numeric(unlist(dat.df[,2]))) %>%
               arrange(., Date) %>%
               set_colnames(c("Date", series.ID))
          # cache data
          if (cache.metadata == TRUE) {
               temp.dat <- capture.output(str(dat, max.level = 3))
               save(list = "temp.dat", file = paste0(cache.path, "/", series.ID, "_meta.data.Rdata"))
          }
          if (cache.data == TRUE) {
               save(list = "dat.tbl", file = paste0(cache.path, "/", series.ID, ".Rdata"))
          }
          assign(series.ID, dat.tbl, envir = .GlobalEnv)
     }
     # bulk data download
     if (n.series > 1) {
          series.ID <- unlist(strsplit(series.ID, ";"))
          for (i in 1:n.series) {
               dat.df <- as.data.frame(
                    do.call("rbind", lapply(dat$series[[i]]$data, function(x) head(x))),
                    stringsAsFactors = FALSE)
               dat.tbl <- tibble(Date = datetime.EIA(series.ID[i], dat.df),
                                 value = as.numeric(unlist(dat.df[,2]))) %>%
                    arrange(., Date) %>%
                    set_colnames(c("Date", series.ID[i]))
               # cache data
               if (cache.metadata == TRUE) {
                    temp.dat <- capture.output(str(dat, max.level = 3))
                    save(list = "temp.dat", file = paste0(cache.path, "/", series.ID[i], "_meta.data.Rdata"))
               }
               if (cache.data == TRUE) {
                    save(list = "dat.tbl", file = paste0(cache.path, "/", series.ID[i], ".Rdata"))
               }
               assign(series.ID[i], dat.tbl, envir = .GlobalEnv)
          }
     }
}

# test function
# key <- "702dd5c99ca504ffa0c10e479e34f234"
# ID <- "PET.W_EPC0_FPF_SAK_MBBLD.W"
# call.EIA(ID, key)
##-------------------------------------------------------------------------------------------##


##-------------------------------------------------------------------------------------------##

# Support functions
datetime.EIA <- function(series.ID, data) {
     type <- gsub(".*(.)$", "\\1", series.ID)
     switch(type,
            A = {Date <- make_date(year = as.numeric(data[, "V1"]), month = 12, day = 31)
            },
            Q = {yr <- matrix(unlist(strsplit(data[,1], "Q")), ncol = 2, byrow = TRUE)[, 1]
            mo <- matrix(unlist(strsplit(data[,1], "Q")), ncol = 2, byrow = TRUE)[, 2]
            Date <- make_date(year = as.numeric(yr), month = as.numeric(mo) * 3,
                              day = days_in_month(as.numeric(mo) * 3))
            },
            M = {yr <- as.numeric(substr(data[, 1], 1, 4))
            mo <- as.numeric(substr(data[, 1], 5, 6))
            Date <- make_date(year = yr, month = mo, day = days_in_month(mo))
            },
            W = {yr <- as.numeric(substr(data[, 1], 1, 4))
            mo <- as.numeric(substr(data[, 1], 5, 6))
            dy <- as.numeric(substr(data[, 1], 7, 8))
            Date <- make_date(year = yr, month = mo, day = dy)
            },
            D = {yr <- as.numeric(substr(data[, 1], 1, 4))
            mo <- as.numeric(substr(data[, 1], 5, 6))
            dy <- as.numeric(substr(data[, 1], 7, 8))
            Date <- make_date(year = yr, month = mo, day = dy)
            },
            H = {yr <- as.numeric(substr(data[, 1], 1, 4))
            mo <- as.numeric(substr(data[, 1], 5, 6))
            dy <- as.numeric(substr(data[, 1], 7, 8))
            hr <- as.numeric(substr(data[, 1], 10, 11))
            Date <- make_datetime(year = yr, month = mo, day = dy, hour = hr,
                                  min = 59, sec = 59)
            },
            stop("The data series ID must end with A, Q, M, W, D or H to call EIA",
                 call. = FALSE)
     )
     return(Date)
}



##-------------------------------------------------------------------------------------------##

# Junk ---
# different ways to extract data from JSON text structure (JavaScropt Object Notation)
# ID <- "NG.RNGC1.D"
# url <- paste0("http://api.eia.gov/series/?api_key=", key, "&series_id=", ID)
# txt <- getURLContent(url)
# dat <- fromJSON(txt, asText = TRUE, simplify = TRUE)
# test <- t(sapply(dat$series[[1]]$data, function(x) rbind(x)))
# test2 <- t(sapply(dat$series[[1]]$data, function(x) head(x)))
# test3 <- matrix(unlist(dat$series[[1]]$data), ncol = 2, byrow = TRUE)
# test4 <- do.call("rbind", lapply(dat$series[[1]]$data, function(x) head(x)))

# test filters
# mtcars$type <- rownames(mtcars)
# mtcars %>%
#      filter(str_detect(type, 'Toyota|Mazda'))
# mtcars %>%
#      filter(!str_detect(type, 'Toyota|Mazda'))

# test multi character replace
# x <- "H.E_L-L.O"
# str_replace_all(x, "[\\.\\_\\-]", "")

