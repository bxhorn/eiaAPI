# Header ----
#
# Package:     eiaAPI
#
# Title:       Robust tools to interface with EIA's open data API.
#
# Description: This package provides a comprehensive toolset for searching and
#              retrieving energy data from the EIA. R functions have been designed
#              to simplify API interface, to expand EIA data search capabilities, and
#              to make EIA data more accessible.  Data downloads include single or
#              mutltiple time series, bulk data blocks, incremental data updates,
#              and spatial data objects.
#
# Note:        https://www.eia.gov/opendata/register.php for API key registration and
#              http://bxhorn.com/eiaAPI for package vignette and expamples
#
# Version:     1.0.0
# Date:        2017-Jul-01
# Author:      Bradley Horn
# Maintainer:  Bradley Horn <bxhorn@gmail.com>
# License:     GPL (>=2)
# Depends:     base R, RCurl, RJSONIO, lubridate, dplyr, tiblle, stringr
#
# Dev.Notes:   1) call.EIA needs to be able to take multiple series IDs seperated by
#              semicolons (up to 100).  To this end, need to assess the length of the
#              ID input paramter, to determine if a download loop is required.  Might
#              be best to save all series in a new environemnt named EIA
#              2) search.EIA should allow for use of start/end dates for series.ID and
#              and keyword searches...basically expand the filters to include dates
#              test with all filters active
#              3) need to have the ability to map a series to ALL parent categories
##-------------------------------------------------------------------------------------------##

# 0.Initialize Code ----
library(RCurl)
library(RJSONIO)
library(lubridate)
library(dplyr)
library(tibble)
library(stringr)
options(tibble.print_max = 20, tibble.print_min = 6)
options(tibble.width = NULL)
key <- "702dd5c99ca504ffa0c10e479e34f234"
##-------------------------------------------------------------------------------------------##

# Browse category ID's (API Category Query)

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
          url <- paste0("http://api.eia.gov/category/?api_key=", key)
     }

     # download JSON data
     txt <- getURLContent(url)
     dat <- fromJSON(txt, asText = TRUE, simplify = TRUE)

     # assess category browse level and format return data
     if (class(dat$category$childcategories) == "list") {
          dat.df <- do.call("rbind", lapply(dat$category$childcategories, function(x) head(x)))
          cat.id <- matrix(unlist(strsplit(dat.df[, 1], "[.]")), ncol = 2, byrow = TRUE)[,1]
          dat.tbl <- tibble(category_id = as.integer(cat.id),
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

# Find the ID category a data series belongs to (API Series Categories Query)

# Source Code:
categoryEIA <- function(series.ID = NULL,
                        key = NULL) {

     # confirm series ID and EIA key
     if (is.null(series.ID)) {
          stop("Invalid or missing series ID",
               call. = FALSE)
     }
     if (is.null(key)) {
          stop("Invalid or missing api_key ... see https://www.eia.gov/opendata/register.php",
               call. = FALSE)
     }

     # download JSON data
     url <- paste0("http://api.eia.gov/series/categories/?series_id=", series.ID, "&api_key=", key)
     txt <- getURLContent(url)
     dat <- fromJSON(txt, asText = TRUE, simplify = TRUE)

     # format return data
     dat.df <- do.call("rbind", lapply(dat$series_categories[[1]]$categories, function(x) head(x)))
     dat.tbl <- tibble(category_id = as.integer(dat.df[, 1]),
                       name = dat.df[, 2])

     return(dat.tbl)
}

##-------------------------------------------------------------------------------------------##

# Search EIA Data Series By Keyword or Series ID or Date (API Search Data Query)

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

##-------------------------------------------------------------------------------------------##

call.EIA <- function(ID,
                     key = NULL,
                     cache.meta.data = FALSE,
                     cache.data = FALSE,
                     cache.path = getwd()) {

     # confirm ID and EIA key
     if (!is.character(ID)) {
          stop("The data series ID must be a character string to call EIA",
               call. = FALSE)
     }
     if (is.null(key)) {
          stop("Invalid or missing api_key ... see https://www.eia.gov/opendata/register.php",
               call. = FALSE)
     }

     # download JSON data
     url <- paste0("http://api.eia.gov/series/?api_key=", key, "&series_id=", ID)
     txt <- getURLContent(url)

     # format return data and configure based on series type
     dat <- fromJSON(txt, asText = TRUE, simplify = TRUE)
     dat.df <- as.data.frame(do.call("rbind", lapply(dat$series[[1]]$data,
                                                     function(x) head(x))),
                             stringsAsFactors = FALSE)
     type <- gsub(".*(.)$", "\\1", ID)
     dat.tbl <- tibble(Date = date.series(type, dat.df),
                       value = dat.df[, -1]) %>%
          arrange(., Date)
     names(dat.tbl) <- c("Date", ID)

     # cache data
     if (cache.meta.data == TRUE) {
          file.name <- paste0(cache.path, "/", ID, "_meta.data.csv")
          capture.output(str(dat, max.level = 3), file = file.name)
     }
     if (cache.data == TRUE) {
          file.name <- paste0(cache.path, "/", ID, "_data.csv")
          write.csv(dat.tbl, file = file.name)
     }
     return(dat.tbl)
}

# test function
key <- "702dd5c99ca504ffa0c10e479e34f234"
ID <- "PET.W_EPC0_FPF_SAK_MBBLD.W"   #"PET.W_EPC0_FPF_SAK_MBBLD.W"
call.EIA(ID, key)
##-------------------------------------------------------------------------------------------##


##-------------------------------------------------------------------------------------------##

# Support functions
date.series <- function(series.code, data) {
     switch(series.code,
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
ID <- "NG.RNGC1.D"
url <- paste0("http://api.eia.gov/series/?api_key=", key, "&series_id=", ID)
txt <- getURLContent(url)
dat <- fromJSON(txt, asText = TRUE, simplify = TRUE)
test <- t(sapply(dat$series[[1]]$data, function(x) rbind(x)))
test2 <- t(sapply(dat$series[[1]]$data, function(x) head(x)))
test3 <- matrix(unlist(dat$series[[1]]$data), ncol = 2, byrow = TRUE)
test4 <- do.call("rbind", lapply(dat$series[[1]]$data, function(x) head(x)))

# test filters
mtcars$type <- rownames(mtcars)
mtcars %>%
     filter(str_detect(type, 'Toyota|Mazda'))
mtcars %>%
     filter(!str_detect(type, 'Toyota|Mazda'))

# test multi character replace
x <- "H.E_L-L.O"
str_replace_all(x, "[\\.\\_\\-]", "")

