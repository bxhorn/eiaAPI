# Initialize Code ----
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(RJSONIO))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(ape))
suppressPackageStartupMessages(library(phytools))
suppressPackageStartupMessages(library(ggtree))
options(tibble.print_max = 15, tibble.print_min = 10)
options(tibble.width = NULL)
key <- "702dd5c99ca504ffa0c10e479e34f234"
##-------------------------------------------------------------------------------------------##
# Download eia Data Series (Series Query)

# Description:
# Call the eia_API and download data by series ID. call function can be used for
# single or bulk series downloads.

# Usage:
# call_eia(series.ID, key, cache.data = FALSE, cache.metadata = FALSE,
#         cache.path = getwd())

# Arguments:
# series.ID         An eia character string or vector used to download one or more
#                   data series.
# key               An API registration key (supplied by eia).
# cach.data         A logical.  Defines if the data series is saved to disk.
# cache.metadata    A logical. Defines if the metadata header info is saved to disk.
# cache.path        A character string for the save data path.

# Details:
# The series.ID is a unique string required by the eia_API. A single call for 100
# data series is supported by the API.  Arguments support data caching of data and/or
# header information.

# Value:
# A modern data.frame of "tibble" (tbl.df) with the data series by date. The series
# ID is the assigned data object name.  Data caching, if true, saves the series to
# disk in R binary format.

# Dev Notes:
# NA

# Reference:
# see https://www.eia.gov/opendata/register.php for API key registration and
# http://bxhorn.com/eiaAPI for the package web page

# See Also:
# browse_eia and search_eia

# Examples:
# single series download
call_eia(series.ID, key, TRUE, TRUE, cache.path = cache.path)
# multiple series download
series.ID <- "PET.W_EPC0_FPF_SAK_MBBLD.W"
series.ID <- c("PET.W_EPC0_FPF_SAK_MBBLD.W", "ELEC.CONS_TOT_BTU.NG-AK-96.A")


# Source Code
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

##-------------------------------------------------------------------------------------------##
