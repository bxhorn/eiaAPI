# Initialize Code ----
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(RJSONIO))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(igraph))
options(tibble.print_max = 5, tibble.print_min = 5)
options(tibble.width = NULL)
key <- "702dd5c99ca504ffa0c10e479e34f234"
##-------------------------------------------------------------------------------------------##

# Date/Time Frequencies for API Series Query

# Description:
# A helper function to format and manage the date/time frequency of EIA data series

# Usage:
# datetime.EIA(series.ID, data)

# Arguments:
# series.ID         An EIA character string (or character vector) used to download
#                   one or multiple data series.
# data              Not a user defined input.  Defined by calls to the eia_API and based
#                   on the data series.ID requested and the data series returned.

# Details:
# datetime.EIA() is an internal function used by call.EIA() to determine the data
# series type.

# Value:
# ISO compliant date or datetime objects of class Date.

# Reference:
# see https://www.eia.gov/opendata/register.php for API key registration and
# http://bxhorn.com/eiaAPI for the package web page

# See Also:
# callEIA

# Examples:
# Not applicable


# Source Code:
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
