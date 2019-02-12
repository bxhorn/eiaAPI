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

# Define the Parent Category of an EIA Data Series (API Series Categories Query)

# Description:
# A wrapper for an API query that returns the parent category ID and name for a data
# series.


# Usage:
# parentEIA(series.ID, key)

# Arguments:
# series.ID    A character string.  The ID name for an EIA data series.
# key          An API registration key (supplied by EIA)

# Details:
# The the parent category ID for any EIA data series is the termal node on the
# EIA category tree that a data series belongs to.  The parent function is helpful to
# understand the structure of EIA data and to find related data series.  It is also a
# helper function to map category IDs across the EIA data tree (e.g. from the root
# node to the terminal node and target data series).

# Value:
# A modern data.frame (tibble) that defines the parent category ID and name of an EIA
# data series.   If the data.frame has more than one row, then the data series has more
# than one parent (e.g. linked to more than one terminal node on the category tree).

# Reference:
# see https://www.eia.gov/opendata/register.php for API key registration and
# http://bxhorn.com/eiaAPI for the package web page

# See Also:
#

# Examples:
parent.EIA(series.ID = "PET.W_EPC0_FPF_SAK_MBBLD.W", key)



# Source Code:
parent.EIA <- function(series.ID = NULL,
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
     dat <- fromJSON(txt, asText = TRUE, simplify = TRUE, nullValue = NA)

     # format return data
     dat.df <- do.call("rbind", lapply(dat$series_categories[[1]]$categories, function(x) head(x)))
     dat.tbl <- tibble(category_id = as.integer(dat.df[, 1]),
                       name = dat.df[, 2])

     return(dat.tbl)
}


