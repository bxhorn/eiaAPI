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

# Search EIA data series by keyword, series ID or date (API Search Data Query)

# Description:
# The search function is an easy-to-use wrapper for three different API search
# capabilities and is used to locate EIA data series.

# Usage:
# search.EIA(x, search.type = keyword, filters.keep = NULL, filters.remove = NULL,
#            start.date = NULL, end.date = NULL, nrows = 100)

# Arguments:
# x                 A character string.  Search text input.  Character vectors allowed
#                   for multiple search inputs;
# search.type       A character string. Can be "series.id", "keyword" or "date"
# filters.keep      A character string.  Defines text to be used to filter API search
#                   results by data series name.  Filter will keep any matches.
# filters.remove    A character string.  Defines text to be used to filter API search
#                   results by data series name.  Filter will remove any matches.
# start.date        A character string of the format "2017-06-14T00:00:00Z".
# end.date          A character string of the format "2017-06-14T00:00:00Z".
# nrows             An integer to define the first n rows to be extracted from the
#                   API search results.

# Details:
# Input to search by series.ID search should include a subset or a portion of the full
# series.ID text. Input for the "keyword" search can be any word(s) that appear in
# the data series name.  Input for the date search must match the format "2017-06-14T00:00:00Z"
# to ensure an API connection.

# Input for the result filters (keep vs remove) can be a single word or a character
# vector.

# Start.date and end.date are used to define a date range search.  They also can be
# used as filters to control search returns for series.ID or keyword searches.  It is
# possble to use one or both dates when filtering search results.

# nrows will filter API search results to the first n rows found.

# Value:
# A modern data.frame (tibble) with search result comprosed of one or multiple EIA
# data series. Each data series is defined series ID, name, frequency, units of measure,
# and the frequency or date the data is updated.

# Reference:
# see https://www.eia.gov/opendata/register.php for API key registration and
# http://bxhorn.com/eiaAPI for the package web page

# See Also:
# browseEIA, callEIA

# Examples:
# search by series.ID
search.EIA(x = "PET.MB", search.type = "series.ID")
search.EIA(x = "PET.MB", search.type = "series.ID", filters.keep = "Annual")
# search by keyword
search.EIA(x = c("crude oil", "Alaska"), search.type = "keyword", nrows = 500)
search.EIA(x = c("crude oil", "Alaska"), search.type = "keyword",
           filters.remove = c("Quarterly", "Annual"), nrows = 500)
# search by date
date1 <- "2017-06-14T00:00:00Z"
date2 <- "2017-06-18T23:59:59Z"
search.EIA(search.type = "date", start.date = date1, end.date = date2, nrows = 1500)



# Source code:
search.EIA <- function(x = NULL,
                       search.type = keyword,
                       filters.keep = NULL,
                       filters.remove = NULL,
                       start.date = NULL,
                       end.date = NULL,
                       nrows = 100) {

     switch(search.type,
            series.ID = {
                 # configure search term(s) for EIA api
                 x <- str_replace_all(x, "[\\.\\_\\-]", "")

                 # download JSON data
                 url <- paste0("http://api.eia.gov/search/?search_term=series_id&search_value=",
                               x, "&rows_per_page=", nrows)
                 txt <- getURLContent(url)

                 # format return data
                 dat <- fromJSON(txt, asText = TRUE, simplify = TRUE, nullValue = NA)
                 dat.df <- as.data.frame(do.call("rbind", lapply(dat$response$docs,
                                                                 function(x) head(x))))
                 dat.tbl <- as.tibble(dat.df)
            },
            keyword = {
                 # configure search term(s) for EIA api
                 x <- gsub(" ", "+", paste(x, collapse = " "))

                 # download JSON data
                 url <- paste0("http://api.eia.gov/search/?search_term=name&search_value=",
                               x, "&rows_per_page=", nrows)
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
                               x, "&rows_per_page=", nrows)
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
