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

# Browse eia Category ID's (Category Query)

# Description:
# The browse function is a wrapper for the Category Query.  It is used to find
# the name and id for a data category or sub-category names. The function should be
# used recursively to unfold the category tree until a specific data series or group
# of series is located.  Data is download using different function calls.

# Usage:
# browse_eia(cat.ID = 371, key = NULL, nrows = 10, plot.as.tree = FALSE)

# Arguments:
# cat.ID            An API category or sub-category number.  371 is the top of the
#                   category tree and is used as the default to reveal other
#                   subcategory numbers.
# key               An API registration key (supplied by eia)
# nrows             An integer. Controls the number of data series output rows. Does
#                   not impact the number of categories or sub-categories returned.
# plot.as.tree      A logical to control the creation of a graphical tree with eia
#                   data categeories and sub-categories

# Details:
# see https://www.eia.gov/opendata/register.php to get an API registration key.
#
# If plot.as.tree is TRUE, the R graphical device must be sized to be large enough
# that the graphical tree is not visually corrupted (e.g. maximize the plot pane
# in RStudio or use par() to set device options).  This argument is active only when
# the query results return categories and sub-categories only. A graphical tree is not
# be created when the browse output is a large list of data series with long names.
# Data series lists presented in table form only for clarity of results.

#
# Value:
# A modern data.frame (tibble) with data category IDs and names, plus a graphical tree
# diagram when plot.as.tree = TRUE.  Sub-categories are the standard output of the
# browse Category Query.
#
# If the input category ID lies on the terminal boundary of the category tree, then a
# data.frame is produced with eia data series. nrows applies to data series results
# only. If nrows is positive (negative), it defines the number of data series returned
# from the top (bottom) of the query.  Browse results for EIA data series include the
# data series ID, name, frequency, units of measure, and the date the data series was
# last updated.

# Reference:
# see https://www.eia.gov/opendata/register.php for API key registration and
# http://bxhorn.com/eiaAPI for the package web page

# See Also:
# search_eia and call_eia

# Examples:
# browse the root of the EIA category tree
browse_eia(cat.ID = 371, key)
browse_eia(cat.ID = 371, key, plot.as.tree = TRUE)
# browse subcategories with visial tree
browse_eia(cat.ID = 40203, key, plot.as.tree = TRUE)
browse_eia(cat.ID = 714755, key, plot.as.tree = TRUE)
# browse data series (using terminal node on the category tree)
browse_eia(cat.ID = 761, key, nrows = 6)
browse_eia(cat.ID = 761, key, nrows = -6)
# visual tree with a large number of output categores
# maximize size of plot window/device
browse_eia(cat.ID = 2134384, key, plot.as.tree = TRUE)

# Source Code:
browse_eia <- function(cat.ID = NULL,
                       key = NULL,
                       nrows = 10,
                       plot.as.tree = FALSE) {

     # confirm cat.ID and key
     if (is.null(cat.ID)) {
          stop(c("Error - cat.ID is NULL and must be specified.\n",
               "Try 371, the eia root level category to find additional ID's"))
     }
     if (!is.integer(cat.ID)) {
          stop("Error - cat.ID must be an integer")
     }
     if (is.null(key)) {
          stop("Invalid or missing api_key. See https://www.eia.gov/opendata/register.php",
               call. = FALSE)
     }
     #define query URL
     url <- paste0("http://api.eia.gov/category/?api_key=", key, "&category_id=",
                   cat.ID, "&out=json")
     # download JSON data
     txt <- getURLContent(url)
     dat <- fromJSON(txt, asText = TRUE, simplify = TRUE, nullValue = NA)
     # test query return and format
     if (class(dat$category$childcategories) == "list") {
          dat.df <- do.call("rbind", lapply(dat$category$childcategories, function(x) head(x)))
          cat.id <- matrix(unlist(strsplit(dat.df[, 1], "[.]")), ncol = 2,
                           byrow = TRUE)[, 1]
          dat.tbl <- tibble(category_id = as.integer(cat.id), name = dat.df[, 2])
          # plot category tree structure
          if (plot.as.tree == TRUE) {
               # define tree nodes, edges and labels
               tree.nodes <- tibble(node.id = 1:(dim(dat.tbl)[1] + 1),
                                    category.id = c(dat.tbl$category_id, cat.ID),
                                    name = c(dat.tbl$name, dat$category$name))
               tree.edges <- tibble(from = rep(dim(dat.tbl)[1] + 1, dim(dat.tbl)[1]),
                                    to = 1:(dim(dat.tbl)[1]))
               n.label <- paste0(dat$category$category_id, " - ", dat$category$name)
               t.labels <- paste0(dat.tbl$category_id, " - ", dat.tbl$name) %>%
                    gsub("\r\n", "", .)
               # define tree data object and coerce to class phylo
               eia.tree <- list(node.count = 1,
                                node.label = n.label,
                                edge = as.matrix(tree.edges),
                                tip.label = rev(t.labels),
                                tip.num = sprintf("%02d", 1:(dim(dat.tbl)[1])))
               class(eia.tree) <- "phylo"
               # compute tree branch lengths, enable singltons and reorder tree edges
               eia.tree <- compute.brlen(eia.tree, method = "Grafen")
               eia.tree <- collapse.singles(eia.tree)
               eia.tree <- reorder(eia.tree, order = "cladewise")
               # configure and plot tree
               if (length(eia.tree$tip.label) >= 50) {
                    font.size <- 2.50
                    } else {
                    font.size <- 3.75
                    }

               p <- ggtree(eia.tree, color = "plum3", size = 0.25) +
                    # format nodes
                    geom_nodepoint(color = "orange2", size = 3) +
                    geom_text2(aes(subset = !isTip, label = label), hjust = -0.10,
                               angle = 0, color = "orange3", size = 3.75,
                               nudge_y = 0.03) +
                    # format tips
                    geom_tippoint(color = "navy", shape = 15, size = 3) +
                    geom_text2(aes(subset = isTip, label = label), hjust = 0,
                               angle = 0, color = "navy", size = font.size,
                               nudge_x = 0.05) +
                    ggplot2::xlim(0, 2)

               print(p)
          }
     } else {
          dat.df <- do.call("rbind", lapply(dat$category$childseries, function(x) head(x)))
          dat.tbl <- as_tibble(dat.df)
          ifelse(nrows >= 0,
                 return(head(dat.tbl, n = nrows)),
                 return(tail(dat.tbl, n = -nrows)))
     }
     if ( sum(dim(dat.tbl)) == 0) {
          stop("No Result Found. Try A Different Category ID", call. = FALSE)
     }
     return(dat.tbl)
}

##-------------------------------------------------------------------------------------------##
