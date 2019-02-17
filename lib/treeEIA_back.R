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

# treeEIA ----

depth <- 10
current <- 1
tree <- list()

if (current == 1 & current < depth) {
     tree$level1 <- browse.EIA(371, key) %>%
          mutate(parent.0 = "EIA Data Sets")
     current <- current + 1
}

if (current == 2 & current < depth) {
     tree$level2 <- tree$level1$category_id %>%
          map(function(x) browse.EIA(cat.ID = x, key = key)) %>%
          set_names(level1$name) %>%
          list(., parent.0 = "EIA Data Sets", parent.1 = level1$name) %>%
          pmap(., cbind)
     current <- current + 1
}

if (current == 3 & current < depth) {
     tree$level3 <- list()
     for (i in seq.int(length(tree$level2))) {
          temp.name <- names(tree$level2[i])
          temp.level <- tree$level2[[i]]
          temp.name <- names(temp.level)
          temp.tree <-
     }

}

make.tree <- function(x) {
     id <- x$category_id
     name <- x$name
     map(function(id, name) browse.EIA(cat.ID = id, key = key)) %>%
}

tree$level2 <- tree$level1 %>%
     map(function(x) {print(x) %>%
          browse.EIA(cat.ID = x$category_id, key = key) %>%
              mutate(parent.0 = "EIA",
                      parent.1 = x$name)}) %>%
     set_names(level1$name)


tree$level2 <- list()
i=1

for (i in seq.int(dim(tree$level1)[1])) {
     id <- tree$level1$category_id[i]
     nameX <- tree$level1$name[i]
     tblX <- browse.EIA(cat.ID = id, key = key) %>%
          mutate(parent.0 = "EIA Data Sets",
                 parent.1 = nameX)
     tree$level2 <- append(tree$level2, list(assign(nameX, tblX)))
}






     depth <- depth + 1

     for (j in seq.int(length(root.list))) {
     new.root <- root.list[[1]]
     new.root.list <- new.root$category_id %>%
          map(function(x) browse.EIA(cat.ID = x, key = key)) %>%
          set_names(new.root$name)
     root.list[[1]] <- list(root.list[[1]], subcategories = list(new.root.list))
     }
}

test <- list(root.list[[1]], list(subcat = new.root.list))




# Junk ----
root <- browse.EIA(371, key)
root.list <- root$category_id %>%
     map(function(x) browse.EIA(cat.ID = x, key = key)) %>%
     set_names(root$name)
root.list

root.list2 <- root$category_id %>%
     map(function(x) browse.EIA(cat.ID = x, key = key)) %>%
     "names<-"(root$name)
root.list2

first <- c("a", "b", "c")
list.test <- sapply(first, function(x) {vector(mode = "list")})
# Header ----
#
# Name:         mapWPS.R
# Title:        Map all data elements in the eia Weekly Petroleum Status Report
# Version:      1.0
# Date:         2019-Feb-12
# Author:       Brad Horn
# License:      GPL (>=2)
#
# Description:  Creat a map of all data series IDs for each element in the WPS report
#               to support automated API calls to eia to download all WPS time series
#               data.
#
# Details:      NA
#
# Dev.Notes:    NA
#
# Depends:      See the configuration script Config_Trading.R in project config folder;
#               See custom package eiaAPI for functions to interface with the eia API
#               An eia API security key is requried
#
# References:   See https://www.eia.gov/opendata/register.php for API key registration
#               See https://www.eia.gov/opendata/commands.php for API commands
##----------------------------------------------------------------------------------------##

# 0.Configure Workspace ----
# update path as needed
source("/home/bxhorn/Dropbox/Trading/R_Projects/NearbyPrices/config/Config_Trading.R")

##----------------------------------------------------------------------------------------##

# 1.API Category Query
# unfold the data hierarchy by layer

# petroleum
browse.EIA(cat.ID = 714755, key = key)
# summary
browse.EIA(cat.ID = 714756, key = key)
# weekly supply estimates
browse.EIA(cat.ID = 235079, key = key)
# by data sereis
browse.EIA(cat.ID = 235678, key = key)

# extract stock level data only and get bulk IDs
stock.ids <- browse.EIA(cat.ID = 235678, key = key)[33:69, 1]

stock.ids %>%
     map(function(x) browse.EIA(cat.ID = x, key = key)) %>%


     id.list <- tibble()
for (i in stock.ids) {
     temp.dat <- browse.EIA(cat.ID = i, key = key)
     id.list <- rbind(id.list, temp.dat)
}

1:10 %>%
     map(function(x) rnorm(10, x))
