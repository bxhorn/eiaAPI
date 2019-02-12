suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(RJSONIO))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(ggtree))
suppressPackageStartupMessages(library(gridExtra))
options(tibble.print_max = 5, tibble.print_min = 5)
options(tibble.width = NULL)
key <- "702dd5c99ca504ffa0c10e479e34f234"
##-------------------------------------------------------------------------------------------##

# treeEIA ----

depth <- 10
current <- 1
tree <- list()

if (current == 1 & current <= depth) {
     tree$level1 <- browse.EIA(371, key, TRUE) %>%
          mutate(parent.0 = "EIA Data Sets")
     current <- current + 1
}

if (current == 2 & current <= depth) {
     tree$level2 <- tree$level1$category_id %>%
          map(function(x) browse.EIA(cat.ID = x, key = key)) %>%
          set_names(tree$level1$name) %>%
          list(.,
               parent.0 = tree$level1$parent.0[1],
               parent.1 = tree$level1$name) %>%
          pmap(., cbind, stringsAsFactors = FALSE) %>%
          map(function(x) as_tibble(x))
     current <- current + 1
}

if (current == 3 & current <= depth) {
     tree$level3 <- list()
     for (i in seq.int(length(tree$level2))) {
          temp.level <- tree$level2[[i]]
          temp.tree <- temp.level$category_id %>%
               map(function(x) browse.EIA(cat.ID = x, key = key)) %>%
               set_names(temp.level$name) %>%
               list(.,
                    parent.0 = tree$level2[[i]]$parent.0[1],
                    parent.1 = tree$level2[[i]]$parent.1[1],
                    parent.2 = tree$level2[[i]]$name) %>%
               pmap(., cbind, stringsAsFactors = FALSE) %>%
               map(function(x) as_tibble(x))
          tree$level3 <- append(tree$level3, temp.tree)
     }
     # categories only - remove queery results that include data.series
     index <- as.vector(which(t(sapply(tree$level3, FUN = function(x) dim(x)))[, 2] == 8))
     tree$level3 <- tree$level3[-index]
     current <- current + 1
}


if (current == 4 & current <= depth) {
     tree$level4 <- list()
     for (i in seq.int(length(tree$level3))) {
          temp.level <- tree$level3[[i]]
          temp.tree <- temp.level$category_id %>%
               map(function(x) browse.EIA(cat.ID = x, key = key)) %>%
               set_names(temp.level$name) %>%
               list(.,
                    parent.0 = tree$level3[[i]]$parent.0[1],
                    parent.1 = tree$level3[[i]]$parent.1[1],
                    parent.2 = tree$level3[[i]]$parent.2[1],
                    parent.3 = tree$level3[[i]]$name) %>%
               pmap(., cbind, stringsAsFactors = FALSE) %>%
               map(function(x) as_tibble(x))
          tree$level4 <- append(tree$level4, temp.tree)
     }
     index <- as.vector(which(t(sapply(tree$level4, FUN = function(x) dim(x)))[, 2] == 9))
     tree$level4 <- tree$level4[-index]
     current <- current + 1
}

if (current == 5 & current <= depth) {
     tree$level5 <- list()
     for (i in seq.int(length(tree$level4))) {
          print(i)
          temp.level <- tree$level4[[i]]
          temp.tree <- temp.level$category_id %>%
               map(function(x) browse.EIA(cat.ID = x, key = key)) %>%
               set_names(temp.level$name) %>%
               list(.,
                    parent.0 = tree$level4[[i]]$parent.0[1],
                    parent.1 = tree$level4[[i]]$parent.1[1],
                    parent.2 = tree$level4[[i]]$parent.2[1],
                    parent.3 = tree$level4[[i]]$parent.3[1],
                    parent.4 = tree$level4[[i]]$name) %>%
               pmap(., cbind, stringsAsFactors = FALSE) %>%
               map(function(x) as_tibble(x))
          tree$level5 <- append(tree$level5, temp.tree)
     }
     index <- as.vector(which(t(sapply(tree$level5, FUN = function(x) dim(x)))[, 2] == 10))
     tree$level5 <- tree$level5[-index]
     current <- current + 1
}

if (current == 6 & current <= depth) {
     tree$level6 <- list()
     for (i in seq.int(length(tree$level5))) {
          print(i)
          temp.level <- tree$level5[[i]]
          temp.tree <- temp.level$category_id %>%
               map(function(x) browse.EIA(cat.ID = x, key = key)) %>%
               set_names(temp.level$name) %>%
               list(.,
                    parent.0 = tree$level5[[i]]$parent.0[1],
                    parent.1 = tree$level5[[i]]$parent.1[1],
                    parent.2 = tree$level5[[i]]$parent.2[1],
                    parent.3 = tree$level5[[i]]$parent.3[1],
                    parent.4 = tree$level5[[i]]$parent.4[1],
                    parent.5 = tree$level5[[i]]$name) %>%
               pmap(., cbind, stringsAsFactors = FALSE) %>%
               map(function(x) as_tibble(x))
          tree$level6 <- append(tree$level6, temp.tree)

          if (i == 400 | 800 | 1200 | 1600 | 2000 | 2400) {
               index <- as.vector(which(t(sapply(tree$level6, FUN = function(x) dim(x)))[, 2] == 11))
               tree$level6 <- tree$level6[-index]
               #cache.name <- paste0("tree_level6_", i, ".RData")
               #save("tree$level6", file = paste0(getwd(), cache.name))
               #tree$level6 <- list()
          }
     }
     current <- current + 1
}

# Junk ----





