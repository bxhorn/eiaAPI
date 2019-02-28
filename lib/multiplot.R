# Header ----
#
# Name:        multi_plot.R
#
# Title:       Tile multiple plots into one frame
#
# Version:     1.1
# Date:        2017-Jan-25
# Author:      Brad Horn
# License:     GPL (>=2)
#
# Description: Funtion script to tile multiple plots into one frame using the grid
#              package for base R.
#
# Details:     Function by Winston Chang. The function arguments are:
#
#                   ...            ggplot objects to be tiled
#                   plotlist       a list object with ggplot objects used as an
#                                  alternative to ...
#                   cols           integer or number of columns in the plot layout
#                   layout         matrix specifying the layout. If present, 'cols' is
#                                  ignored.
#
#              If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#              then plot 1 will go in the upper left, 2 will go in the upper right, and
#              3 will go all the way across the bottom on both the left and right.
#
#              multiplot(p1, p2, p3, p4, layout = matrix(c(1,2,3,4), byrow = TRUE))
#
# Dev.Notes:   N/A
#
# Depends      R programming language (3.5), grid package
#
# References:  http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
##-------------------------------------------------------------------------------------------##


multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {

     library(grid)

     # make a list from the "..." arguments and/or plotlist
     plots <- c(list(...), plotlist)
     numPlots = length(plots)

     # if layout = NULL, then use 'cols' to determine layout
     if (is.null(layout)) {
          # make the panel using ncol;
          # nrow is calculated from ncol
          layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                           ncol = cols,
                           nrow = ceiling(numPlots/cols))
     }
     if (numPlots == 1) {
          print(plots[[1]])

     } else {
          # set up the page
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

          # put each plot, in the correct location
          for (i in 1:numPlots) {
               # get the i,j matrix position of the subplot
               matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
               print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
          }
     }
}