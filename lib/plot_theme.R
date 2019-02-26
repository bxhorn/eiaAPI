# Header ----
#
# Name:        plot_theme.R
#
# Title:       Base plot theme for data visualization
#
# Version:     1.1
# Date:        2017-Jan-25
# Author:      Brad Horn
# License:     GPL (>=2)
#
# Description: A base theme is defined for data analysis that relies on Google Docs
#              design attributes.
#
# Details:     The script is used with the Grammar of Graphics package ggplot2. The
#              function is called using the function theme.Dat as follows:
#
#              ggplot(x, aes(x, y)) +
#              geom_line() +
#              theme.Dat
#
# Dev.Notes:   N/A
#
# Depends      R programming language (3.5), the ggplot2 and ggthemes packages
#
# References:  N/A
##-------------------------------------------------------------------------------------------##


theme.Dat <- theme_gdocs() +
     theme(plot.title = element_text(size = 15, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 12, hjust = 0),
           plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(face = "bold", color = "gray30"),
           axis.title.x = element_text(face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           panel.background = element_rect(fill = "grey98", colour = "grey75"),
           panel.border = element_rect(colour = "white"),
           panel.grid.major.y = element_line(colour = "grey90"),
           panel.grid.minor.y = element_line(colour = "grey95"),
           panel.grid.major.x = element_line(colour = "grey90"),
           panel.grid.minor.x = element_line(colour = "grey98"),
           strip.background = element_rect(fill = "white", colour = "grey75"),
           strip.text.y = element_text(face = "bold"),
           axis.line = element_line(colour = "grey75"))
