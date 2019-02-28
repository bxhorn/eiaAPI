# Header ----
#
# Name:         mapWPS.R
# Title:        map and get all data in the eia Weekly Petroleum Status Report
# Version:      1.0
# Date:         2019MFeb-12
# Author:       Brad Horn
# License:      GPL (>=2)
#
# Description:  Map each element in the WPS report to data series IDs. Use the IDs
#               to get the series data from eia.  Data includes production, storage
#               stocks and spot prices.  Data is transformed for tidy work flow and
#               the creation of WPS summary plots.
#
# Details:      NA
#
# Dev.Notes:    NA
#
# Depends:      See the configuration script Config_CL.R in project config folder;
#               See source file eiaWPS.R for core data inputs and tables.
#               See the custom functions eiaAPI for API interface functions. API use
#               requires a security key, which is a key function input.
#
# References:   See https://www.eia.gov/opendata/register.php for API key registration
#               See https://www.eia.gov/opendata/commands.php for API command syntax
##----------------------------------------------------------------------------------------##

# 0.Configure Workspace ----
# update path as needed
source("/home/bxhorn/Dropbox/Trading/R_Projects/eiaAPI/config/Config_CL.R")

# 1.US balances ----
temp.var <- us.total.stocks
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.color <- c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#000000")
temp.max <- ceiling(max(temp.dat[, 2]) / 5e4) * 5e4
temp.min <- floor(min(temp.dat[, 2]) / 5e4) * 5e4
temp.step <- (temp.max - temp.min) / 10
temp.subtitle <- paste0("Week ", as.character(tail(temp.dat.19, n = 1)$week), "   ",
                        tail(temp.dat.19, n = 1)$date)

p1 <- ggplot(temp.dat, aes(x = week, y = us.total.stocks, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = us.total.stocks), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Total Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_commercial
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 5e4) * 5e4
temp.min <- floor(min(temp.dat[, 2]) / 5e4) * 5e4
temp.step <- (temp.max - temp.min) / 10
p2 <- ggplot(temp.dat, aes(x = week, y = cl_commercial, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_commercial), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- rb_total
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 5e3) * 5e3
temp.min <- floor(min(temp.dat[, 2]) / 5e3) * 5e3
temp.step <- (temp.max - temp.min) / 10
p3 <- ggplot(temp.dat, aes(x = week, y = rb_total, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = rb_total), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- ho_total
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 5e3) * 5e3
temp.min <- floor(min(temp.dat[, 2]) / 5e3) * 5e3
temp.step <- (temp.max - temp.min) / 10
p4 <- ggplot(temp.dat, aes(x = week, y = ho_total, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = ho_total), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Distillate Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

home <- getwd()
setwd(image.path)
png(file = "petroleum_balances.png", width = 950, height = 900)
multiplot(p1, p2, p3, p4, layout = matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))
dev.off()
setwd(home)

# 2.US Crude oil S&D ----
temp.var <- cl_production
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p1a <- ggplot(temp.dat, aes(x = week, y = cl_production, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_production), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Production"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls) Per Day") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_imports %>%
     mutate(sma4 = SMA(cl_imports, n = 4))
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, "sma4"]) / 1e2) * 1e2
temp.min <- floor(min(temp.dat[, "sma4"]) / 1e2) * 1e2
temp.step <- (temp.max - temp.min) / 10

p1b <- ggplot(temp.dat, aes(x = week, y = sma4, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = sma4), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Imports"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls) Per Day") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_exports #%>%
#mutate(sma4 = SMA(cl_imports, n = 4))
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e2) * 1e2
temp.min <- floor(min(temp.dat[, 2]) / 1e2) * 1e2
temp.step <- (temp.max - temp.min) / 10

p1c <- ggplot(temp.dat, aes(x = week, y = cl_exports, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_exports), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Exports"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls) Per Day") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))


temp.var <- refinery.runs
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p1d <- ggplot(temp.dat, aes(x = week, y = refinery.runs, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = refinery.runs), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Crude Oil Refinery Runs"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls) Per Day") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

home <- getwd()
setwd(image.path)
png(file = "cl_balances.png", width = 950, height = 900)
multiplot(p1a, p1b, p1c, p1d, layout = matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))
dev.off()
setwd(home)

# 3.US Crude Oil Storage ----
temp.var <- cl_cushing
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

home <- getwd()
setwd(image.path)
png(file = "cl_cushing.png", width = 950, height = 425)
ggplot(temp.dat, aes(x = week, y = cl_cushing, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_cushing), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("Crude Oil Stocks: Cushing, OK"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank())
dev.off()
setwd(home)

temp.var <- cl_padd1
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p4b <- ggplot(temp.dat, aes(x = week, y = cl_padd1, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_padd1), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("PADD1 Crude Oil Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_padd2
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p4c <- ggplot(temp.dat, aes(x = week, y = cl_padd2, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_padd2), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("PADD2 Crude Oil Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_padd3
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p4d <- ggplot(temp.dat, aes(x = week, y = cl_padd3, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_padd3), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("PADD3 Crude Oil Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_padd4
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p4e <- ggplot(temp.dat, aes(x = week, y = cl_padd4, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_padd4), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("PADD4 Crude Oil Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- cl_padd5
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p4f <- ggplot(temp.dat, aes(x = week, y = cl_padd5, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = cl_padd5), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("PADD5 Crude Oil Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

home <- getwd()
setwd(image.path)
png(file = "cl_stocks.png", width = 950, height = 900)
multiplot(plotlist = list(p2, p4b, p4c, p4d, p4e, p4f), layout = matrix(c(1,2,3,4,5,6), nrow = 3, byrow = TRUE))
dev.off()
setwd(home)

# 4.US Gasoline Storage ----
temp.var <- rb_total
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p5a <- ggplot(temp.dat, aes(x = week, y = rb_total, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = rb_total), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- rb_padd1
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p5b <- ggplot(temp.dat, aes(x = week, y = rb_padd1, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = rb_padd1), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-1"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- rb_padd2
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p5c <- ggplot(temp.dat, aes(x = week, y = rb_padd2, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = rb_padd2), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-2"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- rb_padd3
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p5d <- ggplot(temp.dat, aes(x = week, y = rb_padd3, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = rb_padd3), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-3"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- rb_padd4
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p5e <- ggplot(temp.dat, aes(x = week, y = rb_padd4, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = rb_padd4), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-4"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- rb_padd5
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p5f <- ggplot(temp.dat, aes(x = week, y = rb_padd5, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = rb_padd5), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-5"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

home <- getwd()
setwd(image.path)
png(file = "rb_stocks.png", width = 950, height = 900)
multiplot(plotlist = list(p5a, p5b, p5c, p5d, p5e, p5f), layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE))
dev.off()
setwd(home)

# 5.US Distillate Storage ----
temp.var <- ho_total
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p6a <- ggplot(temp.dat, aes(x = week, y = ho_total, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = ho_total), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Stocks"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- ho_padd1
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p6b <- ggplot(temp.dat, aes(x = week, y = ho_padd1, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = ho_padd1), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-1"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- ho_padd2
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p6c <- ggplot(temp.dat, aes(x = week, y = ho_padd2, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = ho_padd2), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-2"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- ho_padd3
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p6d <- ggplot(temp.dat, aes(x = week, y = ho_padd3, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = ho_padd3), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-3"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- ho_padd4
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p6e <- ggplot(temp.dat, aes(x = week, y = ho_padd4, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = ho_padd4), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-4"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

temp.var <- ho_padd5
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

p6f <- ggplot(temp.dat, aes(x = week, y = ho_padd5, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = ho_padd5), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("US Gasoline Storage: PADD-5"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           plot.title = element_text(size = 13, color = "black", face = "bold", hjust = 0),
           plot.subtitle = element_text(size = 9, hjust = 0),
           plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(size = 10, face = "bold", color = "gray30"),
           axis.title.x = element_text(size = 10, face = "bold", color = "gray30", vjust = -0.25),
           axis.text.y = element_text(size = 9, color = "grey15"),
           axis.text.x = element_text(size = 9, color = "grey15", angle = -90, vjust = 0.5),
           legend.text = element_text(size = 9))

home <- getwd()
setwd(image.path)
png(file = "ho_stocks.png", width = 950, height = 900)
multiplot(plotlist = list(p6a, p6b, p6c, p6d, p6e, p6f), layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE))
dev.off()
setwd(home)