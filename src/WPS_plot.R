# Header ----
#
# Name:         mapWPS.R
# Title:        map and get all data in the eia Weekly Petroleum Status Report
# Version:      1.0
# Date:         2019MFeb-12
# Author:       Brad Horn
# License:      GPL (>=2)
#
# Description:  Batch process the call to eia for the Weekly Petroleum Status Report
#               Get data and create plots for the WeeklyPetroleumStatusReport.Rmd.
#
# Details:      NA
#
# Dev.Notes:    NA
#
# Depends:      See the configuration script Config_CL.R in project config folder;
#               See source file mapWPS.R for core data inputs and tables.
#               See the custom functions eiaAPI for API interface functions.
#               API use requires a security key obtained through eia registration.
#
# References:   See https://www.eia.gov/opendata/register.php for API key registration
#               See https://www.eia.gov/opendata/commands.php for API command syntax
##----------------------------------------------------------------------------------------##

# 0.Configure Workspace ----
source("/home/bxhorn/Dropbox/Trading/R_Projects/eiaAPI/config/Config_CL.R")

# confirm latest data available
# call_eia("PET.WTESTUS1.W", key = key)
# test <- get("PET.WTESTUS1.W") %>%
#      tail(., n = 1) %>%
#      select(Date) %>%
#      mutate(Week = week(Date))
# test
##----------------------------------------------------------------------------------------##

# 1.US balances ----
# change in reported stocks
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

# distribution analysis of key changes
total_delta <- us.total.stocks.spr %>%
     filter(year >= 1990) %>%
     mutate(delta = us.total.stocks.spr - Lag(us.total.stocks.spr, k = 1))
cl_delta <- cl_commercial %>%
     filter(year >= 1990) %>%
     mutate(delta = cl_commercial - Lag(cl_commercial, k = 1))

total.delta.point <- tail(total_delta, n = 1)$delta / 1e3
cl.delta.point <- tail(cl_delta, n = 1)$delta / 1e3

delta1 <- ggplot(total_delta, aes(x = delta / 1e3)) +
     geom_histogram(bins = 50, color = "green", fill = "black", alpha = 0.50) +
     geom_vline(aes(xintercept = mean(delta / 1e3, na.rm = TRUE)),
                linetype = "dashed", size = 0.6, color = "black") +
     geom_vline(aes(xintercept = total.delta.point),
                linetype = "dashed", size = 1, color = "red") +
     labs(title = "Change in Total Stocks",
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Million Barrels (Bbls)",
          y = "Frequency since 1990") +
     theme.Dat +
     theme(axis.text.x = element_text(size = 12, color = "grey15", angle = 0, vjust = 0.5))

delta2 <- ggplot(cl_delta, aes(x = delta / 1e3)) +
     geom_histogram(bins = 50, color = "green", fill = "black", alpha = 0.50) +
     geom_vline(aes(xintercept = mean(delta / 1e3, na.rm = TRUE)),
                linetype = "dashed", size = 0.6, color = "black") +
     geom_vline(aes(xintercept = cl.delta.point),
                linetype = "dashed", size = 1, color = "red") +
     labs(title = "Change in Commercial Crude Stocks",
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Million Barrels (Bbls)",
          y = "Frequency since 1990") +
     theme.Dat +
     theme(axis.text.x = element_text(size = 12, color = "grey15", angle = 0, vjust = 0.5))

home <- getwd()
setwd(image.path)
png(file = "stock_changes.png", width = 950, height = 425)
multiplot(delta1, delta2, layout = matrix(c(1,2), nrow = 1, byrow = TRUE))
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
temp.max <- ceiling(max(tbl4$change) / 1e3) * 1e3
temp.min <- floor(min(tbl4$change) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

home <- getwd()
setwd(image.path)
png(file = "cl_stockchange.png", width = 950, height = 425)
ggplot(tbl4, aes(x = Crude.Stocks, y = change)) +
     geom_bar(stat = "identity", fill = "black", color = "black", size = 1.25,
              alpha = 0.4) +
     geom_hline(aes(yintercept = 0), linetype = "solid", size = 1, color = "red") +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     labs(title = paste0("Crude Oil Stocks: W-o-W Changes"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           axis.text.x = element_text(size = 12, color = "grey15", face = "bold",
                                      angle = 0, vjust = 0.5))
dev.off()
setwd(home)


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
temp.max <- ceiling(max(tbl5$change) / 1e3) * 1e3
temp.min <- floor(min(tbl5$change) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10
home <- getwd()
setwd(image.path)
png(file = "rb_stockchange.png", width = 950, height = 425)
ggplot(tbl5, aes(x = Gasoline.Stocks, y = change)) +
     geom_bar(stat = "identity", fill = "black", color = "black", size = 1.25,
              alpha = 0.4) +
     geom_hline(aes(yintercept = 0), linetype = "solid", size = 1, color = "red") +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     labs(title = paste0("Gasoline Stocks: W-o-W Changes"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           axis.text.x = element_text(size = 12, color = "grey15", face = "bold",
                                      angle = 0, vjust = 0.5))
dev.off()
setwd(home)


temp.var <- rb_padd1b
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

home <- getwd()
setwd(image.path)
png(file = "rb_padd1b.png", width = 950, height = 425)
ggplot(temp.dat, aes(x = week, y = rb_padd1b, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = rb_padd1b), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("Gasoline Stocks: PADD1b New York Harbor"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank())
dev.off()
setwd(home)

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
temp.max <- ceiling(max(tbl6$change) / 1e3) * 1e3
temp.min <- floor(min(tbl6$change) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10
home <- getwd()
setwd(image.path)
png(file = "ho_stockchange.png", width = 950, height = 425)
ggplot(tbl6, aes(x = Distillate.Stocks, y = change)) +
     geom_bar(stat = "identity", fill = "black", color = "black", size = 1.25,
              alpha = 0.4) +
     geom_hline(aes(yintercept = 0), linetype = "solid", size = 1, color = "red") +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     labs(title = paste0("Distillate Stocks: W-o-W Changes"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank(),
           axis.text.x = element_text(size = 12, color = "grey15", face = "bold",
                                      angle = 0, vjust = 0.5))
dev.off()
setwd(home)


temp.var <- ho_padd1b
temp.dat <- temp.var %>%
     filter(year >= 2014)
temp.dat.19 <- temp.var %>%
     filter(year == 2019)
temp.max <- ceiling(max(temp.dat[, 2]) / 1e3) * 1e3
temp.min <- floor(min(temp.dat[, 2]) / 1e3) * 1e3
temp.step <- (temp.max - temp.min) / 10

home <- getwd()
setwd(image.path)
png(file = "ho_padd1b.png", width = 950, height = 425)
ggplot(temp.dat, aes(x = week, y = ho_padd1b, group = year, color = as.factor(year))) +
     geom_line() +
     geom_point(aes(shape = as.factor(year)), size = 1.5) +
     geom_point(data = temp.dat.19, aes(x = week, y = ho_padd1b), color = "black", size = 2.0) +
     scale_y_continuous(labels = comma,
                        limits = c(temp.min, temp.max),
                        breaks = seq(temp.min, temp.max, by = temp.step)) +
     scale_x_continuous(breaks = seq(0, 52, 4)) +
     scale_color_manual(values = temp.color) +
     scale_shape_manual(values = c(0, 2, 5, 6, 8, 16)) +
     labs(title = paste0("Distillate Stocks: PADD1b New York Harbor"),
          subtitle = temp.subtitle,
          caption = "Data from eia using OpenDat API",
          x = "Production Week",
          y = "Thousand barrels (000 Bbls)") +
     theme.Dat +
     theme(legend.title = element_blank())
dev.off()
setwd(home)


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
     labs(title = paste0("US Distillate Storage: PADD-1"),
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
     labs(title = paste0("US Distillate Storage: PADD-2"),
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
     labs(title = paste0("US Distillate Storage: PADD-3"),
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
     labs(title = paste0("US Distillate Storage: PADD-4"),
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
     labs(title = paste0("US Distillate Storage: PADD-5"),
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