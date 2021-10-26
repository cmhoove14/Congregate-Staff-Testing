
library(triangle)
library(tidyverse)
library(patchwork)

source(here::here("R/Utils.R"))

# Generate master schedule -----------
schedule_shifts <- rep(c("M", "E", "N"), times = days)
schedule_days   <- rep(rep(c("U", "M", "T", "W", "R", "F", "S"), each = 3), times = ceiling(days/7))[1:length(schedule_shifts)]
master_schedule <- paste(schedule_days, schedule_shifts, sep = "_")

# Worker schedule example --------------
set.seed(430)

# Randomly determine if worker works morning, night or day shift with equal probability  
draw_shift <- runif(1,0,1)
work_shift <- if_else(draw_shift < 1/3, "M",
                      if_else(draw_shift > 2/3, "N", "E"))

# Randomly determine weekly schedule from four options: 1)Sat-Tues, 2)Tues-Sat,3)Thurs-Sun, 4)Mon-Thurs with eaul probability
draw_days <- runif(1,0,1)
work_days <- if_else(draw_days < 1/4, "SUMT",
                     if_else(draw_days > 3/4, "MTWR", 
                             if_else(draw_days > 1/4 & draw_days < 2/4, "TWRF", "RFSU")))

# Binary work schedule
shift10 <- if_else(work_shift == schedule_shifts, 1, 0)
work10  <- if_else(schedule_days %in% unlist(strsplit(work_days, "")), 1, 0)

schedule_fin <- if_else(shift10 + work10 == 2, 1, 0)

schedule_hourly <- rep(schedule_fin, each = 8)
schedule_hourly[1:10] <- 0 # Cheating. Don't want shift at beginning

# assign dates and exposure time ---------
date_seq <- seq.POSIXt(as.POSIXct("2020-10-02"), 
                       as.POSIXct("2020-10-31"),
                       by = "hour")[1:720]

exposure_time <- 124

# plot ------------

png(here::here("Plots/work_test_infect1.png"),
    width = 7, height = 7, units = "in", res = 200)

par(mfrow = c(3,1),
    mar = c(3,4,1,0.5),
    mgp = c(2,0.75,0))


plot(x = date_seq,
     y = schedule_hourly[1:length(date_seq)], type = "l",
     xlab = "",
     ylab = expression(I["w,t"]),
     xaxt = "n",
     cex.lab = 2,
     cex.axis = 1.5)
axis(side = 1, at = date_seq[seq(1, length(date_seq), by = 24)], 
     labels = weekdays(date_seq)[seq(1, length(date_seq), by = 24)])
abline(v = date_seq[exposure_time],
       lty = 2, col = "blue")
abline(v = date_seq[seq(min(which(schedule_hourly==1)), 720, by = 7*24)],
       lty = 2, col = "green")
legend("topleft",
       legend = c("Infect",
                  "Test"),
       lty = 2,
       col = c("blue", "green"),
       bty = "n")

examp_infectious <- infectious_profile(4, 5.5, 8.5, 1/24)
examp_infectious_expand <- c(rep(0,exposure_time),
                             examp_infectious,
                             rep(0, (length(date_seq)-exposure_time-length(examp_infectious))))

plot(x = date_seq,
     y = examp_infectious_expand, type = "l", col = "blue",
     xlab = "",
     ylab = expression(beta[t]),
     xaxt = "n",
     cex.lab = 2,
     cex.axis = 1.5)
axis(side = 1, at = date_seq[seq(1, length(date_seq), by = 24)], 
     labels = weekdays(date_seq)[seq(1, length(date_seq), by = 24)])
abline(v = date_seq[exposure_time],
       lty = 2, col = "blue")
abline(v = date_seq[seq(min(which(schedule_hourly==1)), 720, by = 7*24)],
       lty = 2, col = "green")


plot(x = date_seq,
     y = examp_infectious_expand*schedule_hourly[1:length(date_seq)], 
     type = "l", col = "blue", lwd = 2,
     xlab = "Time (hourly)",
     ylab = expression(beta[t]~I["w,t"]),
     xaxt = "n",
     cex.lab = 2,
     cex.axis = 1.5)
axis(side = 1, at = date_seq[seq(1, length(date_seq), by = 24)], 
     labels = weekdays(date_seq)[seq(1, length(date_seq), by = 24)])
abline(v = date_seq[exposure_time],
       lty = 2, col = "blue")
abline(v = date_seq[seq(min(which(schedule_hourly==1)), 720, by = 7*24)],
       lty = 2, col = "green")

dev.off()


# plot with alternate infection schedule ------------
exposure_time2 <- exposure_time + 48

png(here::here("Plots/work_test_infect2.png"),
    width = 7, height = 7, units = "in", res = 200)

par(mfrow = c(3,1),
    mar = c(3,4,1,0.5),
    mgp = c(2,0.75,0))


plot(x = date_seq,
     y = schedule_hourly[1:length(date_seq)], type = "l",
     xlab = "",
     ylab = expression(I["w,t"]),
     xaxt = "n",
     cex.lab = 2,
     cex.axis = 1.5)
axis(side = 1, at = date_seq[seq(1, length(date_seq), by = 24)], 
     labels = weekdays(date_seq)[seq(1, length(date_seq), by = 24)])
abline(v = date_seq[exposure_time2],
       lty = 2, col = "red")
abline(v = date_seq[seq(min(which(schedule_hourly==1)), 720, by = 7*24)],
       lty = 2, col = "green")
legend("topleft",
       legend = c("Infect",
                  "Test"),
       lty = 2,
       col = c("red", "green"),
       bty = "n")

examp_infectious <- infectious_profile(4, 5.5, 8.5, 1/24)
examp_infectious_expand2 <- c(rep(0,exposure_time2),
                             examp_infectious,
                             rep(0, (length(date_seq)-exposure_time2-length(examp_infectious))))

plot(x = date_seq,
     y = examp_infectious_expand2, type = "l", col = "red",
     xlab = "",
     ylab = expression(beta[t]),
     xaxt = "n",
     cex.lab = 2,
     cex.axis = 1.5)
axis(side = 1, at = date_seq[seq(1, length(date_seq), by = 24)], 
     labels = weekdays(date_seq)[seq(1, length(date_seq), by = 24)])
abline(v = date_seq[exposure_time2],
       lty = 2, col = "red")
abline(v = date_seq[seq(min(which(schedule_hourly==1)), 720, by = 7*24)],
       lty = 2, col = "green")


plot(x = date_seq,
     y = examp_infectious_expand2*schedule_hourly[1:length(date_seq)], 
     type = "l", col = "red", lwd = 2,
     xlab = "Time (hourly)",
     ylab = expression(beta[t]~I["w,t"]),
     xaxt = "n",
     cex.lab = 2,
     cex.axis = 1.5)
axis(side = 1, at = date_seq[seq(1, length(date_seq), by = 24)], 
     labels = weekdays(date_seq)[seq(1, length(date_seq), by = 24)])
abline(v = date_seq[exposure_time2],
       lty = 2, col = "red")
abline(v = date_seq[seq(min(which(schedule_hourly==1)), 720, by = 7*24)],
       lty = 2, col = "green")

dev.off()
