## ## Validating submersion time
# Selina Cheng
rm(list=ls())

library(tidyverse)
library(data.table)
library(lubridate)
library(zoo)
library(readxl)
library(suntools)
library(plyr)
library(plotly)

source("functions.R")

# Read in intertidal data
# Note: this is 15 min data
# TZ is UTC
intertidal_dat <- read_excel(list.files("data", pattern = "bare 2025", recursive = F, full.names = T))

names(intertidal_dat) <- c("row_id", "time", "temp", "light")

subtidal_dat <- read_excel(list.files("data", pattern = "subtidal 2025", recursive = F, full.names = T))

names(subtidal_dat) <- c("row_id", "time", "temp", "light")

# Add logger
intertidal_dat$logger <- "intertidal_bare"
subtidal_dat$logger <- "subtidal"

# Combine data
temp_dat <- rbind(intertidal_dat, subtidal_dat)

# Change TZ to EST
temp_dat <- temp_dat %>%
  select(-row_id, -light)

temp_dat$timestamp_est <- temp_dat$time - 3600

tz(temp_dat$timestamp_est) <- ("America/Jamaica")

# Rename and reorder cols
temp_dat <- temp_dat %>%
  dplyr::rename(temp_c = temp, timestamp_edt = time) %>%
  select(timestamp_est, timestamp_edt, logger, temp_c)

# Let's just work with Sept data that we have some validated tides for
temp_dat <- temp_dat %>%
  filter(timestamp_est >= ymd_hms("2025-09-17 00:00:00", tz = "EST"))

# ----- Detect submersion ---
# Pivot wider to make it easier for ourselves...
temp_dat_new <- pivot_wider(temp_dat, id_cols = c("timestamp_est"),
                            names_from = logger, values_from = temp_c,
                            names_prefix = "temp_c_")

# Let's try a differencing method
# Create diffs column
temp_dat_new <- temp_dat_new %>%
  # First create first order differences for intertidal temp
  mutate(bare_diffs = c(0,diff(temp_c_intertidal_bare)),
         # Also look at difference between intertidal and water temp
         resid_subtidal = temp_c_intertidal_bare - temp_c_subtidal)

# If first order differences in intertidal temp are >= 0.5 C, flag it
# If difference between intertidal and water temp are >= 1 C, flag it
# DEPENDING ON FREQUENCY OF DATA COLLECTION, YOUR DIFF THRESHOLD WILL BE DIFFERENT
temp_dat_new <- temp_dat_new %>%
  mutate(flag = ifelse(abs(bare_diffs) >= 0.5 | abs(resid_subtidal) >= 1, "exposed", "submerged"))

# Plot data so far
# Plotted data with flags and also the diffs
# The green dotted line shows the threshold for diffs
ggplot(data = temp_dat_new)+
  geom_point(aes(x = timestamp_est, y= temp_c_intertidal_bare, color = flag), alpha = 0.6)+
  geom_line(aes(x = timestamp_est, y = temp_c_intertidal_bare), color = "black", alpha = 0.3)+
  geom_point(aes(x = timestamp_est, y = bare_diffs), color = "red", alpha = 0.7)+
  geom_hline(yintercept= c(0.5, -0.5), linetype = "dashed", color = "green")

# this actually looks pretty good
# There are a couple times where we don't have intertidal temp so let's remove that
temp_dat_new <- temp_dat_new %>%
  filter(!is.na(temp_c_intertidal_bare))

# Next step: 
# interpolate data to 1 min and fill in NAs with exposed flag
temp_dat_new <- linear_interp(df = temp_dat_new, "timestamp_est", "1 min", 
                              c("temp_c_intertidal_bare", "temp_c_subtidal"))

# fill in NAs created by 1 min linear interpolation with "exposed" flag using na.locf function.
# fills in NAs using the method: "Last Observation Carried Forward" 
temp_dat_test <- temp_dat_new %>%
  mutate(flag = na.locf(flag, fromLast = T))

# Look at the data
ggplot(data = temp_dat_test)+
  geom_point(aes(x = timestamp_est, y= temp_c_intertidal_bare, color = flag), alpha = 0.6)+
  geom_line(aes(x = timestamp_est, y = temp_c_intertidal_bare), color = "black", alpha = 0.3)
# geom_point(aes(x = timestamp_est, y = bare_diffs), color = "red", alpha = 0.7)+
# geom_hline(yintercept= c(0.3, -0.3), linetype = "dashed", color = "green")

# Fix RLEs for temp_dat_test
# see rle_status in functions.R script for details on function.
temp_dat_test  <- rle_status(df = temp_dat_test, status_colname = "flag", 
                             status_select = "exposed", min_threshold = 16)

# Again, now for submerged
temp_dat_test  <- rle_status(df = temp_dat_test, status_colname = "new_status", 
                             status_select = "submerged", min_threshold = 180, 
                             drop = "first")

# Look at the data
ggplot(data = temp_dat_test)+
  geom_point(aes(x = timestamp_est, y= temp_c_intertidal_bare, color = new_status), alpha = 0.6)+
  geom_line(aes(x = timestamp_est, y = temp_c_intertidal_bare), color = "black", alpha = 0.3)+
  geom_line(aes(x = timestamp_est, y = temp_c_subtidal), color = "blue", alpha = 0.3)

# ---- Fix end time for each "exposed" event ----
# This fixes the RH side of each spike

# Get time for end of each "exposed" event
# Let's do RLE
runs <- rle(temp_dat_test$new_status)
runs <- data.frame(status = runs$values, length = runs$lengths)

# Make submergence numeric
temp_dat_test$status_numeric <- ifelse(temp_dat_test$new_status == "submerged", 1, 0)
# See where status changes
temp_dat_test$change <- c(diff(temp_dat_test$status_numeric),0)
# Change rows is the row where the status ends
runs$change_rows <- c(which(temp_dat_test$change != 0), nrow(temp_dat_test))

# Filter to just exposed events
runs_exposed <- runs %>% filter(status == "exposed")

# Drop last row of runs_exposed
runs_exposed <- runs_exposed[-nrow(runs_exposed),]

for(n in 1:nrow(runs_exposed)){
  # Get first and last row of the exposure event
  first_row <- (runs_exposed$change_rows[n]-runs_exposed$length[n]+1)
  last_row <- (runs_exposed$change_rows[n])
  
  # Get just the exposed event
  exp_dat <- temp_dat_test[first_row:last_row,]
  
  # Get diffs in temperature
  exp_dat$diffs <- c(0,diff(exp_dat$temp_c_intertidal_bare))
  
  # get sign of diff
  exp_dat$sign <- sign(exp_dat$diffs)
  
  # Give a row ID
  row.names(exp_dat) <- NULL
  
  exp_dat$row_id <- row.names(exp_dat)
  
  # Filter out where sign is 0
  exp_dat <- exp_dat %>%
    filter(sign !=0)
  
  # Identify when sign changes
  exp_dat$change_sign <- c(0, diff(exp_dat$sign))
  
  # Get last sign change
  sign_changes <- data.frame(change_sign = exp_dat$change_sign, 
                             row_name = exp_dat$row_id)
  sign_changes <- sign_changes %>%
    filter(change_sign != 0)
  
  # This is row when the last sign change happens
  last_spike_row <- (first_row + 
                       as.numeric(sign_changes$row_name[nrow(sign_changes)])-1)
  
  # Correct for rate of change
  diffs <- diff(temp_dat_test$temp_c_intertidal_bare[last_spike_row:last_row])
  new_start <- which(abs(diffs) > 0.04)[1]
  last_spike_row <- last_spike_row + new_start - 1
  
  print(paste0("event ", n, "'s last spike row: ", last_spike_row))
  
  if((nrow(sign_changes) > 0)){
    # Change previously "exposed" rows to be "submerged"
    temp_dat_test$new_status[last_spike_row:last_row] <- "submerged"
  }
}

# Look at the data
ggplot(data = temp_dat_test)+
  geom_point(aes(x = timestamp_est, y= temp_c_intertidal_bare, color = new_status), alpha = 0.6)+
  geom_line(aes(x = timestamp_est, y = temp_c_intertidal_bare), color = "black", alpha = 0.3)+
  geom_line(aes(x = timestamp_est, y = temp_c_subtidal), color = "blue", alpha = 0.3)

##### ----- everything in the next section is just reading in submergence data and validating what you have ----
# Read in WL data
wl <- read_excel(list.files("data", pattern = "JEL sel", recursive = F, full.names = T))

# Format data....
names(wl) <- c("row_id", "time", "temp_c", "pressure_kpa")
wl$time <- floor_date(wl$time, unit = "minutes") #round to the minute

# Change to EST
wl$timestamp_est <- wl$time - 3600
tz(wl$timestamp_est) <- "America/Jamaica"

# More formatting
wl <- wl%>%
  dplyr::rename(timestamp_edt = time) %>%
  select(-row_id)

# Plot intertidal logger pressure
ggplot() +
  geom_point(data = wl, aes(x = timestamp_est, y = pressure_kpa)) 

# Code this data as submerged or exposed..
# Create diffs
wl$diffs <- c(0, diff(wl$pressure_kpa))

# Remove first 6 rows of wl for ease..
wl <- wl[-c(1:6),]

# Identify submersion times in pressure data
wl <- wl %>%
  mutate(flag = ifelse((abs(diffs) > 0.1) | pressure_kpa > 110, "submerged", "exposed"))

ggplot() +
  geom_point(data = wl, aes(x = timestamp_est, y = pressure_kpa, color = flag))

# Use rle status
wl <- rle_status(df = wl, status_colname = "flag", status_select = "exposed",
                 min_threshold = 3)
# again
wl <- rle_status(df = wl, status_colname = "new_status", status_select = "submerged",
                 min_threshold = 10)

ggplot() +
  geom_point(data = wl, aes(x = timestamp_est, y = pressure_kpa, color = new_status))

# ID when runs of false start and end
rle_submersion <- rle(wl$new_status)
runs <- data.frame(status = rle_submersion$values, length = rle_submersion$lengths)

# Make submergence numeric
wl$status_numeric <- ifelse(wl$new_status == "exposed", 1, 0)
# See where status changes
wl$change <- c(diff(wl$status_numeric),0)
# Change rows is the row where the status ends
runs$change_rows <- c(which(wl$change != 0), nrow(wl))
runs$timestamp_est <- wl$timestamp_est[runs$change_rows]

# Drop last row of runs time because it actually just marks the end of the ts
runs <- runs[-nrow(runs),]

# If end of true, out of water
# If end of false, in water
runs <- runs %>%
  mutate(label = ifelse(status == "exposed", "in water", "out of water"))

# in water times
in_water_vector <- runs %>% 
  filter(label == "in water") %>%
  pull(timestamp_est)

oow_vector <- runs %>%
  filter(label == "out of water") %>%
  pull(timestamp_est)

wl_runs <- runs

submergence <- data.frame(start_submergence = in_water_vector, end_submergence = oow_vector)

# Mark end of TRUEs and end of FALSEs
ggplot() +
  geom_point(data = wl, aes(x = timestamp_est, y = pressure_kpa, color = new_status))+
  geom_rect(data = submergence, aes(xmin = start_submergence, xmax = end_submergence, 
                                    ymin=-Inf,ymax = Inf), alpha = 0.3, fill = "#8AD2FF")

### 
# Look at the data
ggplot(data = temp_dat_test)+
  geom_point(aes(x = timestamp_est, y= temp_c_intertidal_bare, color = new_status), alpha = 0.6)+
  geom_line(aes(x = timestamp_est, y= temp_c_subtidal),color="blue", alpha = 0.6)+
  geom_line(aes(x = timestamp_est, y = temp_c_intertidal_bare), color = "black", alpha = 0.3)+
  geom_vline(data=  wl_runs, aes(xintercept = timestamp_est, color = label), linetype = "dashed", 
             alpha = 0.5)+
  geom_rect(data = submergence, aes(xmin = start_submergence, xmax = end_submergence, 
                                    ymin=-Inf,ymax = Inf), alpha = 0.3, fill = "#8AD2FF")+
  # Filter to specific time frame
  scale_x_datetime(
    # breaks = date_breaks("2 hours"),
    limits = c(ymd_hms("2025-09-16 03:00:00", tz = "EST"),
               ymd_hms("2025-09-20 00:00:00", tz = "EST")))


# Get start and end time of each submergence period from the temp data and compare from the water level data.
runs <- rle(temp_dat_test$new_status)
runs <- data.frame(status = runs$values, length = runs$lengths)

# Make submergence numeric
temp_dat_test$status_numeric <- ifelse(temp_dat_test$new_status == "submerged", 1, 0)
# See where status changes
temp_dat_test$change <- c(diff(temp_dat_test$status_numeric),0)
# Change rows is the row where the status ends
runs$change_rows <- c(which(temp_dat_test$change != 0), nrow(temp_dat_test))
runs$timestamp_est_end <- temp_dat_test$timestamp_est[runs$change_rows]

# Filter to just submerged events
runs_submerged <- runs %>% filter(status == "submerged")
runs_submerged$start_row <- runs_submerged$change_rows - runs_submerged$length + 1
runs_submerged$timestamp_est_start <- temp_dat_test$timestamp_est[runs_submerged$start_row]

submergence_wl <- submergence
submergence_temp <- runs_submerged %>% select(timestamp_est_start, timestamp_est_end)

# Drop the first row of submergence temp, which isn't in the submergence data
submergence_temp <- submergence_temp[-1,]
# Drop last row of submergence temp, which is an erroneous assignment
submergence_temp <- submergence_temp[-nrow(submergence_temp),]

# Let's floor time to the nearest 12 hour mark..
submergence_wl <- submergence_wl %>%
  mutate(start_floor = floor_date(start_submergence, "12 hours"),
         end_floor = floor_date(end_submergence, "12 hours"))

submergence_temp <- submergence_temp %>%
  mutate(start_floor = floor_date(timestamp_est_start, "12 hours"),
         end_floor = floor_date(timestamp_est_end, "12 hours"))

# Join submergence 
# Actually rename cols
submergence_wl <- submergence_wl %>%
  dplyr::rename(wl_start = start_submergence,
                wl_end = end_submergence)

submergence_temp <- submergence_temp %>%
  dplyr::rename(temp_start = timestamp_est_start,
                temp_end = timestamp_est_end)

submergence_tot <- full_join(submergence_wl, submergence_temp, by = c("start_floor", "end_floor"))

# Create WL adjustment based on elevation
# 5-10 min difference in tide arrival time
submergence_tot <- submergence_tot %>%
  mutate(wl_start_adjust = wl_start + (10*60),
         wl_end_adjust = wl_end - (10*60))

# Calculate differences in start and end time between loggers
submergence_tot <- submergence_tot %>%
  mutate(start_diff = difftime(temp_start, wl_start_adjust, units = "min"),
         end_diff = difftime(temp_end, wl_end_adjust, units = "min"))

## Plot data
p <- ggplot()+
  geom_line(data = temp_dat_test, aes(x = timestamp_est, y = temp_c_subtidal, color = "subtidal"), alpha = 1)+
  geom_point(data = temp_dat_test, aes(x = timestamp_est, y = temp_c_intertidal_bare, color = new_status)) +
  geom_line(data = temp_dat_test, aes(x = timestamp_est, y = temp_c_intertidal_bare), color = "black", alpha = 0.5)+
  geom_vline(data=  submergence_tot, aes(xintercept = as.numeric(wl_start)), color = "blue", linetype = "dashed",
             alpha = 0.5) +
  geom_vline(data = submergence_tot, aes(xintercept = as.numeric(wl_end)), color = "red", linetype = "dashed",
             alpha = 0.5)+
  geom_vline(data = submergence_tot, aes(xintercept = as.numeric(wl_start_adjust), linetype = "water level"),
             color = "black", alpha = 0.5)+
  geom_vline(data = submergence_tot, aes(xintercept = as.numeric(wl_end_adjust), linetype = "water level"),
             color = "black", alpha = 0.5)+
  scale_color_manual(values = c("blue", "#F8766D", "#00BFC4"),
                     breaks = c("subtidal", "exposed", "submerged"),
                     labels = c("subtidal", "intertidal exposed", "intertidal submerged"), guide = "legend")+
  scale_linetype_manual(values = c("dashed"),
                        breaks = c("water level"),
                        labels = c("water level logger"), guide = "legend")+
  labs(title = "Red and blue vertical lines are times from water level logger. Black vertical lines are adjusted for elevation", x= "Temperature", y = "Timestamp (EST)")

ggplotly(p)

# Average time difference from water level logger in exposure event
cat(paste0("Average time diff between WL logger (actual time, sorta) and temp logger (algorithmically detected) for EXPOSURE events: ", round(mean(submergence_tot$end_diff, na.rm=T), 2), " mins, \nwith ", 
length(submergence_tot$end_diff[!is.na(submergence_tot$end_diff)]), " detection events."))

# Average time difference from water level logger in submergence event
cat(paste0("Average time diff between WL logger (actual time, sorta) and temp logger (algorithmically detected) for SUBMERGENCE events: ", round(mean(submergence_tot$start_diff, na.rm=T), 2), " mins, \nwith ", 
           length(submergence_tot$start_diff[!is.na(submergence_tot$start_diff)]), " detection events."))

# Average time difference from water level logger in start or end of events
total <- c(submergence_tot$end_diff, submergence_tot$start_diff)

cat(paste0("Average time diff between WL logger (actual time, sorta) and temp logger (algorithmically detected) for ANY events: ", round(mean(total, na.rm= T), 2), " mins, \nwith ", 
           length(total[!is.na(total)]), " detection events."))






