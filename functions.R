# Functions that I use
library(tidyverse)

# ------ MINI FUNCTIONS -------
mean_trim <- function(x){mean(x, trim = 0.25, na.rm = T)}

# --------- GGPLOT THEME ------------------
sel_theme <- function(){
  theme_classic() +
    theme(text = element_text(size = 6),
          axis.text.x = element_text(margin=margin(0.25,0.25,0.25,0.25, "cm"), 
                                     color = "black", size = 6),
          axis.text.y = element_text(margin=margin(0.25,0.25,0.25,0.25, "cm"), 
                                     color = "black", size = 6),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_line(color="black", linewidth = 0.5),
          axis.ticks = element_line(linewidth = 0.5, color = "black"),
          axis.ticks.length = unit(0.25, "cm"))
  #aspect.ratio = 1)
}

# -------------- RLE FUNCTION ---------------------
# Fix erroneous status assignments in a "status" column of your data frame
# This is useful if your status assignment code results in short blips of the wrong status assignment.

# Let's say you code your timeseries data using a binary status like "day/night". You would expect day and night statuses to be very long -- on the scale of several hours. For some reason, your code is mostly correct but has an error: some blips of points that you *know* should be coded "night" are actually coded "day". You could use this function to correct that and say: if there are any status assignments "day" that are less than 6 hours long, those assignments are erroneous and should be changed to "night."

# Function args:
      # df (data.frame) = your dataframe
      # "status_colname" (string) = column name that contains binary statuses
      # "status_select" (string) = status that you want to eliminate erroneous runs of
      #  min_threshold (numeric) = minimum number of expected values for that status. 
            # Any runs of the selected status that are shorter than the threshold (< min_threshold) 
            # will be converted to the other status. This will depend on the interval of your time variable.
            # For example, I think the min threshold for the status "day" is 6 hours. 
            # If my time variable is in 1 min intervals, my "min_threshold" is 6*60 = 360. 
            # If my time variable is 15 min intervals, my "min_threshold" is 6*4 = 24.
            # If any datapoint is part of a shorter series of "day" codes, it is likely an erroneous assignment.
      # "drop" (string) = "first", "last", NULL
            # The "drop" argument will ignore either the first or last section of your data if 
            # you know it is coded correctly, but it is shorter than the min_threshold. This will tell the code to 
            # ignore either the first or last status when running the status correction 
  
            # Drop "first" if the first status at the start of the timeseries is shorter than expected
            # Drop "last" if the last status at the end of the timeseries is shorter than expected
# Return:
      # df with "new_status" column

rle_status <- function(df, status_colname, status_select, min_threshold, drop){
  og_df <- df 
  
  # Get run length encoding of the status column
  runs <- rle(df[[status_colname]])
  # Convert list to df
  runs <- data.frame(status = runs$values, length = runs$lengths)
  
  # Make a numeric for submergence
  df$status_numeric <- ifelse(df[[status_colname]] == status_select, 1, 0)
  # See where status changes
  df$change <- c(diff(df$status_numeric),0)
  # Change rows is the row where the status ends
  runs$change_rows <- c(which(df$change != 0), NA)
  
  # Diffs represent status of (n+1) being different from (n)
  # A diff of 1 means the logger changed from exposed to submerged
  # A diff of -1 means the logger changed from submerged to exposed
  # When change != 0, it coincides with the end of a status (not the beginning of a new status)
  
  # Flag is TRUE if the status needs to be flipped
  # how many minutes is it reasonable for the logger to be submerged at minimum? like 3 hours? 
  # 3 hours = 3*60 min = 180 min
  runs$flag <- ifelse(runs$length < min_threshold & runs$status == status_select, T, F)
  
  # Create new col for corrected oyster status
  df$new_status <- df[[status_colname]]
  
  # Get runs of T only
  runs_fix <- runs %>% dplyr::filter(flag)
  if (missing(drop)) {
  } else if(drop == "first"){
    runs_fix <- runs_fix[-1,]
  } else if(drop == "last"){
    runs_fix <- runs_fix[-nrow(runs_fix),]
  }
  
  # If change_rows is NA, set it to be the last row in the df
  runs_fix$change_rows <- ifelse(is.na(runs_fix$change_rows), nrow(df), runs_fix$change_rows)
  
  # Get other status option
  status_options <- unique(df[[status_colname]])
  other_status <- status_options[status_options != status_select]
  
  # Change status for runs of T flags
  if(nrow(runs_fix > 0)){
    for(m in 1:nrow(runs_fix)){
      # Get first row of issues
      first_row <- runs_fix$change_rows[m]-runs_fix$length[m]+1
      
      # Get last row of issues
      last_row <- runs_fix$change_rows[m]
      
      # From first row to last row, change status to the one before the first row
      df$new_status[(first_row:last_row)] <- other_status
    }
    return(df)
  } else{
    (warning("no runs of selected status below minimum threshold, returning original dataframe"))
    return(og_df)
  }
}


# --- LINEAR INTERP FUNCTION -----
# Write linear interpolation function to change data to X interval and interpolate data
# df = data frame
# timestamp_col (string) = name of timestamp col
# interval = "1 min" or "1 hour" or "1 sec" 
# col_names (string) = vector of column names that you want to linear interpolate

linear_interp <- function(df, timestamp_col, interval, col_names){
  library(zoo)
  
  # Create full timeseries that you want
  timeladder <- seq(from = min(df[[timestamp_col]]), 
                    to = max(df[[timestamp_col]]), 
                    by = interval)
  
  # Set name of column
  timeladder_df <- data.frame(timestamp_est = timeladder)
  names(timeladder_df) <- timestamp_col
  
  # join "timeladder" with original data
  df_full <- full_join(timeladder_df, df, by = timestamp_col)
  
  # uhhh rewrite this to work with a vector of colnames
  for(n in 1:length(col_names)){
    temp <- col_names[n]
    df_full[[temp]] <- zoo::na.approx(df_full[[temp]])
  }
  return(df_full)
}

## ---- READ ELITECH FUNCTION -----
# Make a read elitech function
elitech_read <- function(filename){
  # Read file appropriately
  dat <- read_excel(filename, sheet = 1, skip = 26)
  # Change colnames
  names(dat) <- c("number", "time_edt", "temp_c", "rh_pct")
  # Add logger name
  dat$logger <- sapply(strsplit(basename(filename), "_"), "[[", 1)
  # Format times and get date
  dat$time_edt <- lubridate::ymd_hms(dat$time_edt)
  dat$date <- lubridate::date(dat$time_edt)
  
  return(dat)
}


## ----- READ HOBO FUNCTION -----


