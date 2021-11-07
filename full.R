library(readxl)
library(lubridate)
library(tidyverse)

#### Read in and transform Summary File
summary <- read_excel("./data/summary.xlsx")
View(summary)

summary <- summary[,-2] # remove Duration column

#Format date information, add new columns for date processing
summary$`Date/Time` <- ymd_hms(summary$`Date/Time`)
summary$Date <- date(summary$`Date/Time`)
summary$Time <- hms(format(summary$`Date/Time`, format = '%H:%M:%S'))
summary$Month <- month(summary$`Date/Time`)
summary$Weekday <- wday(summary$`Date/Time`)


# Read in link files and combine content into a single dataframe
# Pseudocode
# 1. get file list
# 2. create empty data frame to hold link file contents
# 3. Loop through file list
# 4.  Only use files beginning with 0 or 1 - ignore summary.csv, etc.
# 5.    Read in file
# 6.    Some files don't have click info - set click count to NA
# 7.    Get date info from filename and use it to set variable 'date'
# 8.    Using 'Tag' and maximum 'Tag' values, calculate relative position in newsletter
#           0 = First tag, 100 = last tag
# 9.    Add table to combined table

file_list <- list.files("./data")
linkset <- data.frame()
for (i in 1:(length(file_list))) {
  if (substr(file_list[i],1,1) %in% c(0,1)) { 
    temp <- read_excel(paste0("./data/",file_list[i]))
    if (length(temp) < 3) {
      temp$Clicks <- NA
    }
    temp$date <- gsub(".xlsx", "", file_list[i]) 
    temp$relPos <- temp$Tag / max(temp$Tag)
    linkset <- rbind(linkset, temp)
  }
}

# Transform combined dataset
linkset$date <- mdy(linkset$date)

  #Remove link files from before summary document started
linkset <- linkset[linkset$date > mdy(07192020),]
  # Add 'weekday' column to df
linkset$weekday <- wday(linkset$date)


############################
### Work on link files
############################
#dates <- unique(linkset$date)

by_date <- linkset %>% group_by(date)

#max(linkset$Clicks, na.rm = TRUE)

# Create "max" boolean to report whether a record has the most clicks for the day - default to FALSE
linkset$max <- FALSE

# create df of max # of clicks per day
maxclicks <- by_date %>% summarize(max = max(Clicks))
maxclicks$max[is.na(maxclicks$max)] <- 0


# Find records in linkest that are in maxclicks, and reset 'max' value to TRUE to indicate that the link had the most 
#     clicks for the day
for (i in 1:nrow(maxclicks)) {
  index <- which((linkset$Clicks == maxclicks$max[i]) & (linkset$date == maxclicks$date[i]))
  linkset$max[index] <- TRUE
}
# Pull out links with the most clicks for the day into a separate df
maxlinks <- linkset[linkset$max == TRUE,]

####
## get donation info
#####

donations <- read.csv("./data/donations.csv")
donations$created_at_utc <- mdy_hm(donations$created_at_utc)
donations$date <- date(donations$created_at_utc)


###############################################
### Save important data frames for Tableau work
###############################################
write.csv(maxlinks, "./data/maxlinks.csv", row.names = FALSE)
write.csv(linkset, "./data/linkset.csv", row.names = FALSE)
write.csv(df, "./data/newsummary.csv", row.names = FALSE)
write.csv(donations, "./data/newdonations.csv", row.names = FALSE)