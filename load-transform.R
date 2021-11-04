library(readxl)
library(lubridate)
library(tidyverse)

df <- read_excel("./data/summary.xlsx")
View(df)

df$`Date/Time` <- ymd_hms(df$`Date/Time`)
df$Duration <- as.duration(hms(df$Duration))
df$Date <- date(df$`Date/Time`)
df$Time <- hms(format(df$`Date/Time`, format = '%H:%M:%S'))
df$Month <- month(df$`Date/Time`)
df$Weekday <- wday(df$`Date/Time`)

df2020 <- df[df$Date < ymd("2020-12-27"),]
df2021 <- df[df$Date > ymd("2020-12-27"),]


# Read in link files and combine content into a single dataframe

file_list <- list.files("./data")
linkset <- data.frame()
for (i in 1:(length(file_list) - 5)) {
  if (substr(file_list[i],1,1) %in% c(0,1)) {
    temp <- read_excel(paste0("./data/",file_list[i]))
    if (length(temp) < 3) {
      temp$Clicks <- NA
    }
    temp$date <- gsub(".xlsx", "", file_list[i])
    temp$relPos <- temp$Tag / max(temp$Tag) # Convert Tag # into relative position in XL doc
    linkset <- rbind(linkset, temp)
  }
}

linkset$date <- mdy(linkset$date)

#Remove link files from before summary document started
linkset2 <- linkset[linkset$date > mdy(07192020),]


linkset$weekday <- wday(linkset$date)


############################
### Work on link files
############################
dates <- unique(linkset$date)
by_date <- linkset %>% group_by(date)

max(linkset$Clicks, na.rm = TRUE)
linkset$max <- FALSE

maxclicks <- by_date %>% summarize(max = max(Clicks))
maxclicks$max[is.na(maxclicks$max)] <- 0

maxTag <- by_date %>% summarize(max = max(Tag))

linkset
# Convert Tag # to relative position



write.csv(maxlinks, "./data/maxlinks.csv", row.names = FALSE)
write.csv(linkset2, "./data/linkset.csv", row.names = FALSE)
write.csv(df, "./data/newsummary.csv", row.names = FALSE)

####
## get donation info
#####

donations <- read.csv("./data/donations.csv")
donations$created_at_utc <- mdy_hm(donations$created_at_utc)
donations$date <- date(donations$created_at_utc)

write.csv(donations, "./data/newdonations.csv", row.names = FALSE)
