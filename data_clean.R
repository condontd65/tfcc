### This script is for cleaning up the school names for consistency and trying
### to ensure that all zip codes are formatted correctly

# Load required packages
library(ggplot2)
library(RODBC)
library(stringr)
library(data.table)
library(plyr)
library(stringdist)
library(leaflet)
library(leaflet.esri)
library(leaflet.extras)
library(rjson)
library(rgdal)
library(rgeos)
library(dplyr)
library(devtools)
library(webshot)
library(htmlwidgets)
library(tidyr)
library(stringi)

## Import all of the data from csv
# Set working directory
setwd("~/analyst/tfcc/tables")

# Clear workspace to start anew
rm(list = ls())

# Import all data for each class
high.schools <- read.csv('high_schools.csv', stringsAsFactors = FALSE)
accepted.2018 <- read.csv('TFCC Fall 2018 Masterlist_Redacted_acceptance.csv')
applied.2018 <- read.csv('TFCC Fall 2018 Masterlist_Redacted.csv', stringsAsFactors = FALSE)
accepted.spring.2018 <- read.csv('SPRING_2018.csv', stringsAsFactors = FALSE) # 66-125 need schools fixed
accepted.2016 <- read.csv('FALL_2016.csv', stringsAsFactors = FALSE)
accepted.2016 <- accepted.2016[-1,]
accepted.2016 <- data.table(accepted.2016)
accepted.2017 <- read.csv('FALL_2017.csv')

# Trim data of incorrect columns and correct obviously erroneous data
accepted.2018 <- accepted.2018[,1:17]
accepted.2017 <- accepted.2017[,1:13]
accepted.spring.2018 <- accepted.spring.2018[,1:15]
accepted.spring.2018[66:125,4] <- accepted.spring.2018[66:125,15] # fix 66-125 high school

# Bring in Zip Codes for Boston
con <- odbcDriverConnect('driver={SQL Server};
                         server=vsql22;database=EGIS;
                         trusted_connection=true')
query <- "SELECT ZIP5 as zip FROM doit.ZIPCODES;"
zip <- sqlQuery(con, query)
rm(query)
odbcCloseAll()

# Fix zip codes in zip by adding a 0 at the beginning
zip$zip <- str_pad(zip$zip, 5, pad = "0")

## Fix zip codes in data frames as there are values that don't begin with 0
# Start by cleaning up zip miscellaneously 
accepted.2018$Zip <- accepted.2018$Current.Zip.Code
accepted.2016$Zip <- accepted.2016$Current.Zip.Code
accepted.2016[1:100,18] <- accepted.2016[1:100,9]
accepted.2017$Zip <- as.integer(as.character(accepted.2017$Zip))
accepted.2018$Zip <- as.integer(as.character(accepted.2018$Zip))


# Fix zip for accepted.2016

# NAs give a problem, so write a function to convert all to zeros
na.zero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

# Write function to pad values with only 4 zip characters with a zero at the front
zip.fix <- function(df) {
  master <- df
  master$Zip <- na.zero(master$Zip)
  test.zip <- nchar(as.character(master$Zip))
  test.bool <- test.zip == 4
  master$zip.bool <- test.bool
  rm(test.bool)
  rm(test.zip)
  master$Zip[master$zip.bool] <- str_pad(master$Zip, width = 5, pad = "0")
  df <- master
}

# Run zip.fix on data frames. Gives an error but can be ignored
accepted.2016 <- zip.fix(accepted.2016)
accepted.2017 <- zip.fix(accepted.2017)
accepted.2018 <- zip.fix(accepted.2018)
accepted.spring.2018 <- zip.fix(accepted.spring.2018)


### Take a look at high schools attended (clean it up)
# Replace CASH and EMK, two cases that are difficult with the other replacement
accepted.2016$High.School.Attended <- gsub('EMK', 'Edward M Kennedy High School',
                                           accepted.2016$High.School.Attended)
accepted.2016$High.School.Attended <- gsub('Cash', 'Community Academy of Science and Health',
                                           accepted.2016$High.School.Attended)
accepted.2016$High.School.Attended <- gsub('CASH', 'Community Academy of Science and Health',
                                           accepted.2016$High.School.Attended)
accepted.2016$High.School.Attended <- gsub('BATA', 'Boston Adult Technical Academy',
                                           accepted.2016$High.School.Attended)
accepted.2016$High.School.Attended <- gsub('CRLS', 'Cambridge Ringe and Latin School',
                                           accepted.2016$High.School.Attended)
# Alter naming of column to match others
accepted.2017$High.School.Attended <- accepted.2017$Boston.Public.High.School.Attended

accepted.2017$High.School.Attended <- gsub('EMK', 'Edward M Kennedy High School',
                                           accepted.2017$High.School.Attended)
accepted.2017$High.School.Attended <- gsub('CASH', 'Community Academy of Science and Health',
                                           accepted.2017$High.School.Attended)
accepted.2017$High.School.Attended <- gsub('Cash', 'Community Academy of Science and Health',
                                           accepted.2017$High.School.Attended)
accepted.2017$High.School.Attended <- gsub('BATA', 'Boston Adult Technical Academy',
                                           accepted.2017$High.School.Attended)
accepted.2017$High.School.Attended <- gsub('CRLS', 'Cambridge Ringe and Latin School',
                                           accepted.2017$High.School.Attended)


accepted.2018$High.School.Attended <- gsub('EMK', 'Edward M Kennedy High School',
                                           accepted.2018$High.School.Attended)
accepted.2018$High.School.Attended <- gsub('CASH', 'Community Academy of Science and Health',
                                           accepted.2018$High.School.Attended)
accepted.2018$High.School.Attended <- gsub('Cash', 'Community Academy of Science and Health',
                                           accepted.2018$High.School.Attended)
accepted.2018$High.School.Attended <- gsub('BATA', 'Boston Adult Technical Academy',
                                           accepted.2018$High.School.Attended)
accepted.2018$High.School.Attended <- gsub('CRLS', 'Cambridge Ringe and Latin School',
                                           accepted.2018$High.School.Attended)

# Alter naming of column to match others
accepted.spring.2018$High.School.Attended <- accepted.spring.2018$Boston.Public.High.School.Attended
accepted.spring.2018$High.School.Attended <- gsub('EMK', 'Edward M Kennedy High School',
                                           accepted.spring.2018$High.School.Attended)
accepted.spring.2018$High.School.Attended <- gsub('CASH', 'Community Academy of Science and Health',
                                           accepted.spring.2018$High.School.Attended)
accepted.spring.2018$High.School.Attended <- gsub('Cash', 'Community Academy of Science and Health',
                                                  accepted.spring.2018$High.School.Attended)
accepted.spring.2018$High.School.Attended <- gsub('BATA', 'Boston Adult Technical Academy',
                                                  accepted.spring.2018$High.School.Attended)
accepted.spring.2018$High.School.Attended <- gsub('CRLS', 'Cambridge Ringe and Latin School',
                                                  accepted.spring.2018$High.School.Attended)

## Use string distance matching to find closes matches to known schools list
# Write function to do this
high.match <- function(df, df2) {
  d <- df
  hs <- df2
  i <- amatch(d$High.School.Attended, hs$high.schools, maxDist = 40)
  high.match <- data.frame(rawtext = d$High.School.Attended,
                           match = hs$high.schools[i])
  d$High.School <- high.match$match
  df <- d
}

# Run high.match function on the 4 data frames
accepted.2016 <- high.match(accepted.2016, high.schools)
accepted.2017 <- high.match(accepted.2017, high.schools)
accepted.2018 <- high.match(accepted.2018, high.schools)
accepted.spring.2018 <- high.match(accepted.spring.2018, high.schools)

# Turn "blank" into NA
accepted.2016$High.School[accepted.2016$High.School == "blank"] <- NA
accepted.2017$High.School[accepted.2017$High.School == "blank"] <- NA
accepted.2018$High.School[accepted.2018$High.School == "blank"] <- NA
accepted.spring.2018$High.School[accepted.spring.2018$High.School == "blank"] <- NA

# Remove unneeded high schol table going forward
rm(high.schools)

accepted.2016$Planned.College <- accepted.2016$Which.school.do.you.plan.to.attend.in.Spring.2017.
accepted.2017$Planned.College <- accepted.2017$Which.school.do.you.plan.to.attend.in.Spring.2017.
accepted.2018$Planned.College <- accepted.2018$Which.school.do.you.plan.to.attend.in.Fall.2017.
accepted.spring.2018$Planned.College <- accepted.spring.2018$Which.school.do.you.plan.to.attend.in.Spring.2017.

# Bunker Hill Community college is misspelled in the 2018 data frame
accepted.2018$Planned.College <- as.character(accepted.2018$Planned.College)
accepted.2018$Planned.College[accepted.2018$Planned.College == "Bunker Hill Commuity College"] <- 
  "Bunker Hill Community College (BHCC)"

# Set 0 and 00000 values in Zip to NA
accepted.2016$Zip[accepted.2016$Zip == "0" | accepted.2016$Zip == "00000"] <- NA
accepted.2017$Zip[accepted.2017$Zip == "0" | accepted.2017$Zip == "00000"] <- NA
accepted.2018$Zip[accepted.2018$Zip == "0" | accepted.2018$Zip == "00000"] <- NA
accepted.spring.2018$Zip[accepted.spring.2018$Zip == "0" | accepted.spring.2018$Zip == "00000"] <- NA

### Write files to csv to be used in other scripts moving forward
#write.csv(accepted.2016, file = "clean_files/accepted_2016.csv")
#write.csv(accepted.2017, file = "clean_files/accepted_2017.csv")
#write.csv(accepted.2018, file = "clean_files/accepted_2018.csv")
#write.csv(accepted.spring.2018, file = "clean_files/accepted_spring_2018.csv")










