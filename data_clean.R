### This script is for cleaning up the school names for consistency and trying
### to ensure that all zip codes are formatted correctly

# Load required packages
library(ggplot2)
library(RODBC)
library(stringr)
#library(sqldf)
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

# Fix zip codes in zip by adding a 0 at the end
zip$zip <- str_pad(zip$zip, 5, pad = "0")

# Fix zip codes in master as there are values that don't begin with 0
# use loop over all zips
# create vector of column names to loop over
accepted.2018$Zip <- accepted.2018$Current.Zip.Code
accepted.2016$Zip <- accepted.2016$Current.Zip.Code
accepted.2016[1:100,18] <- accepted.2016[1:100,9]
accepted.2017$Zip <- as.integer(as.character(accepted.2017$Zip))
accepted.2018$Zip <- as.integer(as.character(accepted.2018$Zip))


## Turn this into a loop if you have time

#dfs <- list(accepted.2018,accepted.2016,
 #            accepted.2017,accepted.spring.2018)
#new.names <- c('accept.2018', 'accept.2016', 'accept.2017', 'accept.sp.2018')
#dfs <- lapply(dfs, setNames, nm = new.names)

#for (df in dfs) {
 # test.zip <- nchar(as.character(paste(df),"$Zip", sep = ""))
  #test.bool <- test.zip == 4
  #paste(df,"$zip.bool", sep = "") <- test.bool
  #rm(test.bool)
  #rm(test.zip)
  #paste(df,"$Zip[",df,"$zip.bool", sep = "") <- str_pad(paste(df,"$Zip"), 5, pad = "0")
  
#}
## Turn this into a loop if you have time

# Fix zip for accepted.2016

na.zero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

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

accepted.2016 <- zip.fix(accepted.2016)
accepted.2017 <- zip.fix(accepted.2017)
accepted.2018 <- zip.fix(accepted.2018)
accepted.spring.2018 <- zip.fix(accepted.spring.2018)


dfs <- lapply(dfs, zip.fix)

list2env(x = dfs, envir = .GlobalEnv)




#for (df in dfs) {
#  master <- df
#  master$Zip <- master$Zip %>%
#    mutate(Zip = if_else(is.na(Zip), 0, Zip))
#  test.zip <- nchar(as.character(master$Zip))
#  test.bool <- test.zip == 4
#  master$zip.bool <- test.bool
#  rm(test.bool)
#  rm(test.zip)
#  master$Zip[master$zip.bool] <- str_pad(master$Zip, width = 5, pad = "0")
#  df <- master
#}




master <- accepted.2016
#master$Current.Zip.Code[is.na(master$Current.Zip.Code)] <- 00000
test.zip <- nchar(as.character(master$Zip))
test.bool <- test.zip == 4

master$zip.bool <- test.bool
rm(test.bool)
rm(test.zip)
master$Zip[master$zip.bool] <- str_pad(master$Zip, width = 5, pad = "0")
accepted.2016 <- master













