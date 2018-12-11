library(ggplot2)
library(RODBC)
library(stringr)
#library(sqldf)
library(data.table)
library(plyr)



# Set working directory to point to tables interested in
setwd("~/analyst/tfcc/tables")

# Import tables (they were split by masterlist and acceptance tabs)
master <- read.csv("TFCC Fall 2018 Masterlist_Redacted.csv")
accept <- read.csv("TFCC Fall 2018 Masterlist_Redacted_acceptance.csv")

# Accepted has a ton of additional columns with name x, x.1, x.2   take these out
accept <- accept[,1:17]

# Bring in Zip Codes for Boston
con <- odbcDriverConnect('driver={SQL Server};
                          server=vsql22;database=EGIS;
                          trusted_connection=true')
query <- "SELECT ZIP5 as zip FROM doit.ZIPCODES;"
zip <- sqlQuery(con, query)

# Fix zip codes in zip by adding a 0 at the end
zip$zip <- str_pad(zip$zip, 5, pad = "0")

# Fix zip codes in master as there are values that don't begin with 0
test.zip <- nchar(as.character(master$Current.Zip.Code))
test.bool <- test.zip == 4

master$zip.bool <- test.bool
master$Current.Zip.Code[master$zip.bool] <- str_pad(master$Current.Zip.Code,
                                                    5, pad = "0")

# Clean up column names; save old column names in a vector in case need to be replaced
col.names.old.master <- colnames(master)
col.names.old.accept <- colnames(accept)
col.names.master <- c('Timestamp','Date.of.Birth','Confirm.GED.HiSET.Certification',
                      'High.School.Attended','ID','Date.GED.HiSET.Certification.Rec.vd',
                      'Race.Ethnicity','Gender','Native.Language','Zip.Code','Planned.School',
                      'Connected.Success.Boston.Coach','Four.Year.Transfer.Interest',
                      'Boston.Bridge.Interest')



# Join the master with the Boston zip code data to elimate those that don't qualify
# Leave this for later
master$Current.Zip.Code <- as.factor(master$Current.Zip.Code)
zip$zip <- as.factor(zip$zip)

master.boston <- merge(master, zip, by.x = "Current.Zip.Code",
                       by.y = "zip")























