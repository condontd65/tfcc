library(ggplot2)
library(RODBC)
library(stringr)
#library(sqldf)
library(data.table)
library(plyr)
library(stringdist)



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
rm(query)

# Fix zip codes in zip by adding a 0 at the end
zip$zip <- str_pad(zip$zip, 5, pad = "0")

# Fix zip codes in master as there are values that don't begin with 0
test.zip <- nchar(as.character(master$Current.Zip.Code))
test.bool <- test.zip == 4

master$zip.bool <- test.bool
rm(test.bool)
rm(test.zip)
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
col.names.accept <- c('Timestamp','Date.of.Birth','High.School.Attended','High.School.Graduation.Year',
                      'Race.Ethnicity','Gender','Native.Language','Zip.Code','Planned.School',
                      'Confirm.GED.HiSET.Certification','Date.GED.HiSET.Certification.Rec.vd',
                      'Registered.for.Classes','Household.Size','Household.Income','Pell.Eligible',
                      'OWD.Eligibility.Type','Date.Letter.Sent')

colnames(accept) <- col.names.accept
colnames(master) <- col.names.master

col.accept <- paste('accept$', col.names.accept,
                    sep = '')


# Join the master with the Boston zip code data to elimate those that don't qualify
# Leave this for later
master$Zip.Code <- as.factor(master$Zip.Code)
zip$zip <- as.factor(zip$zip)

master.boston <- merge(master, zip, by.x = "Zip.Code",
                       by.y = "zip")

# Clean up accept zip by only keeping first 5 digits
accept$Zip.Code <- substr(accept$Zip.Code, 0, 5)


### Take a look at high schools attended (clean it up)
accept$High.School.Attended <- gsub('EMK', 'Edward M Kennedy High School',
                                    accept$High.School.Attended)

## Use string distance matching to find closes matches to known schools list
# Import a list of schools
high.list <- read.csv('high_schools.csv') #will add to this if ever needed

# Iterated through to find that a distance match of 40 captured all once
i <-amatch(accept$High.School.Attended, high.list$high.schools, maxDist = 40)
i

# Make new dataframe to put in matched values and put back into accept dataframe
high.matched <- data.frame(rawtext = accept$High.School.Attended,
                           match = high.list$high.schools[i])

# Bring high school matchings into accept dataframe
accept$High.School <- high.matched$match































### Plot out school attendance from accepted


# Get basic counts using a loop over all columns
get.counts <- function(column) {
  df <- as.data.frame(table(column))
  return(df)
}

df <- lapply(accept, get.counts)
list2env(df, envir = .GlobalEnv)

# Remove unneeded tables
rm(Confirm.GED.HiSET.Certification)
rm(Date.GED.HiSET.Certification.Rec.vd)
rm(Date.Letter.Sent)
rm(Date.of.Birth)
rm(Household.Size)
rm(Household.Income)
rm(Pell.Eligible)
rm(High.School.Attended)


# Set plot colours according to Boston branding
ch.blue <- '#091F2F'
op.blue <- '#288BE4'
fr.red <- '#FB4D42'

# Test plot using just school counts
school.counts <- as.data.frame(table(accept$Planned.School))

p <- ggplot(data = school.counts, aes(x=Var1, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p + xlab("Planned College") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue))


## Make additional using school counts
p.planned.school <- ggplot(data = Planned.School, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.planned.school + xlab("Planned College") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('Planned College Attendance') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))
# Print(p.planned.school)
png("planned_college.png", width = 600,
    height = 600)
print(p.planned.school + xlab("Planned College") + ylab('Frequency') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue))+
        ggtitle('Planned College Attendance') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()


## Zip counts plot
p.zip.code <- ggplot(data = Zip.Code, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.zip.code + xlab("Zip Code") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('Current Zip Code Residence') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))

# Print(p.planned.school)
png("zip_code.png", width = 900,
    height = 900)
print(p.zip.code + xlab("Zip Code") + ylab('Frequency') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue)) +
        ggtitle('Current Zip Code Residence') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()


## High school plot
p.high.school <- ggplot(data = High.School, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.high.school + xlab("High School") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('High School Attended') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))

# Print(p.planned.school)
png("high_school.png", width = 1200,
    height = 900)
print(p.high.school + xlab("High School") + ylab('Frequency') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue,
                                       angle = 90, hjust = 1)) +
        ggtitle('High School Attended') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()



















