library(civis)
library(DescTools)
library(stringdist)
#library(data.table)

table.schema <- 'owd_restricted_data.bps_college_enrollment'
tfcc <- read_civis(table.schema, database='Boston')

# Clean up high schools with function
high.match <- function(df, df2) {
  d <- df
  hs <- df2
  i <- amatch(d$HighSchoolAttended, hs$high.schools, maxDist = 40)
  high.match <- data.frame(rawtext = d$HighSchoolAttended,
                           match = hs$high.schools[i])
  d$High.School <- high.match$match
  df <- d
}

# Load in high school data
high.schools <- read.csv('tables/high_schools.csv', stringsAsFactors = FALSE)

# Fix burke due to it's difficulty
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended %like% '%burke%' |
                            tfcc$HighSchoolAttended %like% '%Burke%' | tfcc$HighSchoolAttended == "Jeremiah e Burke"] <- 
  'Jeremiah E Burke High School'
# Fix CASH to community academy of science and health
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended %like% '%CASH%' ] <- 
  'Community Academy of Science and Health'
# Fix EMK to Edward M Kennedy Academy
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended %like% '%EMK%' ] <-
  'Edward M Kennedy Academy'
# Fix madison park
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended %like% '%Madison Park%' ] <-
  'Madison Park High School'
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended == 'Madison park' | tfcc$HighSchoolAttended == 'Madison Park' ] <-
  'Madison Park High School'
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended == 'Madison Park Technical Vocational High School' ] <-
  'Madison Park High School'
# Fix community academy
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended == 'Community Academy' ] <-
  'Community Academy of Science and Health'
# Fix o'bryant
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended %like% "%O'Bryant%" | tfcc$HighSchoolAttended == "O'Bryant High School" ] <-
  "John D O'Bryant"
# Boston International
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended %like% "%Boston International%" ] <-
  "Boston International High School"
# Fix English
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended == "English" ] <-
  "The English High School"
tfcc$HighSchoolAttended [ tfcc$HighSchoolAttended == "Greator Egleston" ] <-
  "Greater Egleston High School"

# Correct the high schools using custom function
highs.corrected <- high.match(tfcc,high.schools)

# Create data
high.check <- data.frame(highs.corrected$UniqueID, highs.corrected$HighSchoolAttended, highs.corrected$High.School)

#
tfcc$HighSchoolAttended <- high.check$highs.corrected.High.School



