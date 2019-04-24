library(civis)
library(DescTools)

table.schema <- 'owd_restricted_data.bps_college_enrollment'
tfcc <- read_civis(table.schema, database='Boston')

# Clean up high schools with function
high.match <- function(df, df2) {
  d <- df
  hs <- df2
  i <- amatch(d$High.School.Attended, hs$high.schools, maxDist = 40)
  high.match <- data.frame(rawtext = d$High.School.Attended,
                           match = hs$high.schools[i])
  d$High.School <- high.match$match
  df <- d
}

high.schools <- read.csv('tables/high_schools.csv', stringsAsFactors = FALSE)





