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
library(mapview)



# Set plot colours according to Boston branding
ch.blue <- '#091F2F'
op.blue <- '#288BE4'
fr.red <- '#FB4D42'

# Get basic counts using a custom function
get.counts <- function(column) {
  df <- as.data.frame(table(column))
  return(df)
}

### Run get.counts to get high school, zip code, and planned school counts
# 2016
hs.2016 <- get.counts(accepted.2016$High.School) %>%
  subset(Freq > 0)

zip.2016 <- get.counts(accepted.2016$Zip)

college.2016 <- get.counts(accepted.2016$Planned.College)

# 2017
hs.2017 <- get.counts(accepted.2017$High.School) %>%
  subset(Freq > 0)

zip.2017 <- get.counts(accepted.2017$Zip)

college.2017 <- get.counts(accepted.2017$Planned.College)

# 2018 spring
hs.sp.2018 <- get.counts(accepted.spring.2018$High.School) %>%
  subset(Freq > 0)

zip.sp.2018 <- get.counts(accepted.spring.2018$Zip)

college.sp.2018 <- get.counts(accepted.spring.2018$Planned.College)

# 2018
hs.2018 <- get.counts(accepted.2018$High.School) %>%
  subset(Freq > 0)

zip.2018 <- get.counts(accepted.2018$Zip)

college.2018 <- get.counts(accepted.2018$Planned.College)


## College plots first
#2016
p.planned.school <- ggplot(data = college.2016, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.planned.school + xlab("Planned College") + ylab('Students') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2016 Planned College Attendance') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))
# Print(p.planned.school)
png("plots/planned_college_2016.png", width = 600,
    height = 600)
print(p.planned.school + xlab("Planned College") + ylab('Students') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue))+
        ggtitle('2016 Planned College Attendance') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()

#2017
p.planned.school <- ggplot(data = college.2017, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.planned.school + xlab("Planned College") + ylab('Students') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2017 Planned College Attendance') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))
# Print(p.planned.school)
png("plots/planned_college_2017.png", width = 600,
    height = 600)
print(p.planned.school + xlab("Planned College") + ylab('Students') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue))+
        ggtitle('2017 Planned College Attendance') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()

#2018 Spring
p.planned.school <- ggplot(data = college.sp.2018, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.planned.school + xlab("Planned College") + ylab('Students') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2018 Spring Planned College Attendance') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))
# Print(p.planned.school)
png("plots/planned_college_sp_2018.png", width = 600,
    height = 600)
print(p.planned.school + xlab("Planned College") + ylab('Students') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue))+
        ggtitle('2018 Spring Planned College Attendance') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()

# 2018 Fall
p.planned.school <- ggplot(data = college.2018, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.planned.school + xlab("Planned College") + ylab('Students') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2018 Fall Planned College Attendance') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))
# Print(p.planned.school)
png("plots/planned_college_fall_2018.png", width = 600,
    height = 600)
print(p.planned.school + xlab("Planned College") + ylab('Students') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue))+
        ggtitle('2018 Fall Planned College Attendance') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()


## High Schools
# 2016
p.high.school.horiz <- ggplot(data = hs.2016, 
                              aes(x=factor(column,levels = rev(levels(factor(column)))),y=Freq)) +
  geom_bar(position = "dodge", stat = "identity", fill = ch.blue) +
  coord_flip() +
  geom_text(aes(label=Freq),
            #position = position_dodge(width = 1),
            hjust = -0.25, colour = ch.blue, size = 6, vjust = 0.25)
p.high.school.horiz + xlab("High School") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2016 High School Attended') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))
# print high school horizontal
png("plots/high_school_2016.png", width = 900,
    height = 1200)
p.high.school.horiz + xlab("High School") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue, size = 16)) +
  theme(axis.text = element_text(colour = ch.blue, size = 16)) +
  ggtitle('2016 High School Attended') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue, size = 22))
dev.off()

# 2017
p.high.school.horiz <- ggplot(data = hs.2017, 
                              aes(x=factor(column,levels = rev(levels(factor(column)))),y=Freq)) +
  geom_bar(position = "dodge", stat = "identity", fill = ch.blue) +
  coord_flip() +
  geom_text(aes(label=Freq),
            #position = position_dodge(width = 1),
            hjust = -0.25, colour = ch.blue, size = 5, vjust = 0.25)
p.high.school.horiz + xlab("High School") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2017 High School Attended') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))
# print high school horizontal
png("plots/high_school_2017.png", width = 900,
    height = 1200)
p.high.school.horiz + xlab("High School") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue, size = 16)) +
  theme(axis.text = element_text(colour = ch.blue, size = 16)) +
  ggtitle('2017 High School Attended') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue, size = 22))
dev.off()

# 2018 Spring
p.high.school.horiz <- ggplot(data = hs.sp.2018, 
                              aes(x=factor(column,levels = rev(levels(factor(column)))),y=Freq)) +
  geom_bar(position = "dodge", stat = "identity", fill = ch.blue) +
  coord_flip() +
  geom_text(aes(label=Freq),
            #position = position_dodge(width = 1),
            hjust = -0.25, colour = ch.blue, size = 5, vjust = 0.25)
p.high.school.horiz + xlab("High School") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2018 Spring High School Attended') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))
# print high school horizontal
png("plots/high_school_sp_2018.png", width = 900,
    height = 1200)
p.high.school.horiz + xlab("High School") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue, size = 16)) +
  theme(axis.text = element_text(colour = ch.blue, size = 16)) +
  ggtitle('2018 Spring High School Attended') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue, size = 22))
dev.off()

# 2018 Fall
p.high.school.horiz <- ggplot(data = hs.2018, 
                              aes(x=factor(column,levels = rev(levels(factor(column)))),y=Freq)) +
  geom_bar(position = "dodge", stat = "identity", fill = ch.blue) +
  coord_flip() +
  geom_text(aes(label=Freq),
            #position = position_dodge(width = 1),
            hjust = -0.25, colour = ch.blue, size = 5, vjust = 0.25)
p.high.school.horiz + xlab("High School") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2018 High School Attended') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))
# print high school horizontal
png("plots/high_school_2018.png", width = 900,
    height = 1200)
p.high.school.horiz + xlab("High School") + ylab('Frequency') + 
  theme(axis.title = element_text(colour = ch.blue, size = 16)) +
  theme(axis.text = element_text(colour = ch.blue, size = 16)) +
  ggtitle('2018 High School Attended') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue, size = 22))
dev.off()

## Zip code plots
# 2016
p.zip.code <- ggplot(data = zip.2016, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.zip.code + xlab("Zip Code") + ylab('Students') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2016 Current Zip Code Residence') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))

# Print(p.planned.school)
png("plots/zip_code_2016.png", width = 900,
    height = 900)
print(p.zip.code + xlab("Zip Code") + ylab('Students') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue)) +
        ggtitle('2016 Current Zip Code Residence') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()

# 2017
p.zip.code <- ggplot(data = zip.2017, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.zip.code + xlab("Zip Code") + ylab('Students') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2017 Current Zip Code Residence') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))

# Print(p.planned.school)
png("plots/zip_code_2017.png", width = 1500,
    height = 900)
print(p.zip.code + xlab("Zip Code") + ylab('Students') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue)) +
        ggtitle('2017 Current Zip Code Residence') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()

# Fall 2018
p.zip.code <- ggplot(data = zip.2018, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.zip.code + xlab("Zip Code") + ylab('Students') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2018 Current Zip Code Residence') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))

# Print(p.planned.school)
png("plots/zip_code_fall_2018.png", width = 900,
    height = 900)
print(p.zip.code + xlab("Zip Code") + ylab('Students') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue)) +
        ggtitle('2018 Fall Current Zip Code Residence') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()

# Spring 2018
p.zip.code <- ggplot(data = zip.sp.2018, aes(x=column, y=Freq)) +
  geom_bar(stat='identity', fill = ch.blue) +
  geom_text(aes(label=Freq),
            position = position_dodge(width = 0.9),
            vjust = -0.25, colour = ch.blue)
p.zip.code + xlab("Zip Code") + ylab('Students') + 
  theme(axis.title = element_text(colour = ch.blue)) +
  theme(axis.text = element_text(colour = ch.blue)) +
  ggtitle('2018 Fall Current Zip Code Residence') +
  theme(plot.title = element_text(hjust = 0.5, colour = ch.blue))

# Print(p.planned.school)
png("plots/zip_code_sp_2018.png", width = 1500,
    height = 900)
print(p.zip.code + xlab("Zip Code") + ylab('Students') + 
        theme(axis.title = element_text(colour = ch.blue)) +
        theme(axis.text = element_text(colour = ch.blue)) +
        ggtitle('2018 Spring Current Zip Code Residence') +
        theme(plot.title = element_text(hjust = 0.5, colour = ch.blue)))
dev.off()


### Map based on zip codes
# Edit zip columns to make later merge easier
zip.2016$Freq.2016 <- zip.2016$Freq
zip.2017$Freq.2017 <- zip.2017$Freq
zip.sp.2018$Freq.sp.2018 <- zip.sp.2018$Freq
zip.2018$Freq.2018 <- zip.2018$Freq

# Connect to sql database to bring in zip code spatial data
vsql22dsn <- c("MSSQL:server=vsql22;database=EGIS;
               trusted_connection=yes")

# 102686 EPSG
projstring <- CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000.0000000001 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")

# ogrListLayers(vsql22dsn)
lyr <- c("doit.ZIPCODES")
spdf <- readOGR(dsn = vsql22dsn, layer = lyr)

# Define coordinate system
proj4string(spdf) <- projstring

zips.4326 <- spTransform(spdf, CRS("+init=epsg:4326"))

# Bring in Boston basemap
bos <- "https://awsgeo.boston.gov/arcgis/rest/services/Basemaps/BostonCityBasemap_WM/MapServer"

## Merge them
# 2016
zips.4326.merge <- merge(zips.4326, zip.2016, by.x = 'ZIP5', by.y = 'column', all.x = TRUE)

# Separate them to turn NAs into zeros
zips.step <- data.frame(zips.4326.merge$ZIP5, zips.4326.merge$Freq.2016) %>%
  
  mutate_all(funs(replace(., is.na(.), 0)))
names(zips.step) <- c('zip','count')

# Put zeros back into shapefile
zips.4326.merge$Freq.2016 <- zips.step$count


# 2017
zips.4326.merge <- merge(zips.4326.merge, zip.2017, by.x = 'ZIP5', by.y = 'column', all.x = TRUE)

# Separate them to turn NAs into zeros
zips.step <- data.frame(zips.4326.merge$ZIP5, zips.4326.merge$Freq.2017) %>%
  
  mutate_all(funs(replace(., is.na(.), 0)))
names(zips.step) <- c('zip','count')

# Put zeros back into shapefile
zips.4326.merge$Freq.2017 <- zips.step$count


# 2018
zips.4326.merge <- merge(zips.4326.merge, zip.2018, by.x = 'ZIP5', by.y = 'column', all.x = TRUE)

# Separate them to turn NAs into zeros
zips.step <- data.frame(zips.4326.merge$ZIP5, zips.4326.merge$Freq.2018) %>%
  
  mutate_all(funs(replace(., is.na(.), 0)))
names(zips.step) <- c('zip','count')

# Put zeros back into shapefile
zips.4326.merge$Freq.2018 <- zips.step$count


# 2018 spring
zips.4326.merge <- merge(zips.4326.merge, zip.sp.2018, by.x = 'ZIP5', by.y = 'column', all.x = TRUE)

# Separate them to turn NAs into zeros
zips.step <- data.frame(zips.4326.merge$ZIP5, zips.4326.merge$Freq.sp.2018) %>%
  
  mutate_all(funs(replace(., is.na(.), 0)))
names(zips.step) <- c('zip','count')

# Put zeros back into shapefile
zips.4326.merge$Freq.sp.2018 <- zips.step$count


## Do the maps with Leaflet
pal <- colorNumeric(
  palette = "Blues",
  domain = zips.4326.merge$Freq
)

# Map it
# 2016
m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  #addMarkers(lng = -71.187886, lat = 42.160247, 
  #          popup = "Boston City Hall") %>%
  setView(lng = -71.087886, lat = 42.320247, zoom = 12) %>%
  #addEsriTiledMapLayer(url = bos) %>%
  #addEsriDynamicMapLayer(url = dist)
  addPolygons(data = zips.4326.merge,
              fillColor = ~pal(zips.4326.merge$Freq.2016),
              color = "darkgrey3",
              smoothFactor = 1,
              fillOpacity = 1,
              popup = zips.4326.merge$ZIP5,
              weight = 1.5,
              highlightOptions = highlightOptions(
                color = "#FB4D42", weight = 4,
                bringToFront = TRUE
              )) %>%
  addLegend("bottomright", pal = pal, values = zips.4326.merge$Freq.2016,
            title = "Total Students 2016",
            opacity = 1)

m

# Save the map
mapshot(m, file = "plots/zip_map_2016.png")

# 2017
m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  #addMarkers(lng = -71.187886, lat = 42.160247, 
  #          popup = "Boston City Hall") %>%
  setView(lng = -71.087886, lat = 42.320247, zoom = 12) %>%
  #addEsriTiledMapLayer(url = bos) %>%
  #addEsriDynamicMapLayer(url = dist)
  addPolygons(data = zips.4326.merge,
              fillColor = ~pal(zips.4326.merge$Freq.2017),
              color = "darkgrey3",
              smoothFactor = 1,
              fillOpacity = 1,
              popup = zips.4326.merge$ZIP5,
              weight = 1.5,
              highlightOptions = highlightOptions(
                color = "#FB4D42", weight = 4,
                bringToFront = TRUE
              )) %>%
  addLegend("bottomright", pal = pal, values = zips.4326.merge$Freq.2017,
            title = "Total Students 2017",
            opacity = 1)

m

# Save the map
mapshot(m, file = "plots/zip_map_2017.png")

# 2018 Fall
m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  #addMarkers(lng = -71.187886, lat = 42.160247, 
  #          popup = "Boston City Hall") %>%
  setView(lng = -71.087886, lat = 42.320247, zoom = 12) %>%
  #addEsriTiledMapLayer(url = bos) %>%
  #addEsriDynamicMapLayer(url = dist)
  addPolygons(data = zips.4326.merge,
              fillColor = ~pal(zips.4326.merge$Freq.2018),
              color = "darkgrey3",
              smoothFactor = 1,
              fillOpacity = 1,
              popup = zips.4326.merge$ZIP5,
              weight = 1.5,
              highlightOptions = highlightOptions(
                color = "#FB4D42", weight = 4,
                bringToFront = TRUE
              )) %>%
  addLegend("bottomright", pal = pal, values = zips.4326.merge$Freq.2018,
            title = "Total Students Fall 2018",
            opacity = 1)

m

# Save the map
mapshot(m, file = "plots/zip_map_2018_fall.png")

# 2018 Spring
m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  #addMarkers(lng = -71.187886, lat = 42.160247, 
  #          popup = "Boston City Hall") %>%
  setView(lng = -71.087886, lat = 42.320247, zoom = 12) %>%
  #addEsriTiledMapLayer(url = bos) %>%
  #addEsriDynamicMapLayer(url = dist)
  addPolygons(data = zips.4326.merge,
              fillColor = ~pal(zips.4326.merge$Freq.sp.2018),
              color = "darkgrey3",
              smoothFactor = 1,
              fillOpacity = 1,
              popup = zips.4326.merge$ZIP5,
              weight = 1.5,
              highlightOptions = highlightOptions(
                color = "#FB4D42", weight = 4,
                bringToFront = TRUE
              )) %>%
  addLegend("bottomright", pal = pal, values = zips.4326.merge$Freq.sp.2018,
            title = "Total Students Spring 2018",
            opacity = 1)

m

# Save the map
mapshot(m, file = "plots/zip_map_2018_spring.png")






