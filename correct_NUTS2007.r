

library(maps)
library(rgdal)
library(gbm)
library(raster)
library(rgeos)
library(maptools)
library(ncdf4)
library(ggplot2)

##########################################################################

# open the cleaned (with removed / merged polygons) NUTS Shapefile that 
# fits the EQL 2007 Regions

Nuts2_EQL_2007 <- "C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/old/NUTS2_EQL2007.shp"

# check if file is there
file.exists("C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/old/NUTS2_EQL2007.shp")

Nuts.EQL2007_shp <- readShapeSpatial(Nuts2_EQL_2007)

names(Nuts.EQL2007_shp)

# Projection: GCS_ETRS_1989

proj4string(Nuts.EQL2007_shp) <- "+proj=longlat +ellps=GRS80 +no_defs"

plot(Nuts.EQL2007_shp)

text(Nuts.EQL2007_shp, labels =  as.character(Nuts.EQL2007_shp@data$NUTS_ID),
     cex = 0.4, offset = 0.5)

length(unique(Nuts.EQL2007_shp$EQL_Region)) # 250

length(unique(Nuts.EQL2007_shp$NUTS_ID)) # 251 --> because of Bratislava!!

## EQL_Region with value 0 is Bratislava

Nuts.EQL2007_shp[Nuts.EQL2007_shp$EQL_Region == 0,]@data

##################

Nuts.EQL2007_shp@data <- Nuts.EQL2007_shp@data[,-c(9, 10, 11, 12, 13, 14)]

names(Nuts.EQL2007_shp@data)


######### Merge the two Finish NUTS Regions: FI1B and FI1C

test <- gUnion(Nuts.EQL2007_shp[Nuts.EQL2007_shp$NUTS_ID == "FI1B",], 
              Nuts.EQL2007_shp[Nuts.EQL2007_shp$NUTS_ID == "FI1C",])

test.df <- data.frame("NUTS_ID" = "FI1B_FI1C", "STAT_LEVL_" = 2, "SHAPE_AREA" = 0,  "SHAPE_LEN" = 0,
                         "OID_" = NA,  "CNTR_CODE" = "FI",  "NAME_LATN" = "Helsinki-Uusimaa Etelae-Suomi",
                         "NUTS_NAME" = "Helsinki-Uusimaa Etelae-Suomi")

# convert to spacial polygone

FI1B_FI1C <- SpatialPolygonsDataFrame(test, data = test.df)


plot(FI1B_FI1C)

plot(Nuts.EQL2007_shp[Nuts.EQL2007_shp@data$NUTS_ID == "FI1B",], col = "red", add = TRUE)

plot(Nuts.EQL2007_shp[Nuts.EQL2007_shp@data$NUTS_ID == "FI1C",], col = "lightblue", add = TRUE)


############################################

## reomve the old single polygons

Nuts.EQL2007_shp <- Nuts.EQL2007_shp[-which(Nuts.EQL2007_shp$NUTS_ID == "FI1B"),]

Nuts.EQL2007_shp <- Nuts.EQL2007_shp[-which(Nuts.EQL2007_shp$NUTS_ID == "FI1C"),]

str(Nuts.EQL2007_shp@data) # 249 obs.

plot(Nuts.EQL2007_shp)
## is gone
                                   
length(unique(Nuts.EQL2007_shp$NUTS_ID)) # 249

### now add the new dissolved polygon FI1B_FI1C

Nuts.EQL2007_shp1 <- rbind(Nuts.EQL2007_shp, FI1B_FI1C)

plot(Nuts.EQL2007_shp1)

text(Nuts.EQL2007_shp1, labels =  as.character(Nuts.EQL2007_shp1@data$NUTS_ID),
     cex = 0.4, offset = 0.5)


## NUTS IDS are ok

str(Nuts.EQL2007_shp1@data)

# Nuts.EQL2007_shp2 <- spChFIDs(Nuts.EQL2007_shp1, c(1:249))

# Nuts.EQL2007_shp2@data[Nuts.EQL2007_shp2@data$EQL_Name == 0, ]


################

matchNUTS_EQL <- read.csv(file = "C:/Users/jmethorst/Documents/EQL Data 2007/EQL_2007_match_NUTS.csv",
                          header = TRUE, sep = ";")

str(matchNUTS_EQL)

length(unique(matchNUTS_EQL$NUTS_ID)) # 250

length(unique(matchNUTS_EQL$EQL_Region)) # 250

## as factor
# matchNUTS_EQL$EQL_Region <- as.factor(matchNUTS_EQL$EQL_Region)

## find difference

setdiff(matchNUTS_EQL$NUTS_ID, Nuts.EQL2007_shp1@data$NUTS_ID)
# 0
# no differences!

########## replace the EQL_Region column in NUTs shapefile with correct values

## first remove wrong columns
# Nuts.EQL2007_shp1@data <- Nuts.EQL2007_shp1@data[, -c(9, 10, 11, 12, 13, 14)]

names(Nuts.EQL2007_shp1)

str(Nuts.EQL2007_shp1@data)

## add the correct EQL_REgion column to shapfile

EQL_table <- matchNUTS_EQL[, c(1, 3, 4)]

Nuts.EQL2007_shp2 <- merge(Nuts.EQL2007_shp1, matchNUTS_EQL[, c(1, 3, 4)], by = "NUTS_ID")



# Nuts.EQL2007_shp1@data <- data.frame(Nuts.EQL2007_shp1@data, EQL_table[match(EQL_table$NUTS_ID, EQL_table$NUTS_ID),] )


str(Nuts.EQL2007_shp2@data)

plot(Nuts.EQL2007_shp2)

text(Nuts.EQL2007_shp2, labels =  as.character(Nuts.EQL2007_shp2@data$NUTS_ID),
     cex = 0.4, offset = 0.5)


######### check why NUTS_ID is 250 obs.

summary(Nuts.EQL2007_shp2@data)

# setdiff(Nuts.EQL2007_shp1@data$NUTS_ID, Nuts.EQL2007_shp1@data$NUTS_ID.1)
# no difference

# Nuts.EQL2007_shp1@data <- Nuts.EQL2007_shp1@data[, -9]

names(Nuts.EQL2007_shp1@data)

###################

writeOGR(obj = Nuts.EQL2007_shp2, dsn = "C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles", 
         layer = "Nuts2_EQL_2007", driver="ESRI Shapefile", overwrite_layer = TRUE)

# writePolyShape(Nuts.EQL2007_shp1, "C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/Nuts2_EQL_2007")



shape <- readOGR(dsn = "C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles",
                   layer = "Nuts2_EQL_2007")

plot(shape)
