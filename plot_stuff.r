


library(maps)
library(rgdal)
library(gbm)
library(raster)
library(rgeos)
library(maptools)
library(ncdf4)
library(ggplot2)
library(ggmap)
library(mapproj)

##################################################################################

### load the data

Dataset_total <- read.table("Dataset_total_2007.txt", header = TRUE, sep = "\t")

eql_2012 <- read.table("C:/Users/jmethorst/Documents/EQL Data 2011-2012/EQL 2012/UKDA-7316-tab/tab/3rd_eqls_final_dataset_ammended_2014.tab",
                       sep="\t", header=TRUE)

##################################################################################

socioeco_data1 <- Dataset_total[, c("WGT_TARGET", "WGT_TOTAL", "country", "country_abbr", "EQL_Region", "hh2a", "CVhh2b", "CV6768o", "CVq31", "hh2d", "q29", "q30", 
                                    "ISCED", "q67", "CVq67", "q40_6", "q43", "q22", "q52", "q40_7", "EurostatPopulationDensityAveragenumberofpeoplepersqua_A",
                                    "EurostatGDPpercapitainPPS2005", "EmplstatEF", "Rur_UrbEF", "hhtypeEF", "CISCED",
                                    "UnemployRate_reg", "Country_group1", "regGDPpCapita_EUR.07")]

### change names

names(socioeco_data1) <- c("WGT_TARGET", "WGT_TOTAL", "country", "country_abbr", "EQL_Region", "Gender", "Age", "Householdincome_Euro", "Children", "Employment_status", "Life_Satisfaction", 
                           "Maritial_status", "Education_level_ISCED", "Household_net_income.F", "Household_net_income.Num", "Health_1_10", "Health_1_6",
                           "Religion", "Rural_or_Countryside", "Social_life", "PopulationDensityAveragenumberofpeoplepersqua_A",
                           "GDPpercapitainPPS2005", "EmplstatEF", "Rur_UrbEF", "Household_type", "Collapsed_ISCED",
                           "UnemployRate_reg", "Country_group1", "regGDPpCapita_EUR.07")

str(socioeco_data1)


# life Satisfaction and Social Life are considered numeric!!!

socioeco_data1$country <- as.factor(socioeco_data1$country)
socioeco_data1$country_abbr <- as.factor(socioeco_data1$country_abbr)
socioeco_data1$EQL_Region <- as.factor(socioeco_data1$EQL_Region)
socioeco_data1$Gender <- as.factor(socioeco_data1$Gender)
socioeco_data1$Employment_status <- as.factor(socioeco_data1$Employment_status)

socioeco_data1$Maritial_status <- as.factor(socioeco_data1$Maritial_status)
socioeco_data1$Education_level_ISCED <- as.factor(socioeco_data1$Education_level_ISCED)
socioeco_data1$Collapsed_ISCED <- as.factor(socioeco_data1$Collapsed_ISCED)

socioeco_data1$Household_net_income.F <- as.factor(socioeco_data1$Household_net_income.F)
socioeco_data1$Health_1_10 <- as.factor(socioeco_data1$Health_1_10)
socioeco_data1$Health_1_6 <- as.factor(socioeco_data1$Health_1_6)

socioeco_data1$Religion <- as.factor(socioeco_data1$Religion)
socioeco_data1$Rural_or_Countryside <- as.factor(socioeco_data1$Rural_or_Countryside)

socioeco_data1$EmplstatEF <- as.factor(socioeco_data1$EmplstatEF)
socioeco_data1$Rur_UrbEF <- as.factor(socioeco_data1$Rur_UrbEF)
socioeco_data1$Household_type <- as.factor(socioeco_data1$Household_type)

socioeco_data1$Country_group1 <- as.factor(socioeco_data1$Country_group1)

###########################################################################

# nature.data_mod1 <- Dataset_total[, 60:93]

nature.data1 <- Dataset_total[, 104:156]

str(nature.data1)

names(nature.data1)

nature.data1$TRI.mean.cat <- as.factor(nature.data1$TRI.mean.cat)
nature.data1$Wolf_dummy <- as.factor(nature.data1$Wolf_dummy)
nature.data1$Bear_dummy <- as.factor(nature.data1$Bear_dummy)

###########################################################################

country_dummmies <- Dataset_total[, 3:33]

str(country_dummmies)

country_dummmies <- as.data.frame(lapply(country_dummmies, factor) )

#########################################################################


test_DE <- Dataset_total[Dataset_total$country_abbr == "DE",]

summary(q29 ~ NUTS_ID, data = test_DE)

###########################################################################
########             Open NUTS Shapefiles         #########################
###########################################################################

# open the cleaned (with removed / merged polygons) NUTS Shapefile that 
# fits the EQL 2007 Regions 

# check if file is there
file.exists("C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/NUTS2_EQL_2007.shp")

# ogrInfo("C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/NUTS2_EQL_2007.shp")

# Nuts.EQL2007_shp <- readOGR("C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles", "NUTS2_EQL_2007") 

Nuts.EQL2007_shp <- readShapePoly("C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/NUTS2_EQL_2007.shp") 

# Projection: GCS_ETRS_1989

proj4string(Nuts.EQL2007_shp) <- "+proj=longlat +ellps=GRS80 +no_defs"

# "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

plot(Nuts.EQL2007_shp)

### Transform the NUTS shapefile to LAEA Projection

# Nuts_EQL2007_LAEA <- spTransform(Nuts.EQL2007_shp, CRS = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

# Nuts_EQL2007_newproj <- spTransform(Nuts.EQL2007_shp, CRS = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


########################################################################

# open the cleaned (with removed / merged polygons) NUTS Shapefile that 
# fits the EQL 2012 Regions

Nuts2_EQL_2012 <- "C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/data/NUTS2013_EQL2012_clean.shp"

Nuts.EQL2012_shp <- readShapeSpatial(Nuts2_EQL_2012)

names(Nuts.EQL2012_shp)

# Projection: GCS_ETRS_1989

proj4string(Nuts.EQL2012_shp) <- "+proj=longlat +ellps=GRS80 +no_defs"

# "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

plot(Nuts.EQL2012_shp)

# Change Names

names(Nuts.EQL2012_shp)

names(Nuts.EQL2012_shp) <- c("NUTS_ID", "STAT_LEVL", "SHAPE_AREA", "SHAPE_LEN",
                             "CNTRY_CO", "NUTS_ID_1", "EQL_Region", "EQL_NAME",  
                             "NUTS_NAME", "Area_Size", "IUCN_Perc_Cover")  
# Area Size (km2)
# Percent Cover --> All ICUN Areas

### Transform the NUTS shapefile to LAEA Projection

# Nuts_EQL2012_LAEA <- spTransform(Nuts.EQL2012_shp, CRS = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

# Nuts_EQL2012_newproj <- spTransform(Nuts.EQL2012_shp, CRS = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


################################################################################

################################

# WorldMap <- getData("countries")

# WorldMap <- get_map(location = 'Europe', zoom = 4)

file_path <- "C:/Users/jmethorst/Documents/GIS Files/Global Country Files/ne_10m_admin_0_countries"

WorldMap <- readOGR(file_path, "ne_10m_admin_0_countries")

plot(WorldMap)

str(WorldMap@data)

unique(WorldMap@data$ISO_A2)

WorldMap@data[WorldMap@data$NAME == "Norway", "ISO_A2"]

WorldMap@data[WorldMap@data$NAME == "Kosovo", "ISO_A2"]

# select EU Countries
non.NUTS_countries <- c( "BH", "JO", "IL", "IR", "AE", "IQ", "KW", "LB", "OM", "QA", "SA", "SY", "PA",   # Middle East
                         "BY", "AL", "AD", "BA", "FO", "KV", "RS", "LI", "CH", "NO", "MK", "YU", "XK", "GB", "TR",     # Europe
                         "MD", "MC", "MK", "RO", "RU", "HR", "HU", "BG", "MT",  "UA", "ME", "FI", "GR", "IS",  # Europe
                         "AM", "AF", "EL", "AZ", "GE", "KZ", "KG", "TJ", "TM", "UZ",   # Central Asia
                         "DZ", "EG", "LY", "MA", "TN", "MR", "ML", "EH")                          # Nothern Afrika


## Subset world map to European countries ##
EuropeMap <- WorldMap[WorldMap@data$ISO_A2 %in% non.NUTS_countries | WorldMap@data$NAME == "Kosovo" | WorldMap@data$NAME == "Norway" , ]

str(EuropeMap@data)

plot(EuropeMap)

######################

## Create the clipping polygon
# CP <- as(extent(-30, 65, 25, 75), "SpatialPolygons")
# proj4string(CP) <- CRS(proj4string(WorldMap))

# plot(CP)

## Clip the map
# EuropeMap <- gIntersection(WorldMap, CP, byid=TRUE)

# plot(EuropeMap)

############################################################################

## create data frame with polygon information for ggplot2

# add to data a new column termed "id" composed of the rownames of data
# EuropeMap@data$id <- rownames(EuropeMap)

# create a data.frame from our spatial object
EU_Coastline_id <- fortify(EuropeMap, region = "NAME")

str(EU_Coastline_id)

# merge the "fortified" data with the data from our spatial object
# EU_Coastline.DF <- merge(EU_Coastline_id, EU_Coastline_shp_simpl@dat, by = "id")


ggplot(data = EU_Coastline_id, aes(x=long, y=lat, group = group)) +
  geom_polygon(fill = "lightgrey") +
  geom_path(color = "black") +
  coord_cartesian(xlim = c(-30, 50), ylim = c(30, 75) )


##################################################################################

###### open European Coastline Shapefile

EU_Coastline_File <- "C:/Users/jmethorst/Documents/GIS Files/EEA/Europe_coastline_shapefile/Europe_coastline_poly.shp"

EU_Coastline_shp <- readShapeSpatial(EU_Coastline_File)

# Projection: GCS_ETRS_1989

proj4string(EU_Coastline_shp) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

### simplify
?gSimplify
EU_Coastline_shp_simpl <- gSimplify(EU_Coastline_shp, tol= 8000, topologyPreserve=TRUE)

plot(EU_Coastline_shp_simpl)


#################################################################################


##### add the EQL 2007 survey subset to the NUTS Shapefile

## Column with Life Satisfaction is:
## ariable = q29	
## Variable label = Q29 All things considered, how satisfied would you say you are with your life these days?

names(dat.mod1)

### Calculate Mean for each EQL Region (NUTS Regions)

LS_aggr <- aggregate(Life_Satisfaction ~ EQL_Region, FUN = mean, data = dat.mod1)

LS_aggr.df <- as.data.frame(LS_aggr)

str(LS_aggr.df)

n_aggr <- aggregate(Life_Satisfaction ~ EQL_Region, FUN = length, data = dat.mod1)

n_aggr.df <- as.data.frame(n_aggr)

n_aggr.df[n_aggr.df$EQL_Region == 234, ] # 64

################################

## using the merge function to merge the Mean life satisfaction values
## to the NUTS shapefiles

Nuts.EQL2007_LS_merge <- merge(Nuts.EQL2007_shp, LS_aggr.df, by = "EQL_Region")

names(Nuts.EQL2007_LS_merge@data)

Nuts.EQL2007_LS_merge@data[, c(1, 2, 10, 11)]

# DE13 = DE Freiburg = 234

########################################################################

## Plot a map with NUTS REgions and Mean Life Satisfaction 


library(RColorBrewer)

# my.palette <- brewer.pal(n = 8, name = "Blues")

# colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
#                    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# data("worldMapEnv")

# eu_coastline <- list("sp.polygons", EU_Coastline_shp_simpl, col = "lightgrey")


############## plot the Maps


## create data frame with polygon information for ggplot2

# add to data a new column termed "id" composed of the rownames of data
Nuts.EQL2007_LS_merge@data$id <- rownames(Nuts.EQL2007_LS_merge@data)

# create a data.frame from our spatial object
Nuts.EQL2007_Mean.LS.fort <- fortify(Nuts.EQL2007_LS_merge, region = "id")

str(Nuts.EQL2007_Mean.LS.fort)

# merge the "fortified" data with the data from our spatial object
Nuts.EQL2007_Mean.LS.df <- merge(Nuts.EQL2007_Mean.LS.fort, Nuts.EQL2007_LS_merge@data, by = "id")

str(Nuts.EQL2007_Mean.LS.df)

## plot

# pdf("Life-Satisfaction_2007.pdf", width = 10, height = 8)

jpeg("Life-Satisfaction_2007.jpeg", width = 1200, height = 1000,
     quality = 100)

EQL2007_Mean.LifeSat <- ggplot() +
  
  geom_polygon(data = EU_Coastline_id, aes(x=long, y=lat, group = group), fill = "lightgrey", col = "gray28") +

  geom_polygon(data = Nuts.EQL2007_Mean.LS.df, aes(x=long, y=lat, group = group,
                                                        fill = Life_Satisfaction), col = "gray28")  +

  ggtitle("EQL 2007 Mean Life Satisfaction") +
  
  scale_fill_gradient2(low = "darkred", mid = "white", high = "dodgerblue4",
                       breaks = c(5, 6, 7, 8, 9, 10),
                       labels = c("5", "6", "7", "8", "9", "10"),
                       midpoint = 6.5,
                       name = "Mean") +
  # coord_equal() +
  coord_cartesian(xlim = c(-25, 37), ylim = c(28, 70)) +
  theme( axis.title = element_blank(), axis.text = element_blank(),
         # axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(colour = "gray28", fill = NA, size = 2, linetype="solid"),
        plot.title = element_text(hjust = 0.5))
   

print(EQL2007_Mean.LifeSat) 

dev.off()


#############################################################################


##########################################################################

##### add the EQL survey subset to the NUTS Shapefile

### First remove the unusable answers
# Value = 98.0	Label = (Don't know)
# Value = 99.0	Label = (refusal)

no_answer_id <- which(eql_2012_subset_1$Y11_Q30 %in% c(98, 99))

eql_2012_subset_2 <- eql_2012_subset_1[-no_answer_id,]

str(eql_2012_subset_2) # 38456 obs. of  4 variables


### Calculate Mean for each EQL REgion

Y11_Q30_aggr <- aggregate(Y11_Q30 ~ EQL_Region, FUN = mean, data = eql_2012_subset_2)

# create data frame
Y11_Q30_aggr.df <- as.data.frame(Y11_Q30_aggr)

str(Y11_Q30_aggr.df)

names(Y11_Q30_aggr.df) <- c("EQL_Region", "Y11_Q30_mean")

################################

##### using the merge function

names(Nuts.EQL2012_shp)

Nuts.EQL2012_Y11_Q30_merge <- merge(Nuts.EQL2012_shp, Y11_Q30_aggr.df, by = "EQL_Region")

summary(Nuts.EQL2012_Y11_Q30_merge$Y11_Q30_mean)


################################################################################

library(RColorBrewer)

my.palette <- brewer.pal(n = 8, name = "Blues")

colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

data("worldMapEnv")

eu_coastline <- list("sp.polygons", EU_Coastline_shp_simpl, col = "lightgrey")

pdf(file= "EQL2012_NUTS_clean1.pdf")

spplot(test_1, c("Y11_Q30"), names.attr = c("Mean Life Satisfation"),
       col = "transparent",  main = "Mean Life Satisfaction EQL 2012",
       col.regions = my.palette, cuts = 6, sp.layout = eu_coastline)

dev.off()


###############################################################################

# Nuts.EQL2012_Y11_Q30_merge

str(Nuts.EQL2012_Y11_Q30_merge@data)

## create data frame with polygon information for ggplot2

# add to data a new column termed "id" composed of the rownames of data
Nuts.EQL2012_Y11_Q30_merge@data$id <- rownames(Nuts.EQL2012_Y11_Q30_merge@data)

# create a data.frame from our spatial object
Nuts.EQL2012_LifeSatisf <- fortify(Nuts.EQL2012_Y11_Q30_merge, region = "id")

# merge the "fortified" data with the data from our spatial object
Nuts.EQL2012_LifeSatisf.DF <- merge(Nuts.EQL2012_LifeSatisf, Nuts.EQL2012_Y11_Q30_merge@data, by = "id")

str(Nuts.EQL2012_LifeSatisf.DF)


## plot

 pdf("Life-Satisfaction_2012.pdf", width = 10, height = 8)

# jpeg("Life-Satisfaction_2012.jpeg", width = 1200, height = 1000,
#     quality = 100)

EQL2007_Mean.LifeSat <- ggplot() +
  
  geom_polygon(data = EU_Coastline_id, aes(x=long, y=lat, group = group), fill = "lightgrey", col = "gray28") +
  
  geom_polygon(data = Nuts.EQL2012_LifeSatisf.DF, aes(x=long, y=lat, group = group,
                                                   fill = Y11_Q30_mean), col = "gray28")  +
  
  ggtitle("EQL 2012 Mean Life Satisfaction") +
  
  scale_fill_gradient2(low = "darkred", mid = "white", high = "dodgerblue4",
                       breaks = c(5, 6, 7, 8, 9, 10),
                       labels = c("5", "6", "7", "8", "9", "10"),
                       midpoint = 6.5,
                       name = "Mean") +
  # coord_equal() +
  coord_cartesian(xlim = c(-25, 37), ylim = c(28, 70)) +
  theme( axis.title = element_blank(), axis.text = element_blank(),
         # axis.line = element_line(colour = "black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         legend.position = "bottom",
         panel.border = element_rect(colour = "gray28", fill = NA, size = 2, linetype="solid"),
         plot.title = element_text(hjust = 0.5))


print(EQL2007_Mean.LifeSat) 

dev.off()
