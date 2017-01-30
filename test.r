
########## Pilot Analysis

######### Effects of NAture on Human Well-Being

#####################################################################################

## Load Panel Data from European Quality of Life Survey 2007 (EQL 2007)

eql_2007_sub <- read.table("C:/Users/jmethorst/Documents/R analyses/EQL_2007_subset.txt",
                           sep="\t", header=TRUE)

names(eql_2007_sub)


####################################################################################

## Load the Nature Indicators summarized for NUTS Regions


######## Landscape Heterogeneity

LC_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Corine Landcover/Corine2006_NUTS2006_diversity.txt",
                           sep="\t", header=TRUE)

str(LC_2007)

Land.Hetero_2007 <- LC_2007[ , c(1, 9, 10, 11, 12, 13, 14, 15, 16)]

# NUTS_ID, EQL_Region, EQL_Name, Corine2006_H, Corine2006_simp     
# Corine2006_nat.H, Corine2006_nat.simp, Corine2006_H.even, Corine2006_nat.H.even

names(Land.Hetero_2007)

########################################

####### Bird Diversity

Bird.Birdlife_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/BirdSpecies Tables/Nuts.EQL2007_Birdlife.df.txt",
                      sep="\t", header=TRUE)

Bird.EBBC_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/BirdSpecies Tables/Nuts.EQL2007_BirdSpRich.df.txt",
                             sep="\t", header=TRUE)

str(Bird.Birdlife_2007)

str(Bird.EBBC_2007)


Bird.Sp.Rich_2007 <- cbind(Bird.Birdlife_2007[, c(1, 9, 10, 11)], Bird.EBBC_2007[, c(11, 12)] ) 

names(Bird.Sp.Rich_2007)

# "NUTS_ID"                "EQL_Region"             "EQL_Name"              
# "Birdlife_SpR"           "EBBA1_SpR"              "EBBA1_AreaWeighted_SpR"


########################################

########## Mammal Diversity

Mammals_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Mammals/Nuts.EQL2007_Mammals.txt",
                                 sep="\t", header=TRUE)

str(Mammals_2007)

Megafauna_2007 <- Mammals_2007[, c(1, 9, 10, 11, 12, 13)]

names(Megafauna_2007)

# "NUTS_ID"             "EQL_Region"          "EQL_Name"           
# "Megafauna_Spec.Rich" "Bear_dummy"          "Wolf_dummy" 


#########################################

############ Tree Diversity

Trees_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Tree Species/Tree_SpRich_2007.txt",
                           sep="\t", header=TRUE)

str(Trees_2007)

Tree_Sp.Rich_2007 <- Trees_2007[, c(1, 9, 10, 65)]

names(Tree_Sp.Rich_2007)

# "NUTS_ID"      "EQL_Region"   "EQL_Name"     "Tree_Sp.Rich"

###########################################

#### Elevation / Terrain Roughness

Elevation_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Elevation/Elevation_NUTS2006.txt",
                         sep="\t", header=TRUE)

str(Elevation_2007)

Terrain_2007 <- Elevation_2007[, c(1, 9, 10, 11, 12, 13, 14, 15)] 

names(Terrain_2007)

# "NUTS_ID"         "EQL_Region"      "EQL_Name"        "Elevation_sd"   
# "Elevation_range" "Elevation_mean"  "TRI.mean"        "TRI.mean.cat" 

###########################################

#### Climate

Climate_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Climate/Climate_NUTS2007.txt",
                             sep="\t", header=TRUE)

str(Climate_2007)

Climate_2007_sub <- Climate_2007[, 9:24]

names(Climate_2007_sub)

# "EQL_Region"              "EQL_Name"                "tmean.yearmean"         
# "tmean.yearrange"         "tmean.yearmax"           "tmean.yearmin"          
# "prec.yearmean"           "prec.yearrange"          "prec.yearmax"           
# "prec.yearmin"            "tg_0.25deg_HDD"          "tg_0.25deg_HDD.yearmean"
# "tg_0.25deg_CDD"          "tg_0.25deg_CDD.yearmean" "tg_0.25deg_GDD"         
# "tg_0.25deg_GDD.yearmean"

###########################################

######### area size

Nuts2_EQL_2007 <- "C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/"

Nuts.EQL2007_shp <- readShapePoly("C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/NUTS2_EQL2007.shp") 

names(Nuts.EQL2007_shp)

# Projection: GCS_ETRS_1989

proj4string(Nuts.EQL2007_shp) <- "+proj=longlat +ellps=GRS80 +no_defs"

Nuts_EQL2007_newproj <- spTransform(Nuts.EQL2007_shp, CRS = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

library(geosphere)

# area in square meters

a.2007 <- areaPolygon(Nuts_EQL2007_newproj)

# area in square km
a.km.2007 <- a.2007/1000000

area_2007 <- cbind(a.2007, a.km.2007)

str(area_2007)

################################################################################

###################### Merge all the Data

names(Bird.Sp.Rich_2007)

Nature_data_2007 <- cbind(Bird.Sp.Rich_2007, Megafauna_2007[,4:6],
                          Tree_Sp.Rich_2007[,4], Land.Hetero_2007[,4:9],
                          Terrain_2007[,4:8], Climate_2007_sub[,3:16], area_2007)

names(Nature_data_2007)

boxplot(Nature_data_2007[,1:35])

write.table(Nature_data_2007, "Nature_data_2007.txt", sep="\t")
