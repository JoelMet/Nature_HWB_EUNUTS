
library(maps)
library(rgdal)
library(gbm)
library(raster)
library(rgeos)
library(maptools)
library(ncdf4)
library(ggplot2)

########## Sort and merge Data for 2007

######### Effects of NAture on Human Well-Being

#####################################################################################

## Load Panel Data from European Quality of Life Survey 2007 (EQL 2007)

eql_2007_sub <- read.table("C:/Users/jmethorst/Documents/R analyses/Nature_HWB_EUNUTS/EQL_2007_subset.txt",
                           sep="\t", header=TRUE)

names(eql_2007_sub)

str(eql_2007_sub) # 30562 obs. of  60 variables

summary(eql_2007_sub)

############### remove NAs

summary(eql_2007_sub$EQL_Region)

unique(na.exclude(eql_2007_sub1$EQL_Region))

eql_2007_sub1 <- eql_2007_sub[complete.cases(eql_2007_sub$EQL_Region),]

str(eql_2007_sub1) # 30562 obs. of  60 variables


### First remove the unusable answers (values range from 1 - 10)
# Value = 1	Label = 1 Very dissatisfied
# Value = 10	Label = 10 Very satisfied
# Value = 99.0	Label = DK

summary(eql_2007_sub1$q29)

eql_2007_sub.1 <- eql_2007_sub1[-which(eql_2007_sub1$q29 %in% 99),]

summary(eql_2007_sub.1$q29)

str(eql_2007_sub.1) # 30430 obs. of  60 variables

## check how man row have EQL region "212"

# str(eql_2007_sub.1[eql_2007_sub.1$EQL_Region == 212, ]) # 5124

# summary(eql_2007_sub.1[eql_2007_sub.1$EQL_Region == 212, ])

## I assume here that 212 must be Bratislava (NUTS SK01)


###########################################################################

### ad NUTS ID Column to EQL data

matchNUTS_EQL <- read.csv(file = "C:/Users/jmethorst/Documents/EQL Data 2007/EQL_2007_match_NUTS.csv",
                           header = TRUE, sep = ";")

str(matchNUTS_EQL)

length(unique(eql_2007_sub.1$EQL_Region)) # 250

length(unique(matchNUTS_EQL$EQL_Region)) # 250 

setdiff(eql_2007_sub.1$EQL_Region, matchNUTS_EQL$EQL_Region)
# no difference

# transform to factor 

matchNUTS_EQL$EQL_Region <- as.factor(matchNUTS_EQL$EQL_Region)

str(eql_2007_sub.1)

eql_2007_sub.1$EQL_Region <- as.factor(eql_2007_sub.1$EQL_Region)

### merge data sets

eql_2007_sub.2 <- merge(eql_2007_sub.1, matchNUTS_EQL[, c(1, 3)], by = "EQL_Region")

# eql_2007_sub.2 <- dataframe(eql_2007_sub.1[, 1:59], matchNUTS_EQL[, c(1, 2, 3)], by.x = "EQL_Region")


names(eql_2007_sub.2)

length(unique(eql_2007_sub.2$EQL_Region)) # 250

length(unique(eql_2007_sub.2$NUTS_ID)) # 250

str(eql_2007_sub.2) # 30430 obs. of  62

summary(eql_2007_sub.2)


#############################################################################

################### load other economic data

### EUROSTAT Regional GDP per Capita in EUR (Mio.)

GDPpCapita_EUR <- read.table("C:/Users/jmethorst/Documents/Economics Data/EUROSTAT_t_nama_reg/Regioanl_GDP_Mio.Eur/tgs00003_new.txt",
                             header = TRUE, sep = "\t")


names(GDPpCapita_EUR)

regGDPpCapita_EUR.2007 <-  GDPpCapita_EUR[, c(3, 9)]

# change names
names(regGDPpCapita_EUR.2007) <- c("NUTS_ID", "regGDPpCapita_EUR.07")

regGDPpCapita_EUR.2007$NUTS_ID[regGDPpCapita_EUR.2007$NUTS_ID == "<NA>"] <- NA

## change to numeric
regGDPpCapita_EUR.2007$regGDPpCapita_EUR.07 <- as.numeric(as.character(regGDPpCapita_EUR.2007$regGDPpCapita_EUR.07))

summary(regGDPpCapita_EUR.2007)

?aggregate

## agregate the value to fit the NUTS REgions length
regGDPpCapita_EUR.2007 <- aggregate(regGDPpCapita_EUR.07 ~ NUTS_ID, data = regGDPpCapita_EUR.2007, FUN = mean)

str(regGDPpCapita_EUR.2007) # NUTS 251

###################

UnemployRate_reg <- read.table("C:/Users/jmethorst/Documents/Economics Data/EUROSTAT_Arbeitslosenquote.reg_t_employ/Unemployment_total.txt",
                             header = TRUE, sep = "\t")

str(UnemployRate_reg)

names(UnemployRate_reg)

UnemployRate_reg.2007 <- UnemployRate_reg[, c(5, 10)]

names(UnemployRate_reg.2007) <- c("NUTS_ID", "UnemployRate_reg")

UnemployRate_reg.2007$UnemployRate_reg <- as.numeric(as.character(UnemployRate_reg.2007$UnemployRate_reg))

summary(UnemployRate_reg.2007)

##########

UnemployRate_reg.2007 <- aggregate(UnemployRate_reg ~ NUTS_ID, data = UnemployRate_reg.2007, FUN = mean)

length(unique(UnemployRate_reg.2007$NUTS_ID))

str(UnemployRate_reg.2007)

### merge with data

?merge

eql_2007_sub.2 <- merge(eql_2007_sub.2, UnemployRate_reg.2007, by = "NUTS_ID", all.x = TRUE)

names(eql_2007_sub.2)

str(eql_2007_sub.2) # 30430 obs. of  62 variables


##########################

Popdensity_reg <- read.table("C:/Users/jmethorst/Documents/Economics Data/tgs00024.tsv/Eurostat.Pop_densityNUTS2.txt",
                               header = TRUE, sep = "\t")

str(Popdensity_reg)

names(Popdensity_reg)

Popdensity_reg.2007 <- Popdensity_reg[, c(3, 8)]

names(Popdensity_reg.2007) <- c("NUTS_ID", "Popdensity_reg")

Popdensity_reg.2007$Popdensity_reg <- as.numeric(as.character(Popdensity_reg.2007$Popdensity_reg))

str(Popdensity_reg.2007)

#####

Popdensity_reg.2007 <- aggregate(Popdensity_reg ~ NUTS_ID, data = Popdensity_reg.2007, FUN = mean)

length(unique(Popdensity_reg.2007$NUTS_ID))

str(UnemployRate_reg.2007)

### merge with data


eql_2007_sub.2 <- merge(eql_2007_sub.2, Popdensity_reg.2007, by = "NUTS_ID", all.x = TRUE)

names(eql_2007_sub.2)

str(eql_2007_sub.2) # 30430 obs. of  62 variables


####################################################################################

## Load the Nature Indicators summarized for NUTS Regions


######## Landscape Heterogeneity

LC_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Corine Landcover/Corine2006_NUTS2006_diversity.txt",
                           sep="\t", header=TRUE)

names(LC_2007)

Land.Hetero_2007 <- LC_2007[ , c(9, 11, 12, 13, 14, 15, 16, 17, 18)]

# NUTS_ID, EQL_Region, EQL_Name, Corine2006_H, Corine2006_simp     
# Corine2006_nat.H, Corine2006_nat.simp, Corine2006_H.even, Corine2006_nat.H.even

names(Land.Hetero_2007)

########################################

####### Bird Diversity

Bird.Birdlife_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/BirdSpecies Tables/Nuts.EQL2007_Birdlife.df.txt",
                      sep="\t", header=TRUE)

Bird.EBBC_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/BirdSpecies Tables/Nuts.EQL2007_BirdSpRich.df.txt",
                             sep="\t", header=TRUE)

names(Bird.Birdlife_2007)

summary(Bird.Birdlife_2007)

summary(Bird.EBBC_2007) # there are some 0 values

Bird.EBBC_2007[Bird.EBBC_2007$EBBA1_SpR == 0,]
# CY00 Cyprus
# ES70 ES Canarias
# MT00 MALTA
### --> these REgions are not covered by EBBA data

## change zero value to NA

Bird.EBBC_2007$EBBA1_SpR[Bird.EBBC_2007$EBBA1_SpR == 0] <- NA

Bird.EBBC_2007$EBBA1_AreaWeighted_SpR[Bird.EBBC_2007$EBBA1_AreaWeighted_SpR == 0] <- NA

########

Bird.Sp.Rich_2007 <- cbind(Bird.Birdlife_2007[, c(9, 13)], Bird.EBBC_2007[, c(11, 12)] ) 

names(Bird.Sp.Rich_2007)

#  "EQL_Region"                   
# "Birdlife_SpR"           "EBBA1_SpR"              "EBBA1_AreaWeighted_SpR"


########################################

########## Mammal Diversity

Mammals_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Mammals/Nuts.EQL2007_Mammals.txt",
                                 sep="\t", header=TRUE)

names(Mammals_2007)

Megafauna_2007 <- Mammals_2007[, c(9, 11, 12, 13, 14)]

names(Megafauna_2007)

# [1] "EQL_Region"          "Mammal_Spec.Rich"    "Bear_dummy"         
# [4] "Wolf_dummy"          "Megafauna_Spec.Rich"


#########################################

############ Tree Diversity

Trees_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Tree Species/Tree_SpRich_2007.txt",
                           sep="\t", header=TRUE)

names(Trees_2007)

Tree_Sp.Rich_2007 <- Trees_2007[, c(9, 65)]

names(Tree_Sp.Rich_2007)

# "EQL_Region"  "Tree_Sp.Rich"

summary(Tree_Sp.Rich_2007)

Tree_Sp.Rich_2007[Tree_Sp.Rich_2007$Tree_Sp.Rich == 0, ]
# NUTS_ID  EQL_Region    EQL_Name
# ES70         68 ES Canarias            0

# change zero value so NA
Tree_Sp.Rich_2007$Tree_Sp.Rich[Tree_Sp.Rich_2007$Tree_Sp.Rich == 0] <- NA


########### Tree Diversity form Mauri et al 2017

Mauri_Trees_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Tree Species/Mauri etal 2017/Tree_SpR_NUTS2007.txt",
                         sep="\t", header=TRUE)

names(Mauri_Trees_2007)

Mauri_Trees_2007.sub <- Mauri_Trees_2007[, c( 9, 66)]

summary(Mauri_Trees_2007.sub)

Mauri_Trees_2007.sub[Mauri_Trees_2007.sub$Mauri.Tree_SpR == 0, ]

# NUTS_ID EQL_Region                          EQL_Name 
#     BE10         12 BE Brussels hoofdstedelijk gewest              
#     BE25         13                BE West-Vlaanderen             
#     BE31         16                 BE Brabant Wallon             
#     EL43         51                          EL Kriti             
#     MT00        172                             MALTA

#### change Zero value to NA, because we do not have records in these NUTS regions

Mauri_Trees_2007.sub$Mauri.Tree_SpR[Mauri_Trees_2007.sub$Mauri.Tree_SpR == 0] <- NA


###########################################

#### Elevation / Terrain Roughness

Elevation_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Elevation/Elevation_NUTS2006.txt",
                         sep="\t", header=TRUE)

names(Elevation_2007)

Terrain_2007 <- Elevation_2007[, c(9, 11, 12, 13, 14, 15, 16)] 

names(Terrain_2007)

#   "EQL_Region"         "Elevation_sd"   
# "Elevation_range" "Elevation_mean"  "TRI.mean"        "TRI.mean.cat" 

###########################################

#### Climate

Climate_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Climate/Climate_NUTS2007.txt",
                             sep="\t", header=TRUE)

names(Climate_2007)

Climate_2007_sub <- Climate_2007[, c(9, 11, 12, 13, 14, 15, 16, 17, 18, 19 , 20, 21, 22, 23, 24)]

names(Climate_2007_sub)

#  [1] "EQL_Region"              "tmean.yearmean"          "tmean.yearrange"        
# [4] "tmean.yearmax"           "tmean.yearmin"           "prec.yearmean"          
# [7] "prec.yearrange"          "prec.yearmax"            "prec.yearmin"           
# [10] "tg_0.25deg_HDD"          "tg_0.25deg_HDD.yearmean" "tg_0.25deg_CDD"         
# [13] "tg_0.25deg_CDD.yearmean" "tg_0.25deg_GDD"          "tg_0.25deg_GDD.yearmean" 

###########################################

######### area size

# Nuts2_EQL_2007 <- "C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/"

Nuts.EQL2007_shp <- readShapePoly("C:/Users/jmethorst/Documents/NUTS Regions Shapefiles/NUTS_2013_01M_SH/NUTS_2013_01M_SH/Final Shapefiles/NUTS2_EQL_2007.shp") 

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


#########################################

######## NATURA 2000

natura.2000 <- read.table("C:/Users/jmethorst/Documents/R analyses/Natura2000/Natura2000.ter_2007.txt",
                          sep="\t", header=TRUE)

names(natura.2000)

natura.2000.sub <- natura.2000[, c(9, 12, 14)]

names(natura.2000.sub)
# [1]  "EQL_Region"           
# [4] "Natura_AreaSize_km2" "Natura_Perc_Cover"

summary(natura.2000.sub)

#######################################

## distanc to coast etc.

coast <- read.table("C:/Users/jmethorst/Documents/R analyses/Coast/Coast_NUTS2007.txt",
                          sep="\t", header=TRUE)

names(coast)

coast.sub <- coast[, c(1, 11, 12)]

names(coast.sub)

# "EQL_Region"          "Dist_centroid.coast" "Coast_length.km" 

################################################################################

###################### Merge all the Data

names(Bird.Sp.Rich_2007)

Nature_data_2007 <- cbind("EQL_Region" = Nuts.EQL2007_shp@data[, 9], Bird.Sp.Rich_2007[, c(2, 3,4)], Megafauna_2007[,2:5],
                          "Tree.Sp.Rich" = Tree_Sp.Rich_2007[,2], "Mauri.Tree_SpR" = Mauri_Trees_2007.sub[,2], 
                          Land.Hetero_2007[,2:9], 
                          area_2007, Terrain_2007[,2:7], Climate_2007_sub[,2:15],  
                          natura.2000.sub[,2:3], coast.sub[,2:3])

names(Nature_data_2007)

boxplot(Nature_data_2007[,1:27])

write.table(Nature_data_2007, "Nature_data_2007.txt", sep="\t")

Nature_data_2007 <- read.table("Nature_data_2007.txt", header = TRUE, sep = "\t")

#############################################################################

### merge EQL Data with NAture Data into one dataframe
?merge

length(unique(eql_2007_sub.2$EQL_Region)) # 250

length(unique(Nature_data_2007$EQL_Region)) # 250

length(unique(Nature_data_2007$NUTS_ID)) # 250

## find difference

# setdiff(eql_2007_sub.1$EQL_Region, Nature_data_2007$EQL_Region)
# 212
# for some reason there is a 212 in the data set!
# but in the Meta Data Table there is no 212 listes under "regionNUTS2EF"

# eql_2007_sub.1[eql_2007_sub.1$EQL_Region == 212, ]

## remove the NUTS_ID for Bratislava

# Nature_data_2007 <- Nature_data_2007[-which(Nature_data_2007$NUTS_ID == "SK01"), ]


################################

Dataset_total <- merge(eql_2007_sub.2, Nature_data_2007, by.x = "EQL_Region")

str(Dataset_total) # 30430 obs. of  98 variables

summary(Dataset_total)

names(Dataset_total)

## Column with Question about Life Satisfaction is:
## Variable = q29	
## Variable label = Q29 All things considered, how satisfied would you say you are with your life these days?

#### testing first model 

# respose Variabel = Life Satisfaction (q29)

summary(Dataset_total$q29)
hist(Dataset_total$q29)

# explanatory Variable = 
# hh2a = Gender, factor
# hh2b = Age , factor
summary(Dataset_total$hh2b)
# CVhh2b = age = numeric
summary(Dataset_total$CVhh2b)

hist(Dataset_total$CVhh2b)

# hh2d = employment , factor
summary(Dataset_total$hh2d)

# q30 = maritial status , factor
summary(Dataset_total$q30)

Dataset_total$q30[Dataset_total$q30 == 5] <- NA
# CVq31 = Children
summary(Dataset_total$CVq31)

hist(Dataset_total$CVq31)

Dataset_total$CVq31[Dataset_total$CVq31 == 999] <- NA

# ISCED = education level, factor
summary(Dataset_total$ISCED)

# q67 = income (monthly household net income), factor
# CVq67 = household net income, numeric
summary(Dataset_total$CVq67) # -1 = refusal
Dataset_total$CVq67[Dataset_total$CVq67 == -1] <- NA

hist(log(Dataset_total$CVq67))

# CV6768o = Householdincome in Euro, numeric
summary(Dataset_total$CV6768o)
hist(Dataset_total$CV6768o)

# q40_6 = health (1 = very dissatisfied, 10 = very satisfied), factor
summary(Dataset_total$q40_6)
Dataset_total$q40_6[Dataset_total$q40_6 == 99] <- NA

# q43 = health (1= very good, 2 = good, 3 = fait, etc.), factor 

summary(Dataset_total$q43)
Dataset_total$q43[Dataset_total$q43 == 6] <- NA

# q22 = Religion
summary(Dataset_total$q22)

Dataset_total$q22[Dataset_total$q22 == 9] <- NA

# q52 = Rural or Countryside
summary(Dataset_total$q52)

Dataset_total$q52[Dataset_total$q52 == 5] <- NA

# q40_7 = Your social life
summary(Dataset_total$q40_7)

Dataset_total$q40_7[Dataset_total$q40_7 == 99] <- NA

# EmplstatEF = Employment Status, factor
summary(Dataset_total$EmplstatEF)

# "EurostatPopulationDensityAveragenumberofpeoplepersqua" 2005

summary(Dataset_total$EurostatPopulationDensityAveragenumberofpeoplepersqua)

# "EurostatPopulationDensityAveragenumberofpeoplepersqua_A" 2006


# EurostatGDPpercapitainPPS2005 = GDP per Capita
summary(Dataset_total$EurostatGDPpercapitainPPS2005)

hist(log(Dataset_total$EurostatGDPpercapitainPPS2005))

# Rur_UrbEF = 1 = countryside/smalle town, 2 = city, urban area; factor
summary(Dataset_total$Rur_UrbEF)

# hhtypeEF = household type, factor
summary(Dataset_total$hhtypeEF)


##############################################################################


## save data

write.table(Dataset_total, "Dataset_total_2007.txt", sep="\t")
