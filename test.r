
library(maps)
library(rgdal)
library(gbm)
library(raster)
library(rgeos)
library(maptools)
library(ncdf4)
library(ggplot2)

########## Pilot Analysis

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

eql_2007_sub.2 <- merge(eql_2007_sub.1, matchNUTS_EQL[, c(1, 2, 3)], by = "EQL_Region")

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

UnemployRate_reg.2007 <- aggregate(UnemployRate_reg ~ NUTS_ID, data = UnemployRate_reg.2007, FUN = mean)

length(unique(UnemployRate_reg.2007$NUTS_ID))


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

names(Trees_2007)

Tree_Sp.Rich_2007 <- Trees_2007[, c(1, 9, 10, 65)]

names(Tree_Sp.Rich_2007)

# "NUTS_ID"      "EQL_Region"   "EQL_Name"     "Tree_Sp.Rich"

summary(Tree_Sp.Rich_2007)

Tree_Sp.Rich_2007[Tree_Sp.Rich_2007$Tree_Sp.Rich == 0, ]
# NUTS_ID  EQL_Region    EQL_Name
# EL30         50      EL Attiki  
# EL43         51      EL Kriti    
# ES70         68      ES Canarias  

# 102    ES61         52 ES Andalucia            0 ?????

# change zero value so NA
Tree_Sp.Rich_2007$Tree_Sp.Rich[Tree_Sp.Rich_2007$Tree_Sp.Rich == 0] <- NA


########### Tree Diversity form Mauri et al 2017

Mauri_Trees_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Tree Species/Mauri etal 2017/Tree_SpR_NUTS2007.txt",
                         sep="\t", header=TRUE)

names(Mauri_Trees_2007)

Mauri_Trees_2007.sub <- Mauri_Trees_2007[, c(1, 9, 10, 66)]

summary(Mauri_Trees_2007.sub)

Mauri_Trees_2007.sub[Mauri_Trees_2007.sub$Mauri.Tree_SpR == 0, ]

# NUTS_ID EQL_Region                          EQL_Name 
#     BE10         12 BE Brussels hoofdstedelijk gewest              
#     BE25         13                BE West-Vlaanderen             
#     BE31         16                 BE Brabant Wallon             
#     EL43         51                          EL Kriti             
#     MT00        172                             MALTA

#### change Zero value to NA, because we do not have records in these NUTS regions

# Mauri_Trees_2007.sub$Mauri.Tree_SpR[Mauri_Trees_2007.sub$Mauri.Tree_SpR == 0] <- NA


###########################################

#### Elevation / Terrain Roughness

Elevation_2007 <- read.table("C:/Users/jmethorst/Documents/R analyses/Elevation/Elevation_NUTS2006.txt",
                         sep="\t", header=TRUE)

names(Elevation_2007)

Terrain_2007 <- Elevation_2007[, c(1, 9, 10, 11, 12, 13, 14, 15, 16)] 

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

natura.2000.sub <- natura.2000[, c(1, 9, 10, 12, 14)]

names(natura.2000.sub)
# [1] "NUTS_ID"             "EQL_Region"          "EQL_Name"           
# [4] "Natura_AreaSize_km2" "Natura_Perc_Cover"

summary(natura.2000.sub)

################################################################################

###################### Merge all the Data

names(Bird.Sp.Rich_2007)

Nature_data_2007 <- cbind(Nuts.EQL2007_shp@data[, c(1, 9)], Bird.Sp.Rich_2007[, c(4,5,6)], Megafauna_2007[,4:6],
                          "Tree.Sp.Rich" = Tree_Sp.Rich_2007[,4], "Mauri.Tree_SpR" = Mauri_Trees_2007.sub[,4], 
                          # Land.Hetero_2007[,4:9], 
                          area_2007, Terrain_2007[,4:8], # Climate_2007_sub[,3:16],  
                          natura.2000.sub[,4:5])

names(Nature_data_2007)

boxplot(Nature_data_2007[,1:35])

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

Dataset_total <- merge(eql_2007_sub.2, Nature_data_2007, by = "NUTS_ID")

str(Dataset_total) # 30430 obs. of  98 variables

summary(Dataset_total)

names(Dataset_total)

## Column with Question about Life Satisfaction is:
## Variable = q29	
## Variable label = Q29 All things considered, how satisfied would you say you are with your life these days?


library(lme4)
library(nlme)
library(arm)

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

# q40_7 = Rural or Countryside
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

# hhtypeEF = household type, factor, factor
summary(Dataset_total$hhtypeEF)

##############################################################################

######## plot data

names(Dataset_total)

summary(Dataset_total$Mauri.Tree_SpR)

par(mfrow = c(1,2))

boxplot(Natura_Perc_Cover ~ q29, data = Dataset_total, main = "Natura2000 %-Cover + LS")

boxplot(Natura_AreaSize_km2 ~ q29, data = Dataset_total, main = "Natura2000 km2 + LS")


boxplot(Mauri.Tree_SpR ~ q29, data = Dataset_total, main = "Mauri et al. Tree Sp.R. + LS")

boxplot(Tree.Sp.Rich ~ q29, data = Dataset_total, main = "EUFORGEN + FISE Tree Sp.R. + LS")



boxplot(Birdlife_SpR ~ q29, data = Dataset_total, main = "Birdlife Bird Sp.R. + LS")

boxplot(EBBA1_SpR ~ q29, data = Dataset_total, main = "EBBC Bird Sp.R. + LS")


boxplot(Megafauna_Spec.Rich ~ q29, data = Dataset_total, main = "Megafauna Sp.R. + LS")


boxplot(Elevation_range ~ q29, data = Dataset_total, main = "Elevation Range + LS")

boxplot(TRI.mean ~ q29, data = Dataset_total, main = "TRI Mean + LS")




###############################################################################
################           data analysis
################################################################################

?glm

dataset_mod1 <- Dataset_total[, c("country", "EQL_Region", "hh2a", "CVhh2b", "CV6768o", "CVq31", "hh2d", "q29", "q30", 
                                  "ISCED", "q67", "CVq67", "q40_6", "q43", "q22", "q52", "q40_7", "EurostatPopulationDensityAveragenumberofpeoplepersqua_A",
                                  "EurostatGDPpercapitainPPS2005", "EmplstatEF", "Rur_UrbEF", "hhtypeEF")]

str(dataset_mod1)

dataset_mod1$country <- as.factor(dataset_mod1$country)
dataset_mod1$EQL_Region <- as.factor(dataset_mod1$EQL_Region)
dataset_mod1$hh2a <- as.factor(dataset_mod1$hh2a)
dataset_mod1$hh2d <- as.factor(dataset_mod1$hh2d)
dataset_mod1$q30 <- as.factor(dataset_mod1$q30)
dataset_mod1$ISCED <- as.factor(dataset_mod1$ISCED)
dataset_mod1$q67 <- as.factor(dataset_mod1$q67)
dataset_mod1$q40_6 <- as.factor(dataset_mod1$q40_6)
dataset_mod1$q43 <- as.factor(dataset_mod1$q43)
dataset_mod1$q22 <- as.factor(dataset_mod1$q22)
dataset_mod1$q52 <- as.factor(dataset_mod1$q52)
dataset_mod1$q40_7 <- as.factor(dataset_mod1$q40_7)
dataset_mod1$EmplstatEF <- as.factor(dataset_mod1$EmplstatEF)
dataset_mod1$Rur_UrbEF <- as.factor(dataset_mod1$Rur_UrbEF)
dataset_mod1$hhtypeEF <- as.factor(dataset_mod1$hhtypeEF)

nature.data_mod1 <- Dataset_total[, 60:93]

str(nature.data_mod1)

nature.data_mod1$TRI.mean.cat <- as.factor(nature.data_mod1$TRI.mean.cat)
nature.data_mod1$Wolf_dummy <- as.factor(nature.data_mod1$Wolf_dummy)
nature.data_mod1$Bear_dummy <- as.factor(nature.data_mod1$Bear_dummy)

hist(nature.data_mod1$Birdlife_SpR)
hist(nature.data_mod1$EBBA1_SpR)

dat.mod1 <- cbind(dataset_mod1, nature.data_mod1)

str(dat.mod1)


################################################################################

####### explore data

par(mar = c(5,4,2,2))

##

par(mfrow = c(1,2))

plot(Birdlife_SpR ~ q29, data = dat.mod1)

plot(EBBA1_SpR ~ q29, data = dat.mod1)

####

par(mfrow = c(2,2))

plot(Corine2006_H ~ q29, data = dat.mod1)

plot(Corine2006_simp ~ q29, data = dat.mod1)

plot(Corine2006_nat.H ~ q29, data = dat.mod1)

plot(Corine2006_nat.simp ~ q29, data = dat.mod1)

####

plot(Tree_Sp.Rich_2007...4. ~ q29, data = dat.mod1, ylab = "TreeSpecies Richness")

plot(Megafauna_Spec.Rich ~ q29, data = dat.mod1)

plot(q29 ~ Bear_dummy, data = dat.mod1)

plot(q29 ~ Wolf_dummy, data = dat.mod1)

####


plot(log(CVq67) ~ q29, data = dat.mod1, xlab = "Life Satisfaction", ylab = "log Net Monthly Income")

plot(log(CV6768o) ~ q29, data = dat.mod1, xlab = "Life Satisfaction", ylab = "log Household Income [EUR]")

plot(log(EurostatGDPpercapitainPPS2005) ~ q29, data = dat.mod1, xlab = "Life Satisfaction", ylab = "log GDP per Capita")


plot(q29 ~ hh2a, data = dat.mod1, xlab = "Sex", ylab = "Life Satisfaction", xaxt = "n")

axis(1, 1:2, labels = c("Male", "Female"))


plot(CVhh2b ~ q29, data = dat.mod1, xlab = "Life Satisfaction", ylab = "Age")

par(mar = c(10,5,1,1))
plot(q29 ~ ISCED, data = dat.mod1, xlab = NULL, ylab = "Life Satisfaction", xaxt = "n")

axis(1, 1:7, labels = c("Early Childhood Education", "Primary Education", "Lower Secondary Education",
                        "Upper Secondary Education", "Post-Secondary non-tertiary",
                        "Short-cycle tertiary ", "Bachelor or Equivalent"), las = 2, cex.axis = 0.6)

# Value = 1	Label = ISCED0 = Early Childhood Education
# Value = 2	Label = ISCED1 = Primary Education
# Value = 3	Label = ISCED2 = Lower Secondary Education
# Value = 4	Label = ISCED3 = Upper Secondary Education
# Value = 5	Label = ISCED4 = Post-Secondary non-tertiary 
# Value = 6	Label = ISCED5 = Short-cycle tertiary 
# Value = 7	Label = ISCED6 = Bachelor or Equivalent

######## Generalized Linear Models

mod_glm1 <- glm(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                  q40_6 + Birdlife_SpR:a.km.2007 + country, data = dat.mod1)

summary(mod_glm1)

par(mfrow = c(2,2))

plot(mod_glm1)


mod_glm2 <- glm(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                  q40_6 + EBBA1_SpR:a.km.2007 + country, data = dat.mod1)

summary(mod_glm2)
# EBBA1_SpR:a.km.2007  5.486e-09  2.906e-09   1.888 0.059093 .

plot(mod_glm2)

mod_glm3 <- glm(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                  q40_6 + EBBA1_AreaWeighted_SpR:a.km.2007 + country, data = dat.mod1)

summary(mod_glm3)



###############################################################################

###### Mixed Effect Model

?na.exclude

mod_lme1 <- lme(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                  q40_6 + Birdlife_SpR*a.km.2007, 
                random = ~1 + Birdlife_SpR | country, data = dat.mod1, na.action = na.exclude)

summary(mod_lme1)
# AIC      BIC    logLik
# 65380.23 65680.69 -32651.12

mod_lme1.1 <- lme(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                  q40_6 + Birdlife_SpR*a.km.2007, 
                random = ~1 | country, data = dat.mod1, na.action = na.exclude)

summary(mod_lme1.1)
#  AIC      BIC    logLik
# 65383.58 65668.63 -32654.79

mod_lme1.2 <- lme(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                    q40_6 + Birdlife_SpR*a.km.2007, method = "ML",
                  random = ~1 | country, data = dat.mod1, na.action = na.exclude)

summary(mod_lme1.2)

plot(mod_lme1.2)

# AIC      BIC    logLik
# 65180.27 65465.41 -32553.14

mod_lme1.3 <- lme(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                    q40_6 + Birdlife_SpR*a.km.2007, method = "REML",
                  random = ~1 | country, data = dat.mod1, na.action = na.exclude)

summary(mod_lme1.3)

# AIC      BIC    logLik
# 65383.58 65668.63 -32654.79

####

mod_lme2 <- lme(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                  q40_6 + EBBA1_SpR*a.km.2007, method = "ML",
                random = ~1 | country, data = dat.mod1, na.action = na.exclude)

summary(mod_lme2)
# AIC      BIC    logLik
# 65169.74 65454.87 -32547.87

plot(mod_lme2)

mod_lme3 <- lme(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                  q40_6 + EBBA1_AreaWeighted_SpR*a.km.2007, 
                random = ~1 | country, data = dat.mod1, na.action = na.exclude)

summary(mod_lme3)


##############################################################################

################### Land heterogeneity

names(dat.mod1)

hist(log(dat.mod1$CV6768o))

hist(nature.data_mod1$Corine2006_nat.H)

####### linar model

mod_landhetero.1 <- gls(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                          q43 + q52 + q40_7 + Corine2006_nat.H, data = dat.mod1, na.action = na.exclude,
                        method = "ML")

summary(mod_landhetero.1)
#     AIC      BIC    logLik
# 63035.46 63350.59 -31476.73

plot(mod_landhetero.1)

###### with random effect

mod_landhetero.1.1 <- lme(log(q29) ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                          q43 + q52 + q40_7 + Corine2006_nat.H, data = dat.mod1, na.action = na.exclude,
                        random = ~1 | country, method = "ML")

summary(mod_landhetero.1.1)

# AIC      BIC    logLik
# 62419.38 62742.19 -31167.69

plot(mod_landhetero.1.1$residuals ~ mod_landhetero.1.1$fitted)




#########################################################################

######### Ordinal Data

######### ordered logistic regression or probit regression

library(MASS)

dat.mod1$q29 <- as.factor(dat.mod1$q29)

mod_olr <- polr(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                q40_6 + log(Birdlife_SpR):a.km.2007 + country, data = dat.mod1)

summary(mod_olr)

library(ordinal)

?clm

ordinal.mod.1 <- clm(q29 ~ log(CVq67) + hh2a + CVhh2b + CVq31 + hh2d + q30 + ISCED + 
                       q43 + q52 + q40_7 + Corine2006_H, data = dat.mod1, na.action = na.exclude)

summary(ordinal.mod.1)

confint(ordinal.mod.1)


plot(ordinal.mod.1$fitted.values)

plot(ordinal.mod.1$coefficients)

plot(ordinal.mod.1$)
