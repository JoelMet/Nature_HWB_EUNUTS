
library(maps)
library(rgdal)
library(gbm)
library(raster)
# library(rgeos)
# library(maptools)
# library(ncdf4)
library(ggplot2)

library(lme4)
library(nlme)
library(arm)

library(ordinal)
library(MASS)
library(VGAM)

library(effects)
library(GGally)
library(scales)

########## Pilot Analysis

######### Effects of Nature on Human Well-Being

###############################################################################

## load data set 

Dataset_total <- read.table("Dataset_total_2007.txt", header = TRUE, sep = "\t")


######## plot data

names(Dataset_total)

par(mfrow = c(1,1))


x_label <- quantile(Dataset_total$Natura_Perc_Cover, probs = seq(0.1, 1, 0.10))

cut(Dataset_total$Natura_Perc_Cover, pretty( Dataset_total$Natura_Perc_Cover, n = 8) , include.lowest = TRUE)

summary(Dataset_total$Natura_Perc_Cover)

# pretty( Dataset_total$Natura_Perc_Cover)

boxplot(q29 ~ cut(Natura_Perc_Cover, pretty( Dataset_total$Natura_Perc_Cover, n = 8) , include.lowest = TRUE), 
        data = Dataset_total, main = "Natura2000 %-Cover + LS", cex.axis=0.5,
        xlab = "Natura_Perc_Cover", ylab = "LS") # , xaxt = "n"

boxplot(q29 ~ cut(Natura_Perc_Cover, breaks = 10, include.lowest = TRUE), 
        data = Dataset_total, main = "Natura2000 %-Cover + LS", cex.axis=0.5,
        xlab = "Natura_Perc_Cover", ylab = "LS") # , xaxt = "n"

axis(1, at = c(1:10), labels = x_label, cex.axis=0.5, las=1)

plot(q29 ~ Natura_Perc_Cover, data = Dataset_total, main = "Natura2000 %-Cover + LS")

boxplot(Natura_AreaSize_km2 ~ q29, data = Dataset_total, main = "Natura2000 km2 + LS")


boxplot(Mauri.Tree_SpR ~ q29, data = Dataset_total, main = "Mauri et al. Tree Sp.R. + LS")

boxplot(Tree.Sp.Rich ~ q29, data = Dataset_total, main = "EUFORGEN + FISE Tree Sp.R. + LS")

boxplot(q29 ~ cut(Mauri.Tree_SpR, breaks = 10, include.lowest = TRUE), 
        data = Dataset_total, main = "Mauri et al. Tree Sp.R. + LS", cex.axis=0.5,
        xlab = "Mauri.Tree_SpR", ylab = "LS") # , xaxt = "n"


boxplot(Birdlife_SpR ~ q29, data = Dataset_total, main = "Birdlife Bird Sp.R. + LS")

summary(Dataset_total$Birdlife_SpR)

boxplot(q29 ~ cut(Birdlife_SpR, breaks = 10, include.lowest = TRUE), 
        data = Dataset_total, main = "Birdlife Sp.R. + LS", cex.axis=0.5,
        xlab = "Birdlife SpR", ylab = "LS") # , xaxt = "n"

boxplot(EBBA1_AreaWeighted_SpR ~ q29, data = Dataset_total, main = "EBBC Bird Sp.R. + LS")

boxplot(q29 ~ cut(EBBA1_AreaWeighted_SpR, breaks = 10, include.lowest = TRUE), 
        data = Dataset_total, main = "EBBC AreaWeighted Sp.R. + LS", cex.axis=0.5,
        xlab = "EBBC Area Weighted SpR", ylab = "LS") # , xaxt = "n"

boxplot(Megafauna_Spec.Rich ~ q29, data = Dataset_total, main = "Megafauna Sp.R. + LS")

boxplot(q29 ~ cut(Megafauna_Spec.Rich, breaks = 10, include.lowest = TRUE), 
        data = Dataset_total, main = "Megafauna Sp.R. + LS", cex.axis=0.5,
        xlab = "Megafauna SpR", ylab = "LS") # , xaxt = "n"


boxplot(q29 ~ cut(Mammal_Spec.Rich, breaks = 10, include.lowest = TRUE), 
        data = Dataset_total, main = "Mammal Sp.R. + LS", cex.axis=0.5,
        xlab = "Mammal SpR", ylab = "LS") # , xaxt = "n"


boxplot(Elevation_range ~ q29, data = Dataset_total, main = "Elevation Range + LS")

boxplot(TRI.mean ~ q29, data = Dataset_total, main = "TRI Mean + LS")

################################

# plot the area - species richness relationship with log scale

plot(log(Birdlife_SpR) ~ log(a.km.2007), data = Dataset_total, main = "Birdlife Sprich and NUTS Area Size 2007")


abline(lm(log(Birdlife_SpR) ~ log(a.km.2007), data = Dataset_total))


###############################################################################
###########           data analysis
################################################################################

names(Dataset_total)

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

hist(nature.data1$Birdlife_SpR)
hist(nature.data1$EBBA1_SpR)

###########################################################################

country_dummmies <- Dataset_total[, 3:33]

str(country_dummmies)

country_dummmies <- as.data.frame(lapply(country_dummmies, factor) )

#########################################################################


##### check for colinearity

?pairs


panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}


### first 10 columns

pairs(nature.data1[,1:9], upper.panel = panel.cor)

## -> very correlated with each other

## species richness with landscape hetero.

pairs(nature.data1[,c(1,2,4,7,8,9,10,12,13,14)], upper.panel = panel.cor)

cor(nature.data1[,c(1,2,4,7,8,9,10,12,13,14)])

ggpairs(nature.data1[,c(1,2,4,7,8,9,10,12,13,14)])

## -> landscape hetero. and species richness seem to have low correlation

################

ggpairs(nature.data1[,c(1,2,4,7,8,9,13,14,19,22,24,26,30,41,42,43)])



##########################################################################
############               Start analysis
##########################################################################

## merge the data subsets

dat.mod1 <- cbind(country_dummmies, socioeco_data1, nature.data1)

str(dat.mod1)

################################################################################

####### explore data

summary(dat.mod1$Household_net_income.Num) # 13792 NAs

summary(dat.mod1$Householdincome_Euro) # 9846 NAs


###############################################################################
######                   Mixed Effect Model
###############################################################################

names(dat.mod1)

GLMM_formula1 <- Life_Satisfaction ~ log(Householdincome_Euro) + EmplstatEF + Age + Maritial_status +
                                      Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
                                      PopulationDensityAveragenumberofpeoplepersqua_A + UnemployRate_reg

mod_lme1 <- lme(GLMM_formula1, random = ~1 | country, data = dat.mod1, na.action = na.exclude)

summary(mod_lme1)
# AIC      BIC    logLik
# 66320.1 66583.55 -33126.05

plot()

?lme

GLMM_formula2 <- Life_Satisfaction ~ log(Householdincome_Euro) + EmplstatEF + Age + Maritial_status +
                                      Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
                                      PopulationDensityAveragenumberofpeoplepersqua_A + UnemployRate_reg + 
                                      # nature
                                      Birdlife_SpR + EBBA1_AreaWeighted_SpR + Megafauna_Spec.Rich + Mauri.Tree_SpR +
                                      nat.Simp_div + Natura_AreaSize_km2 + TRI.mean + a.km.2007 + tmean.yearmean

mod_lme2 <- lme(GLMM_formula2, random = ~1 | country / EQL_Region , data = dat.mod1, na.action = na.exclude)

# NUTS Region als random effect

summary(mod_lme2)
# AIC       BIC     logLik
# 60908.09 61237.52 -30411.05

plot(mod_lme2)


##############################################################################
#########                       Ordinal Data
##############################################################################

dat.mod2 <- dat.mod1

##################################################################

###########  multinomial regression

?ordered

# dat.mod2$Life_Satisfaction <- ordered(dat.mod2$Life_Satisfaction, levels=c("1","2","3","4","5","6","7","8","9","10"))

# dat.mod2$Social_life <- ordered(dat.mod2$Social_life, levels=c("1","2","3","4","5","6","7","8","9","10"))

str(dat.mod2)

#########################

VGLM_formula2 <- Life_Satisfaction ~ log(Householdincome_Euro) + EmplstatEF + Age + Maritial_status +
                                      Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
                                      PopulationDensityAveragenumberofpeoplepersqua_A + UnemployRate_reg + 
                                      # nature
                                      Birdlife_SpR + EBBA1_AreaWeighted_SpR + Megafauna_Spec.Rich + Mauri.Tree_SpR +
                                      nat.Simp_div + Natura_AreaSize_km2 + TRI.mean + a.km.2007 + tmean.yearmean

?vglmm

vglm_mod1 <- vglm(VGLM_formula2, family = propodds, data = dat.mod2)

summary(vglm_mod1)

AICvlm(vglm_mod1) # AIC = 56761.85

# show coeficients
coefvlm(vglm_mod1)

confintvglm()

df.residual(vglm_mod1) # 141576

length(vglm_mod1@fitted.values)

length(vglm_mod1@residuals)

plotvglm(vglm_mod1)

# predictvglm

######### ordered logistic regression or probit regression

?polr

dat.mod3 <- dat.mod1

dat.mod3$Life_Satisfaction <- as.factor(dat.mod1$Life_Satisfaction)



polr_mod1 <- polr(Life_Satisfaction ~ log(Householdincome_Euro) + EmplstatEF + Age + Maritial_status +
                    Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
                    PopulationDensityAveragenumberofpeoplepersqua_A + UnemployRate_reg + 
                    # nature
                    Birdlife_SpR + EBBA1_AreaWeighted_SpR + Megafauna_Spec.Rich + Mauri.Tree_SpR +
                    nat.Simp_div + Natura_AreaSize_km2 + TRI.mean + a.km.2007 + tmean.yearmean, data = dat.mod3,
                    Hess = TRUE)

summary(polr_mod1)
## ERROR
# Error in svd(X) : unendliche oder fehlende Werte in 'x'



################################################################################
#############            ordinal model with random effects
###############################################################################

str(dat.mod2)

dat.mod2 <- dat.mod1

dat.mod2$Life_Satisfaction <- as.factor(dat.mod2$Life_Satisfaction)

dat.mod2$Social_life <- as.factor(dat.mod2$Social_life)

dat.mod2$logHouseholdincome_Euro <- log(dat.mod2$Householdincome_Euro)


###################

CLMM_formula1 <- Life_Satisfaction ~ log(Householdincome_Euro) + EmplstatEF + Age + Maritial_status +
        Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
        PopulationDensityAveragenumberofpeoplepersqua_A + UnemployRate_reg + 
        # nature
        Birdlife_SpR + EBBA1_AreaWeighted_SpR + Megafauna_Spec.Rich + Mauri.Tree_SpR +
        nat.Simp_div + Natura_AreaSize_km2 + TRI.mean + a.km.2007 + tmean.yearmean


unique(dat.mod2$country)
# 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 31 32 33 34 35 36 37 38 39 40 41 42

unique(dat.mod2$PopulationDensityAveragenumberofpeoplepersqua_A)
# n = 27

summary(dat.mod2$logHouseholdincome_Euro)

##  run model

ordinal.mod.1 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
                        (1|country / EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.1) # 65275.24

# confidence intervals
confint(ordinal.mod.1)

# extract random effects form model
ranef.clmm(ordinal.mod.1)

plot(ordinal.mod.1$fitted.values)

ordinal.mod.1$optRes

# coefficients
mod.1_co <- coef(ordinal.mod.1)

# variance-covariance matrix
mod.1_vc <- vcov(ordinal.mod.1)

###################

?effects

plot(Effect("logHouseholdincome_Euro", ordinal.mod.1))


####################

## link = probit

# ordinal.probit.mod.1 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
#                              Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
#                              (1|country / EQL_Region), data = dat.mod2, na.action = na.exclude, link = "probit")

# summary(ordinal.probit.mod.1) # 

# link   threshold nobs  logLik    AIC      niter        max.grad cond.H 
# probit flexible  18183 -32902.44 65900.88 10093(30279) 1.08e+03 1.4e+06

# Random effects:
#   Groups             Name        Variance Std.Dev.
# EQL_Region:country (Intercept) 0.02877  0.1696  
# country            (Intercept) 0.07653  0.2766  
# Number of groups:  EQL_Region:country 250,  country 27

####################

## Age quadratic
ordinal.mod.1.1 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
                        (1|country / EQL_Region), data = dat.mod2, na.action = na.exclude)


summary(ordinal.mod.1.1 ) # 65275.24

####################

## without social life as fixed factor

ordinal.mod.1.2 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED  + Rural_or_Countryside +
                          (1|country / EQL_Region), data = dat.mod2, na.action = na.exclude)


summary(ordinal.mod.1.2 ) # 69701.74

## -> AIC is much higher without Social Life!!


## with weights: WGT_Target = Weight variable to weight the result from target

ordinal.mod.1.3 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
                          (1|country / EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TARGET)


summary(ordinal.mod.1.3) # 65275.24

#############################################################################

#### select best model with package MuMin : dredge

library("MuMIn")

# prevent fitting sub-models to different datasets

dat.modSelection <- na.omit(socioeco_data1)

str(dat.modSelection) # 13919 obs. of  29 variables

dat.modSelection$Life_Satisfaction <- as.factor(dat.modSelection$Life_Satisfaction)

dat.modSelection$Social_life <- as.factor(dat.modSelection$Social_life)

dat.modSelection$logHouseholdincome_Euro <- log(dat.modSelection$Householdincome_Euro)

dat.modSelection$squareAge <- (dat.modSelection$Age)^2


######

options(na.action = "na.fail")

fm1 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + squareAge + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
                          (1|country / EQL_Region), data = dat.modSelection) #  na.action = na.exclude)

d.fm1 <- dredge(fm1, fixed = c("logHouseholdincome_Euro", "squareAge"))

saveRDS(d.fm1, "~/R analyses/Nature_HWB_EUNUTS/dredge.result_d.fm1.rds")

# fixed term is "intercept"

subset(d.fm1, delta < 4)

# Visualize the model selection table:
par(mar = c(3,5,6,4))

plot(d.fm1, labAsExpr = TRUE)

# Model average models with delta AICc < 4
model.avg(d.fm1, subset = delta < 4)

########

# Model selection table 
# (Int) EEF Hlt_1_6 lgH_Eur Mrt_stt Rlg Scl_lif       sqA df    logLik    AICc
# 95     +   +       +  0.5587       +   +       + 0.0001234 39 -25135.67 50349.6
# delta weight
# 95     0      1
# Models ranked by AICc(x) 
# Random terms (all models): 
#  ‘1 | country/EQL_Region’

##############################################################################

ordinal.mod.1.4 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                        Health_1_6 + Religion + Social_life + 
                        (1|country / EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.1.4)

# link  threshold nobs  logLik    AIC      niter       max.grad cond.H 
# logit flexible  18486 -33190.41 66458.81 7583(23810) 1.53e+02 1.3e+06

# Random effects:
#   Groups             Name        Variance Std.Dev.
# EQL_Region:country (Intercept) 0.08041  0.2836  
# country            (Intercept) 0.24017  0.4901  
# Number of groups:  EQL_Region:country 250,  country 27 

##############################################################################

############ Full Model with Nature data 

hist(dat.mod2$Coast_length.km)

summary(dat.mod2$Coast_length.km)

summary(log(dat.mod2$Dist_centroid.coast))

summary(dat.mod2$Natura_Perc_Cover)

summary(log(dat.mod2$prec.yearrange))

summary(dat.mod2$Megafauna_Spec.Rich)

hist(dat.mod2$nat.Simp_div)

hist(dat.mod2$TRI.mean)

summary(log(dat.mod2$Elevation_range))


ordinal.mod.full <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                           Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside +
                            Birdlife_SpR + Megafauna_Spec.Rich + Mauri.Tree_SpR + nat.Simp_div + nat.H_div + TRI.mean + Elevation_range + tmean.yearmean +
                            prec.yearrange + Natura_Perc_Cover + CDDA_All.2.IUCN_PercCover + Coast_length.km + Dist_centroid.coast +
                            log(a.km.2007) + 
                            (1 | country_abbr/EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TARGET)


summary(ordinal.mod.full) 

#                             CNTRBEL + CNTRDEN + CNTRDEU + CNTRELL + CNTRESP + CNTRFRA + CNTRIRL +  
# CNTRITAL + CNTRLUX + CNTRNL + CNTRAT + CNTRPOR + CNTRFIN + CNTRSWE +  
#  CNTRGB + CNTRCYP + CNTRCZ + CNTREE + CNTRHU + CNTRLV + CNTRLIT +  
#  CNTRMT + CNTRPOL + CNTRSK + CNTRSI + CNTRBUL + CNTRROM + CNTRCRO +  
#  CNTRTUR + CNTRNOR + CNTRMAC + 

##########################

## rescale data

dat.rescale <- dat.mod2

names(dat.rescale)

summary(dat.rescale)

# 81 = Elevation Range
# 84 = TRI Mean
# 90 = PREC Year Range
# 102 = Dist to Coast
# 103 = Coastline length

summary(dat.rescale[, 81])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6     275     664    1082    1947    4399

# 0 - 100
# dat.rescale$Elevation_range <- scale(dat.rescale[, 60 ], center = FALSE,  scale = max(dat.rescale[, 60 ], na.rm = TRUE)/100)

# summary(dat.rescale[, 60])
# Min.   :  0.1364  
# 1st Qu.:  6.2514  
# Median : 15.0943  
# Mean   : 24.5981  
# 3rd Qu.: 44.2601  
# Max.   :100.0000

### for more than one column 

dat.rescale[, c(81, 84, 91, 102, 103) ] <- lapply(dat.rescale[, c(81, 84, 91, 102, 103) ], function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/100))

summary(dat.rescale[, c(81, 84, 91, 102, 103) ])

names(dat.rescale)

######### try model again

hist(dat.rescale$prec.yearrange)

hist(dat.rescale$tmean.yearmean)

summary(dat.rescale$Megafauna_Spec.Rich)

names(country_dummmies)

ordinal.mod.rescale.full <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +                           
                                 Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside +
                                 log(Birdlife_SpR) + Megafauna_Spec.Rich + Mauri.Tree_SpR + nat.Simp_div + nat.H_div + 
                                 TRI.mean + Elevation_range + tmean.yearmean + prec.yearrange + 
                                 Natura_Perc_Cover + CDDA_All.2.IUCN_PercCover + Coast_length.km + Dist_centroid.coast +
                                 log(a.km.2007) +
                                 (1 | country_abbr / EQL_Region), data = dat.rescale, na.action = na.exclude, weights = dat.mod2$WGT_TARGET)

summary(ordinal.mod.rescale.full)

# Warning message:
#  (2) Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables? 
# In addition: Absolute and relative convergence criteria were met 

#  link  threshold nobs     logLik    AIC      niter        max.grad cond.H 
# logit flexible  16538.15 -30873.33 61852.65 14885(44771) 6.28e+01 6.2e+07

# Random effects:
#   Groups                  Name        Variance Std.Dev.
# EQL_Region:country_abbr (Intercept) 0.1258   0.3547  
# country_abbr            (Intercept) 0.1678   0.4096  
# Number of groups:  EQL_Region:country_abbr 243,  country_abbr 25 


##########################################################################

### normalize the data

# normalised (mean subtracted then divided by standard deviation)

dat.norm <- dat.mod2

names(dat.norm)

dat.norm[, c(61, 64, 67, 69, 73, 74, 79, 81, 
             84, 86, 91, 101, 102, 103, 105) ] <- lapply(dat.norm[, c(61, 64, 67, 69, 73, 74, 79, 81, 
                                                                      84, 86, 91, 101, 102, 103, 105) ], 
                                                         function(x) (x - mean(x, na.rm = TRUE))/ sd(x, na.rm = TRUE))

summary(dat.norm[, c(61, 64, 67, 69, 73, 74, 79, 81, 
                     84, 86, 91, 101, 102, 103, 105) ])

ordinal.mod.norm.full <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +                           
                                   Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside +
                                   Birdlife_SpR + Megafauna_Spec.Rich + Mauri.Tree_SpR + nat.Simp_div + nat.H_div + 
                                   TRI.mean + Elevation_range + tmean.yearmean + prec.yearrange + 
                                   Natura_Perc_Cover + CDDA_All.2.IUCN_PercCover + Coast_length.km + Dist_centroid.coast +
                                   a.km.2007 +
                                   (1 | country_abbr / EQL_Region), data = dat.norm, na.action = na.exclude, weights = dat.mod2$WGT_TARGET)

summary(ordinal.mod.norm.full)

# link  threshold nobs     logLik    AIC      niter        max.grad cond.H 
# logit flexible  16538.15 -30874.18 61854.37 11904(35892) 2.16e+01 1.7e+06

# Random effects:
#   Groups                  Name        Variance Std.Dev.
# EQL_Region:country_abbr (Intercept) 0.1259   0.3548  
# country_abbr            (Intercept) 0.1936   0.4400  
# Number of groups:  EQL_Region:country_abbr 243,  country_abbr 25 



###############################################################################

names(dat.mod2)

hist(log(dat.mod2$Birdlife_SpR))

hist(dat.mod2$Birdlife_SpR)

hist(log(dat.mod2$a.km.2007))

ordinal.mod.2 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                        log(Birdlife_SpR):log(a.km.2007) + log(a.km.2007) +
                        (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.2)

####

ordinal.mod.2.1 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                          log(Birdlife_SpR) + log(Birdlife_SpR):log(a.km.2007) + log(a.km.2007) +
                        (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

#Warning message:
# (3) Model is nearly unidentifiable: large eigenvalue ratio
# - Rescale variables? 
# In addition: Absolute and relative convergence criteria were met 

summary(ordinal.mod.2.1)

###

ordinal.mod.2.2 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                          log(Birdlife_SpR) + log(a.km.2007) +
                          (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.2.2)

####

anova(ordinal.mod.2, ordinal.mod.2.1, ordinal.mod.2.2)

######################

## calculate with quadratic term for Age 

ordinal.mod.2.3 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                          log(Birdlife_SpR) + log(a.km.2007) +
                          (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)


summary(ordinal.mod.2.3)

## compare model

anova(ordinal.mod.2.2, ordinal.mod.2.3)

## about the same!!

######################

ordinal.mod.2.4 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                          log(Birdlife_SpR) + log(a.km.2007) +
                          (1|EQL_Region), data = dat.mod2, na.action = na.exclude)


summary(ordinal.mod.2.4) # AIC 69820.49


plot(residuals(ordinal.mod.2.4, type = "normalized") ~ fitted(ordinal.mod.2.4))


########

## with weights: WGT_Target

ordinal.mod.2.5 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                          log(Birdlife_SpR) + log(a.km.2007) +
                          (1 | country/EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TARGET)


summary(ordinal.mod.2.5) # 66671.52


###

## weights : WGT_TOTAL = Weight variable for total sample (31 countries)

dat.mod2$WGT_TOTAL

ordinal.mod.2.6 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                          log(Birdlife_SpR) + log(a.km.2007) +
                          (1 | country/EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TOTAL)


summary(ordinal.mod.2.6) # 64295.04

###

## random slope with Rural or Countryside

ordinal.mod.2.7 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                          log(Birdlife_SpR) + log(a.km.2007) +
                          (1 + Rural_or_Countryside| country/EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TOTAL)


summary(ordinal.mod.2.7) # 63979.25

plot(Effect("Birdlife_SpR", ordinal.mod.2.7))

#########

ordinal.mod.2.8 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Collapsed_ISCED + Rural_or_Countryside + 
                          log(Birdlife_SpR) + log(a.km.2007) +
                          (1 | country/EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TOTAL)


summary(ordinal.mod.2.8) # 64320.04

ordinal.mod.2.9 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Collapsed_ISCED + Rural_or_Countryside + 
                          log(Birdlife_SpR) + log(a.km.2007) +
                          (1 + Collapsed_ISCED| country/EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TOTAL)


summary(ordinal.mod.2.9) # 64221.14

##########

hist(sqrt(dat.mod2$Birdlife_SpR))

hist(log(dat.mod2$Birdlife_SpR))

hist(dat.mod2$Birdlife_SpR^2)

plot(Birdlife_SpR^2 ~ log(a.km.2007), data = dat.mod2)

ordinal.mod.2.10 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                          Health_1_6 + Religion + Collapsed_ISCED + Rural_or_Countryside + 
                          Birdlife_SpR^2 + log(a.km.2007) +
                          (1 | country/EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TOTAL)


summary(ordinal.mod.2.10) # 64322.72


?effects

plot(Effect("Birdlife_SpR", ordinal.mod.2.10))

#####

ordinal.mod.2.11 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                           Health_1_6 + Religion + Collapsed_ISCED + Rural_or_Countryside + 
                           log(Birdlife_SpR) + log(a.km.2007) + country_abbr +
                           (1 | EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TOTAL)


summary(ordinal.mod.2.11) # 64292.44


########

ordinal.mod.2.12 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                           Health_1_6 + Religion + Collapsed_ISCED + Rural_or_Countryside + 
                           log(Birdlife_SpR) + log(a.km.2007) + Country_group1 + 
                           (1 | country_abbr/EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TOTAL)


summary(ordinal.mod.2.12) # 64316.95

########

ordinal.mod.2.13 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                           Health_1_6 + Religion + Collapsed_ISCED + Rural_or_Countryside + 
                           log(Birdlife_SpR) + log(a.km.2007) + 
                           (1 | Country_group1/EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TOTAL)


summary(ordinal.mod.2.13) # 64377.73


######


ordinal.mod.2.14 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age^2 + Maritial_status +
                           Health_1_6 + Religion + Collapsed_ISCED + Rural_or_Countryside + 
                           log(Birdlife_SpR) + log(a.km.2007) + Country_group1 +
                           (1 | EQL_Region), data = dat.mod2, na.action = na.exclude, weights = dat.mod2$WGT_TOTAL)


summary(ordinal.mod.2.14) # 64370.34

##############################

ordinal.mod.3 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                        log(EBBA1_AreaWeighted_SpR):log(a.km.2007) + log(a.km.2007) +
                        (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.3)

## 

ordinal.mod.3.1 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                        log(EBBA1_AreaWeighted_SpR) + log(EBBA1_AreaWeighted_SpR):log(a.km.2007) + log(a.km.2007) +
                        (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.3.1)

### anova compare models

anova(ordinal.mod.3, ordinal.mod.3.1)


##############################################################################

names(dat.mod2)

hist(dat.mod2$nat.H_div)

hist(dat.mod2$nat.Simp_div)

# hist(log(dat.mod2$nat.Simp_div))

## land hetero 

ordinal.mod.4 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                          Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                          nat.Simp_div + nat.H_div + log(a.km.2007) +
                          (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.4)

###

ordinal.mod.4.1 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                        nat.Simp_div + log(a.km.2007) +
                        (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.4.1)

##

ordinal.mod.4.2 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                        nat.H_div + log(a.km.2007) +
                        (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.4.2)

##

anova(ordinal.mod.4, ordinal.mod.4.1, ordinal.mod.4.2)


##############################################################################

### Tree Species

names(dat.mod2)

hist(dat.mod2$Mauri.Tree_SpR)

hist(log(dat.mod2$Mauri.Tree_SpR))

# hist(log(dat.mod2$nat.Simp_div))

## land hetero 

ordinal.mod.5 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                        Mauri.Tree_SpR + log(a.km.2007) +
                        (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.5)

####

ordinal.mod.5.1 <- clmm(Life_Satisfaction ~ logHouseholdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Rural_or_Countryside + 
                        log(Mauri.Tree_SpR) + log(a.km.2007) +
                        (1|country/EQL_Region), data = dat.mod2, na.action = na.exclude)

summary(ordinal.mod.5.1)






###############################################################################

################################################################################

library(MCMCglmm)

?MCMCglmm

MCMC.mod.1 <- MCMCglmm(Life_Satisfaction ~ Householdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside,
                        random = ~ country + EQL_Region, data = dat.mod2, family = "ordinal")

#############

library("gmnl")
library("mlogit")

?mlogit.data

data("Electricity", package = "mlogit")

names(Electricity)

Electr <- mlogit.data(Electricity, id.var = "id", choice = "choice",
                      varying = 3:26, shape = "wide", sep = "")

str(Electr )

test.df <- mlogit.data(dat.mod2, shape = "wide")

###############################################################################

################################################################################

install.packages("classInt")
install.packages("XML")

library(XML)
library(classInt)

colmat <- function(nquantiles=10, upperleft = rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label")
{
  my.data<-seq(0,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)
  }
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
  }
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
}

col.matrix <- colmat(nquantiles=10, upperleft="blue", upperright="yellow", bottomleft="green", bottomright="red", xlab="My x label", ylab="My y label")

col.matrix <- colmat(nquantiles=10)


