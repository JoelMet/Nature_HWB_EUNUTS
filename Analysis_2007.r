
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




###############################################################################
###########           data analysis
################################################################################

names(Dataset_total)

socioeco_data1 <- Dataset_total[, c("country", "EQL_Region", "hh2a", "CVhh2b", "CV6768o", "CVq31", "hh2d", "q29", "q30", 
                                  "ISCED", "q67", "CVq67", "q40_6", "q43", "q22", "q52", "q40_7", "EurostatPopulationDensityAveragenumberofpeoplepersqua_A",
                                  "EurostatGDPpercapitainPPS2005", "EmplstatEF", "Rur_UrbEF", "hhtypeEF",
                                  "UnemployRate_reg")]

### change names

names(socioeco_data1) <- c("country", "EQL_Region", "Gender", "Age", "Householdincome_Euro", "Children", "Employment_status", "Life_Satisfaction", 
                           "Maritial_status", "Education_level_ISCED", "Household_net_income.F", "Household_net_income.Num", "Health_1_10", "Health_1_6",
                           "Religion", "Rural_or_Countryside", "Social_life", "PopulationDensityAveragenumberofpeoplepersqua_A",
                            "GDPpercapitainPPS2005", "EmplstatEF", "Rur_UrbEF", "Household_type",
                            "UnemployRate_reg")

str(socioeco_data1)

# life Satisfaction and Social Life are considered numeric!!!

socioeco_data1$country <- as.factor(socioeco_data1$country)
socioeco_data1$EQL_Region <- as.factor(socioeco_data1$EQL_Region)
socioeco_data1$Gender <- as.factor(socioeco_data1$Gender)
socioeco_data1$Employment_status <- as.factor(socioeco_data1$Employment_status)

socioeco_data1$Maritial_status <- as.factor(socioeco_data1$Maritial_status)
socioeco_data1$Education_level_ISCED <- as.factor(socioeco_data1$Education_level_ISCED)

socioeco_data1$Household_net_income.F <- as.factor(socioeco_data1$Household_net_income.F)
socioeco_data1$Health_1_10 <- as.factor(socioeco_data1$Health_1_10)
socioeco_data1$Health_1_6 <- as.factor(socioeco_data1$Health_1_6)

socioeco_data1$Religion <- as.factor(socioeco_data1$Religion)
socioeco_data1$Rural_or_Countryside <- as.factor(socioeco_data1$Rural_or_Countryside)

socioeco_data1$EmplstatEF <- as.factor(socioeco_data1$EmplstatEF)
socioeco_data1$Rur_UrbEF <- as.factor(socioeco_data1$Rur_UrbEF)
socioeco_data1$Household_type <- as.factor(socioeco_data1$Household_type)


###########################################################################

# nature.data_mod1 <- Dataset_total[, 60:93]

nature.data1 <- Dataset_total[, 63:105]

str(nature.data1)

nature.data1$TRI.mean.cat <- as.factor(nature.data1$TRI.mean.cat)
nature.data1$Wolf_dummy <- as.factor(nature.data1$Wolf_dummy)
nature.data1$Bear_dummy <- as.factor(nature.data1$Bear_dummy)

hist(nature.data1$Birdlife_SpR)
hist(nature.data1$EBBA1_SpR)

##########################################################################

## merge the data subsets

dat.mod1 <- cbind(socioeco_data1, nature.data1)

str(dat.mod1)

################################################################################

####### explore data



######################################

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

names(dat.mod1)

###### Mixed Effect Model

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


#########################################################################

######### Ordinal Data

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



######################################################################

# multiple regression analysis

library(car)

avPlots(lm1)

#other options
multinom(prog2 ~ ses + write, data = ml)




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

######################################################################

str(dat.mod2)

dat.mod2$Life_Satisfaction <- as.factor(dat.mod2$Life_Satisfaction)

dat.mod2$Social_life <- as.factor(dat.mod2$Social_life)

CLMM_formula1 <- Life_Satisfaction ~ log(Householdincome_Euro) + EmplstatEF + Age + Maritial_status +
        Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
        PopulationDensityAveragenumberofpeoplepersqua_A + UnemployRate_reg + 
        # nature
        Birdlife_SpR + EBBA1_AreaWeighted_SpR + Megafauna_Spec.Rich + Mauri.Tree_SpR +
        nat.Simp_div + Natura_AreaSize_km2 + TRI.mean + a.km.2007 + tmean.yearmean


?clmm

unique(dat.mod2$country)
# 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 31 32 33 34 35 36 37 38 39 40 41 42

ordinal.mod.1 <- clmm(Life_Satisfaction ~ Householdincome_Euro + EmplstatEF + Age + Maritial_status +
                        Health_1_6 + Religion + Education_level_ISCED + Social_life + Rural_or_Countryside +
                        PopulationDensityAveragenumberofpeoplepersqua_A + 
                        (1|country / EQL_Region), data = dat.mod2)

# Warning message:
# (2) Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables? 
# In addition: Absolute and relative convergence criteria were met 


summary(ordinal.mod.1)

confint(ordinal.mod.1)


plot(ordinal.mod.1$fitted.values)

plot(ordinal.mod.1$coefficients)


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


