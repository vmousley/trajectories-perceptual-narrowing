# "Developmental trajectories of perceptual narrowing among monolingual and bilingual infants"
# Stage 1: Registered Report
# XXXXXXXXXXXXXXXXXX
# Submitted to Developmental Science

# Dependencies ------------------------------------------------------------
setwd("~/Downloads/trajectories-perceptual-narrowing-master/Analysis") # Need to re-set if folder downloaded from GitHub
require("praise")
require("dplyr")
require("lme4")
require("reshape2")
require ("ggplot2")
require("car")

# Pre-Processing ----------------------------------------------------------

# NOTE: Important pre-processing steps occur in the python script 'codeCleanData.py'
  # located in 'Analysis Scripts (Syntax)' folder of the OSF profile.
  # This script converts raw data into two .csv files: 
    # 1: 'allCleanData.csv' which contains all eye-tracking data for all participants
    # 2: 'analysisData.csv' which contains all eye-tracking data *for included participants only*

  # Specific inclusion criteria met in this pre-processing script are that infants have BOTH: 
    # 1: Habituated within 9 - 33 habituation trials 
      # 'habTrials > 33 or habTrials < 9'
    # 2: Looked for at least 1 second in test trials
      # 'LTSame < 1 or LTSwitch < 1'

  # Therefore, this script constitutes the analysis pipeline for all pre-processed eye-tracking data.

# Importing Data ----------------------------------------------------------

# Import pre-processed eye-tracking data (inclusion criteria mets)
cleandata <- data.frame(read.csv("analysisData.csv"))#; View(cleandata)
mydata <- cleandata

# Import behavioural data, which include all hand-scored 
  # data required for analyses (e.g., questionnaires, Mullen, etc)
behavedata <- data.frame(read.csv("behaviouralData.csv"))#; View(behavedata)

# Clean behavedata to match number of participants in excluded data set
A <- cleandata$Participant.ID
B <- behavedata$ID
identical(A, B)
# False, means there are excluded participants
  # can see in this example that clean data doesn't include
  # p's 1, 5, or 10
  # NTS: eventually make this automatic

# Alter behavedata accordingly 
behavedata <- behavedata[-c(14:29), ] # this is just erasing unnecessary columns
B <- behavedata$ID
identical(A, B)
# Now it's true (hopefully), means they contain same included p's

# You now have all the data you need!
praise()

# Clean for modeling ------------------------------------------------------
ogdata <- mydata # save original data set in case you need it later (FOR PLOTS)
mydata$group <- behavedata$Group
mydata$age <- behavedata$age

# rename variables
names(mydata)[names(mydata)=="Participant.ID"] <- "id"
names(mydata)[names(mydata)=="Sex..1...male..2...female."] <- "sex"
names(mydata)[names(mydata)=="LT.Pre.test"] <- "LTPre"
names(mydata)[names(mydata)=="LT.Same.Trials"] <- "LTSame"
names(mydata)[names(mydata)=="LT.Switch.Trials"] <- "LTSwitch"
names(mydata)[names(mydata)=="LT.Post.test"] <- "LTPost"
names(mydata)[names(mydata)=="LT.both.same.and.switch"] <- "TotalTest"

### UPGRADE ###

# Graphical Analysis ------------------------------------------------------

# NTS: Not finished, need to add illustration of group and trial effects

# Age x Total Test LT
plotAgexTotalTest <- ggplot(mydata, aes(age, TotalTest)) + geom_point() +
  labs(x ='Age (days)', y = 'Total Test LT (seconds)') +
  labs(title = 'Age x Total Test LT') +
  labs(tag ='A')
plotAgexTotalTest

# start making new data frame w/ only the columns you need
df <- mydata[ , c(1, 2)]

# add relevant behavioural variables to mydata, add new column for trial
df$age <- mydata$age # NTS: this is weird bc i did some stuff manually
df$group <- mydata$group

# give each participant 2 rows
df <- df %>% slice(rep(1:n(), each = 2))

# make new column for trial type
df$trial <- NA

# reshape looking time data to long format
  # could probably melt eloquently, but idk how so i'm forcing it
same <- mydata$LTSame # save same looking times
switch <- mydata$LTSwitch # save switch looking times
ltsb <- rbind(same, switch) # bind them together w ps as columns (b for 'before')
ltsa <- melt(ltsb) # put in correct long order (a for 'after')

# check visually to be sure this was done correctly 
ltsb; ltsa
  # make sure that columns from ltsb ([,1], [,2], [,3], [,4], etc) 
  # are alternating over 'same' and 'switch' in ltsa correctly
  # SUPER important: if incorrect, all analysis data will be wrong

# input data in correct format back into df
df$LT <- ltsa$value
df$trial <-ltsa$Var1
df <- na.omit(df) # omit empty rows

# Data is ready for modelling
praise()

# Log transformation ------------------------------------------------------

# Per Csibra et al. (2014), log transform looking time before
# Modeling or checking assumptions 

# NTS: ADD!

# Linear Modelling --------------------------------------------------------

### ASSUMPTIONS OF LINEAR MIXED EFFECTS MODELS ###
  # 1: Explanatory variables are related linearly to the outcoome.
  # 2: The errors have constant variance.
  # 3: The errors are independent.

# NOTES TO SELF ABOUT TESTING MODEL ASSUMPTIONS
# Plotting the residuals against the explanatory variable will indicate if the wrong model has been fitted (i.e. higher order terms are needed) or if there is some dependence on some other explanatory variable. 
# If this is the case some obvious patterning will be visible in the plot. 
# Plotting the residuals in order, any trend visible may indicate seasonal pattern or autocorrelation.
# Plotting the residuals against the fitted values will indicate if there is non-constant error variance, i.e. if the variance increases with the mean the residuals will fan out as the fitted value increases. Usually transforming the data, or using another distribution will help. 
# A Normal probability plot, histogram of the residuals or say a Wilk-Shapiro test will indicate if the normality

m0 <- lmer(LT ~ 1 + (1|id), data=df)
m1 <- lmer(LT ~ age*group*trial + (1|id), data=df)
  # Outcome = looking time
  # Predictors: age (days), group (M v B), trial (same v switch)
  # The fixed effect tells the model to fit individual trajectories for each participant

# Plotting regression line
df$group <- as.factor(df$group)
mydata <- na.omit(mydata)
mydata$group <- as.factor(mydata$group)
ggplot(mydata, aes(x = age, y = TotalTest, colour=group)) + 
  geom_smooth(method="lm",se=F,size=1) +
  geom_point(alpha = 1) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  ggtitle('LT to test phase over time') +
  xlab('Age (days)') + 
  ylab ('LT to Test Phase (Same & Switch) (sec)')
  

# Exploration of model ----------------------------------------------------

# Isolate coefficients and CIs 
coefficients(m1)
confint(m1)

# Goodness of Fit
summary(m1)$adj.r.squared
deviance(m1)
BIC(m1)

# Explore fitted values, predictions, and residuals
fit.m <- fitted(m1); summary(fit.m)
predict.m <- predict(m1); summary(predict.m)

residuals(m1)
rstudent(m1)

# Explore residuals

# tells you how much the outcome will change if you take out that predictor (?double check)
hist(cooks.distance(m1),nclass=10)

# Influence measures
inf.m <- influence.measures(m1)
par(mfrow=c(3,3))
for(i in 1:(dim(inf.m$infmat)[2]-1)){
  hist(inf.m$infmat[,i],
       main=dimnames(inf.m$infmat)[[2]][i])
}

# Identifying outliers by ID number
mydata$ID[cooks.distance(m1) > 0.2]

# Plot of model/residuals
layout(matrix(c(1,2,3,4),2,2))
plot(m1)

# Collinearity measures 
vif(m1)

