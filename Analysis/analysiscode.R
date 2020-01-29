# "Developmental trajectories of perceptual narrowing among monolingual and bilingual infants"
# Stage 1: Registered Report
# XXXXXXXXXXXXXXXXXX
# Submitted to Developmental Science

# Dependencies ------------------------------------------------------------
require("praise")
require("dplyr")
require("lme4")
require("reshape2")
require("ggplot2")
require("car")
require("readxl")
require("formattable")
require("lmerTest")
# require('cowplot')
# require('readr')

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
cleandata <- data.frame(read.csv("/Volumes/leap/PYTHON data/analysisData.csv"))#; View(cleandata)

# rename variables
names(cleandata)[names(cleandata)=="Participant.ID"] <- "id"
names(cleandata)[names(cleandata)=="Sex..1...male..2...female."] <- "sex"
names(cleandata)[names(cleandata) == "Sound.Condition"] <- "condition"
names(cleandata)[names(cleandata)=="LT.Pre.test"] <- "LTPre"
names(cleandata)[names(cleandata)=="LT.Same.Trials"] <- "LTSame"
names(cleandata)[names(cleandata)=="LT.Switch.Trials"] <- "LTSwitch"
names(cleandata)[names(cleandata)=="LT.Post.test"] <- "LTPost"
names(cleandata)[names(cleandata)=="LT.both.same.and.switch"] <- "TotalTest"
names(cleandata)[names(cleandata)=="LT.total.of.pre.and.post.test"] <- "TotalAttenGet"

# Import behavioural data, which include all hand-scored 
  # data required for analyses (e.g., questionnaires, Mullen, etc)
behavedata <- data.frame(read_excel('/Volumes/leap/Behavioural/BehaviouralData.xlsx'))#; View(behavedata)
names(behavedata)[names(behavedata)=="Age"] <- "age"
names(behavedata)[names(behavedata)=="Group"] <- "group"
names(behavedata)[names(behavedata)=="Difference.score.switch.same"] <- "diffscore"
names(behavedata)[names(behavedata)=="Overall.Experiment.LT"] <- "LTexperiment"
names(behavedata)[names(behavedata)=="degree.of.bi"] <- "degreeofbilingualism"
names(behavedata)[names(behavedata)=="CDI.MONO"] <- "cdimono"
names(behavedata)[names(behavedata)=="CDI.BI.ENG"] <- "cdibieng"
names(behavedata)[names(behavedata)=="CDI.BI.OTHER"] <- "cdibiother"
alldata <- merge(cleandata, behavedata, by.x = 'id', by.y = 'ID')
alldata$age <- as.numeric(alldata$age)
# You now have all the data you need!
praise()

# Matching ----------------------------------------------------------------
## for those with data collected
alldata$agem <- floor(alldata$age*0.0328767); alldata$agem
mtable <- table(alldata$agem, alldata$group); mtable 
# mplot <- mosaicplot(mtable,
           # main = 'Data Collected Matching',
           # xlab = 'Age bins (months)', ylab = 'Group',
           # cex.axis = 1.5, border = TRUE, color = TRUE)

## for those booked


# Counterbalancing --------------------------------------------------------
# cb <- data.frame(alldata$condition, alldata$condition, alldata$agem)
# cbtable <- table(cb); cbtable

# Graphical Analysis ------------------------------------------------------

# Age x Total Test LT
plotAgexTotalTest <- ggplot(alldata, aes(age, TotalTest, colour = group)) + 
  geom_point() +
  labs(x ='Age (days)', y = 'Total Test LT (seconds)') +
  labs(title = 'Age x Total Test LT') +
  labs(tag ='A')
plotAgexTotalTest

# Age x LT to Switch
plotAgexSwitch <- ggplot(alldata, aes(age, LTSwitch, colour = group)) + 
  geom_point() +
  labs(x ='Age (days)', y = 'LT Switch (seconds)') +
  labs(title = 'Age x LT Switch') +
  labs(tag ='A')
plotAgexSwitch

# Global Attention
plotAttention <- ggplot(alldata, aes(age, TotalAttenGet, colour = group)) + 
  geom_point() + 
  labs(x = 'Age (days)', y = 'Total LT to Attention Getter Phases') +
  labs(title = 'Total LT to Atten Gett Phases') + 
  labs(tag = 'A')
plotAttention

# Modeling ----------------------------------------------------------------

# start making new data frame w/ only the columns you need
df <- alldata[ , c(1, 2, 13, 14)]

# give each participant 2 rows
df <- df %>% slice(rep(1:n(), each = 2))

# make new column for trial type
df$trial <- NA

# reshape looking time data to long format
  # could probably melt eloquently, but idk how so i'm forcing it
same <- alldata$LTSame # save same looking times
switch <- alldata$LTSwitch # save switch looking times
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
df$group <- alldata$group

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

m0 <- lmer(LT ~ 1 + (1|id), data=df); summary(m0)
m1 <- lmer(LT ~ age*group*trial + (1|id), data=df); summary(m1)

  # Outcome = looking time
  # Predictors: age (days), group (M v B), trial (same v switch)
  # The fixed effect tells the model to fit individual trajectories for each participant

# Plotting regression line
df$group <- as.factor(df$group)
df <- na.omit(df)
df$group <- as.factor(df$group)

plotAgexTotalTest <- ggplot(alldata, aes(x = age, y = TotalTest, colour=group)) + 
  geom_smooth(method = "lm", se = F, size = 0.5) +
  geom_point(alpha = 1) + 
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), fill='blue') +
  geom_text(aes(label=id)) +
  theme_bw() +
  ggtitle('LT to test phase over time') +
  xlab('Age (days)') + 
  ylab ('LT to Test Phase (Same & Switch) (sec)'); plotAgexTotalTest

plotAgexSwitch <- ggplot(alldata, aes(x = age, y = LTSwitch, colour = group)) +
  geom_smooth(method="lm", se = F, size = 0.5) +
  geom_point(alpha = 1) + 
  geom_text(aes(label=id)) +
  theme_bw() + 
  ggtitle('LT to switch phase') + 
  xlab ('Age (days)') + 
  ylab ('LT to Switch Phase (sec)'); plotAgexSwitch

# Exploration of model ----------------------------------------------------
# Effects of attention
mean()

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
df$ID[cooks.distance(m1) > 0.2]

# Plot of model/residuals
layout(matrix(c(1,2,3,4),2,2))
plot(m1)

# Collinearity measures 
vif(m1)

# Controlling for global attention ----------------------------------------
df2 <- alldata[ , c(1, 2, 10, 13, 14)] # total attenge
df2 <- df2 %>% slice(rep(1:n(), each=2))
df2$TrialLT <- df$LT
df2$TrialType <- df$trial
names(df2)[names(df2)=="TotalAttenGet"] <- "atn"
m3 <- lmer(TrialLT ~ atn + age*group*TrialType + (1|id), data = df2)
summary(m3)

# Difference? -------------------------------------------------------------
names(alldata)[names(alldata)=="Difference.score.switch.same"] <- "TestDiff"
plotAgexTestDiff <- ggplot(alldata, aes(x = age, y = TestDiff, colour = group)) +
  geom_smooth(method="lm", se = F, size = 0.5) +
  geom_point(alpha = 1) + 
  geom_text(aes(label=id)) +
  theme_bw() + 
  ggtitle('Difference in LT between test phases') + 
  xlab ('Age (days)') + 
  ylab ('Difference in LT between test phases'); plotAgexTestDiff

df3 <- df2
#### giving up on this

m4 <- lmer(TestDiff ~ age*group*trial + (1|id), data = df3)

# Raincloud ---------------------------------------------------------------
library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)

source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')

raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

## TOTAL TEST PLOT
totaltest <- melt(alldata, id.vars=c('id', 'group', 'age'),
                 measure.vars=c('TotalTest'), variable.name='LT to Total Test',
                 value.name='TotalTest')

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld <- ddply(totaltest, ~TotalTest, summarise, mean=mean(TotalTest), median=median(TotalTest), lower=lb(TotalTest),
               upper=ub(TotalTest))
head(sumld)

totaltestplot <- ggplot(data = totaltest, aes(y = TotalTest, x = group, fill = group)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TotalTest, color = "darkred"), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme
totaltestplot

## SWITCH PLOT
switchplot <- melt(alldata, id.vars=c('id', 'group', 'age'),
                  measure.vars=c('LTSwitch'), variable.name='LT to Switch Phase',
                  value.name='LTSwitch')

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld2 <- ddply(switchplot, ~LTSwitch, summarise, mean=mean(LTSwitch), median=median(LTSwitch), lower=lb(LTSwitch),
               upper=ub(LTSwitch))
head(sumld2)

switchplot <- ggplot(data = switchplot, aes(y = LTSwitch, x = group, fill = group)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = LTSwitch, color = "darkred"), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme
switchplot
