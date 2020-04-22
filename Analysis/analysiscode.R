# "Developmental trajectories of perceptual narrowing among monolingual and bilingual infants"
# Stage 1: Registered Report, rejected by Developmental Science
# last updated 22 Apr 2020

# Dependencies ------------------------------------------------------------
require("praise")
require("dplyr")
require("lme4")
require("multcomp")
require("reshape2")
require("ggplot2")
require("car")
require("readxl")
require("formattable")
require("lmerTest")
require("e1071")
require("lattice")
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

# Import pre-processed eye-tracking data (inclusion criteria met) - WILL NEED TO CHANGE PATH 
# to wherever data is stored on your computer
cleandata <- data.frame(read.csv("/Users/victoriamousley/Documents/PYTHON data/allCleanData.csv"))#; View(cleandata)

# View(cleandata): shows you what the data frame 'cleandata' looks like, so that should help
# you understand what I'm doing below w/ renaming etc.

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
names(cleandata)[names(cleandata)=="Difference.score.switch.same"] <- "Diff"
names(cleandata)[names(cleandata)=="Overall.Experiment.LT"] <- "TotalLT"

# Import behavioural data, which include all hand-scored 
  # data required for analyses (e.g., questionnaires, Mullen, etc)
behavedata <- data.frame(read_excel('/Users/victoriamousley/Documents/MATLAB/BehaviouralData.xlsx'))#; View(behavedata)
names(behavedata)[names(behavedata)=="ID"] <- "id"
names(behavedata)[names(behavedata)=="Age"] <- "age"
names(behavedata)[names(behavedata)=="Group"] <- "group"
alldata <- merge(cleandata, behavedata, by.x = 'id', by.y = 'id') # merge eye-tracking
# and behavioural data by ID (so all data aligns correctly)
class(alldata$age) # this command tells you what type of data/object R thinks it's looking at
# sometimes it thinks things are factors or characters when it shouldn't, so you can 
# re-write them to be numeric for analyses

alldata$age <- as.numeric(alldata$age) # this does the above so age can be continuous, numeric var

# take out babies with valid PN data but invalid PN backgrounds
exclude <- list('B007', 'B041','B047','B048') # these are ones I've identified as having 
# either not enough bilingual exposure to be 'bilingual', or babies who had Hindi/other exposure
alldata <- alldata[(!alldata$id %in% exclude),] # saying: write 'alldata' variable that
# is made of the 'alldata' frame, EXCEPT ids that are listed in 'exclude' list (defined above)
# allows manual exclusion of rows/participants, basically

# You now have all the data you need!
praise() # a fun package that makes you feel good about yourself! hooray

# Matching ----------------------------------------------------------------
## for those BOOKED
booked <- data.frame(read_excel('/Users/victoriamousley/Documents/MATLAB/IDs.xlsx'))
# this is the excel doc i was updating with each recruiting/booking of the babies
# so this code is just for me to make sure I know where our group numbers are
names(booked)[names(booked)=="Age"] <- "age"
names(booked)[names(booked)=="Group"] <- "group"
names(booked)[names(booked)=='PN.valid.'] <- 'PN'
names(booked)[names(booked)=='ID.no'] <- 'id'
booked$agem <- floor(booked$age*0.0328767); booked$agem # says: 'find age var in the booked 
# dataframe, which is calculated in days, and multiply it by 0.03 to turn it into months, 
# then find the 'floor' integer (aka which month bin the baby should be in) and put that in 
# a new variable in booked table called 'agem' (age in months)
booked_table <- table(booked$agem, booked$group); booked_table # make a table of
# show me a pivot table of groups by age bins

# mplot <- mosaicplot(mtable,
           # main = 'Data Collected Matching',
           # xlab = 'Age bins (months)', ylab = 'Group',
           # cex.axis = 1.5, border = TRUE, color = TRUE)
# this just makes a nice plot but means nothing really

## subtract those with bad data or excluded! (see IDs sheet)
booked <- booked[!(booked$PN == '0'),] # go to booked table, if PN val=0 (which means they
# don't have valid PN data for some reason) then exclude them in the final booked table
booked_table <- table(booked$agem, booked$group) # show pivot table of babies who are 
# both valid for inclusion AND have good data - this ASSUMES booked in future babies have 
# good PN data so we don't overbook way too much
booked_table # all numbers BOOKED -- excluding infants with excluded data

## for those with GOOD data
alldata$agem <- floor(alldata$age*0.0328767); alldata$agem # same as above, but using alldata
# frame which is the actual data collected during ET task
good_table <- table(alldata$agem, alldata$group); good_table  # same as above

### Behavioural analysis --------------------------------------------------
mono <- filter(alldata, group=='M') # filter all rows in 'alldata' where group = M
# and assing in a new frame called 'mono'
names(mono)[names(mono)=="CDI.Eng"] <- "cdieng" # rename 
mono$cdieng <- as.numeric(mono$cdieng, na.rm = T) # reclassify as number
mean(mono$cdieng, na.rm = TRUE) # mean < median
median(mono$cdieng, na.rm = TRUE) # median > mean
skewness(mono$cdieng, na.rm = TRUE) # super skewed
hist(mono$cdieng) # looks skewed
boxplot(mono$cdieng) # has 2 outliers
outliers <- boxplot(mono$cdieng)$out # find them
mono[which(mono$cdieng %in% outliers),] # see rest of info for outlier babies
# mono <- mono[-which(mono$cdieng %in% outliers),] this code will exclude outliers


# show me CDI by age in monolinguals -- some example code for making a scatterplot where
# you can see ID numbers. there's warning messages but you can ignore them
ggplot(mono, aes(y=cdieng, x=age, colour=agem)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = T, size = 0.5) +
  geom_text(aes(label=id)) + 
  ggtitle('CDI by Age: Monolinguals') +
  xlab('Age (days)') + 
  ylab('Raw CDI sum')

m1 <- lm(cdieng ~ age, data = mono) # basic code for a linear model -- is CDI 
# score predicted by age in the monolingual group?
summary(m1) # summary of M1

bi <- filter(alldata, group=='B')
names(bi)[names(bi)=="Eng.exp"] <- "pereng"
names(bi)[names(bi)=="Second.lang.exp"] <- "perother"
names(bi)[names(bi)=="lang.mixing.scale"] <- "langmix"
bi$perother <- as.numeric(bi$perother) # reclassify percentage of non-Eng exposure
bi$degbi <- (bi$pereng/bi$perother) # calculate degree of bilingualism 

# Pre-Reg Hyp: Visual Analysis ------------------------------------------------------
# Age x Total Test LT
plotAgexTotalTest <- ggplot(alldata, aes(age, TotalTest, colour = group)) + 
  geom_point() +
  labs(x ='Age (days)', y = 'Total Test LT (seconds)') +
  labs(title = 'Age x Total Test LT') +
  labs(tag ='A') # scatterplot for age x total test LT, coloured dots = by group

plotAgexTotalTest # show scatterplot made above

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

# OUTLIERS ------
mean(alldata$TotalTest, na.rm = TRUE)
median(alldata$TotalTest)
sd(alldata$TotalTest)
skewness(alldata$TotalTest, na.rm = TRUE) # super skewed
hist(alldata$TotalTest) # looks skewed
boxplot(alldata$TotalTest)

# start making new data frame w/ only the columns you need
df <- alldata[ , c(1, 2, 13, 14, 15, 28)] # says: "go to 'alldata' frame, pull out ALL 
# rows, but only columns 1, 2, 13, 14, 15, and 28 (which we care about)
# can tell we care about them with command: colnames(alldata)
# this gives list of column numbers with their labels, so we are pulling out 
# id, sex, age, gender, group, and age in months for now

# give each participant 2 rows
df <- df %>% slice(rep(1:n(), each = 2)) # this is probably a bad way to do this, 
# but it's using dplyr to slice every row, then make 2 rows for every 1, in a new data frame 
# called DF

# make new column for trial type
df$trial <- NA # make a new column for trial type

# reshape looking time data to long format
  # could probably melt eloquently, but idk how so i'm forcing it
same <- alldata$LTSame # save same looking times
switch <- alldata$LTSwitch # save switch looking times
# total <- alldata$TotalTest # control for total attention
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
df$group <- alldata$group # move data from alldata$group to df$group

# Data is ready for modelling
praise()

# Log transformation ------------------------------------------------------

# Per Csibra et al. (2014), log transform looking time before
# Modeling or checking assumptions 

ggplot(data = df, aes(LT)) + geom_histogram() # looks non-normal
skewness(df$LT) # data is approximately symmetric? rule is -0.5 to 0.5 
# is approximately symmatric, 1 > or -1 < is highly skewed

ggplot(data = df, aes(x = age, y = LT))+
  geom_point()

ggplot(data = df, aes(x = age, y = LT))+
  geom_point() + 
  scale_y_log10()

ggplot(data = df, aes(x = age, y = LT)) + 
  geom_point() + 
  scale_x_log10()

ggplot(data = df, aes(x=age, y=LT)) + 
  geom_point() + 
  scale_x_log10() + scale_y_log10()

lm.model = lm(LT ~ age*group*trial, data = df)
summary(lm.model) # linear model isn't going to work for this because
# then we can't have fixed effect, but 
# without log transformation, teh residual standard error is 1.817

lm_log.model=lm(log(LT) ~ age*group*trial, data = df)
summary(lm_log.model) # and WITH log transformed LTs, residual standard 
# error is 0.4161

lm_log10.model=lm(log10(LT) ~ age*group*trial, data = df)
summary(lm_log10.model) # with log 10 transformed LTs, residual
# standard error is 0.1807

# IF WE WANT TO LOG10 IT
df$LT <- log10(df$LT)

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

mean(df$LT)
median(df$LT)
sd(df$LT)
skewness(df$LT) # super skewed
hist(df$LT) # looks skewed
boxplot(df$LT) # has one outlier
outliers <- boxplot(df$LT)$out # B022 is outlier
df[which(df$LT %in% outliers),] # see rest of info
# df <- df[-which(df$LT %in% outliers),] take out outliers

m0 <- lmer(LT ~ 1 + (1|id), data=df); summary(m0)
# ggplot(df, aes(y=LT, x=age, colour=group)) + 
#   geom_point() + 
#   geom_smooth(method = 'lm', se = T, size = 0.5) + 
#   geom_text(aes(label=id))

m1 <- lmer(LT ~ age*group*trial + (1|id), data=df); summary(m1)
  # Outcome = looking time
  # Predictors: age (days), group (M v B), trial (same v switch)
  # The fixed effect tells the model to fit individual trajectories for each participant

m2 <- lmer(LT~agem*group*trial + (1|id), data = df); summary(m2)

# Plotting regression line
df$group <- as.factor(df$group) # make group a factor
df <- na.omit(df) # omit the NAs in df 
df$group <- as.factor(df$group)

plotAgexTotalTest <- ggplot(alldata, aes(x = age, y = TotalTest, colour=group)) + 
  geom_smooth(method = 'lm', se = T, size = 0.5) +
  geom_point(alpha = 1) +
  # geom_ribbon(aes(ymin = cilower, ymax=ciupper), fill='blue') +
  # geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), fill='blue') +
  geom_text(aes(label=id)) +
  theme_bw() +
  ggtitle('LT to test phase over time') +
  xlab('Age (days)') + 
  ylab ('LT to Test Phase (Same & Switch) (sec)'); plotAgexTotalTest # make a plot

plotAgexSwitch <- ggplot(alldata, aes(x = age, y = LTSwitch, colour = group)) +
  geom_smooth(method = 'lm', se = T, size = 0.5) +
  geom_point(alpha = 1) + 
  geom_text(aes(label=id)) +
  theme_bw() + 
  ggtitle('LT to switch phase') + 
  xlab ('Age (days)') + 
  ylab ('LT to Switch Phase (sec)'); plotAgexSwitch

# standardized residuals versus fitted values by group
plot(m0, resid(., scaled=TRUE) ~ fitted(.) | group, abline = 0)

# box-plots of residuals by subject
plot(m0, id ~ resid(., scaled = TRUE))

# observed versus fitted values by subject
plot(m0, LT ~ fitted(.) | id, abline = c(0.1))

# residuals by age, separated by subject
plot(m0, resid(., scaled = TRUE) ~ age | id, abline=0)

## for m1

# standardized residuals versus fitted values by group
plot(m1, resid(., scaled=TRUE) ~ fitted(.) | group, abline = 0)

# box-plots of residuals by subject
plot(m1, id ~ resid(., scaled = TRUE))

# observed versus fitted values by subject
plot(m1, LT ~ fitted(.) | id, abline = c(0.1))

# residuals by age, separated by subject
plot(m1, resid(., scaled = TRUE) ~ age | id, abline=0)

qqmath(m1)
# if (require("ggplot2")){
#   ## we can create the same plots using ggplot2 and fortify() function
#   m1F <- fortify.merMod(m1)
#   ggplot(m1, aes(.fitted, .resid)) + geom_point(colour="blue") +
#     facet_grid(.~group) + geom_hline(yintercept=0)
#   ## note: ids are ordered by mean distance
#   ggplot(m1F, aes(id,.resid)) + geom_boxplot() + coord_flip()
#   ggplot(m1F, aes(.fitted,LT)) + geom_point(colour = "blue") +
#     facet_wrap(~id) + geom_abline(intercept=0, slope=1)
#   ggplot(m1F, aes(age,.resid)) + geom_point(colour="blue") + facet_grid(.~group) + 
#     geom_hline(yintercept=0)+geom_line(aes(group=id), alpha=0.4)+geom_smooth(method="loess")
#   detach("packages::ggplot2")
# }
# Exploration of model (NTS: NO IDEA WHAT THIS DOES----------------------------------------------------
# Effects of attention
mean()

# Isolate coefficients and CIs 
coefficients(m1)
ci <- confint(m1)

ggplot()# Goodness of Fit
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

# Difference? (NTS: DOESN'T WORK YET) -------------------------------------------------------------
names(alldata)[names(alldata)=="Difference.score.switch.same"] <- "TestDiff" # why Diff?
plotAgexTestDiff <- ggplot(alldata, aes(x = age, y = Diff, colour = group)) +
  geom_smooth(method="lm", se = T, size = 0.5) +
  geom_point(alpha = 1) + 
  geom_text(aes(label=id)) +
  theme_bw() + 
  ggtitle('Difference in LT between test phases') + 
  xlab ('Age (days)') + 
  ylab ('Difference in LT between test phases'); plotAgexTestDiff

# need to make TestDiff a thing to make this model work

m4 <- lmer(TestDiff ~ age*group*trial + (1|id), data = df)

# Raincloud (NTS: NO IDEA WHAT THIS DOES)---------------------------------------------------------------
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
