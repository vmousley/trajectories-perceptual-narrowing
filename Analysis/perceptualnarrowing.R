###############################################################################
################### analysis code for perceptual narrowing ####################
##################### last updated by victoria 11 june 22 #####################
###############################################################################

# loading packages -----
require("dplyr")
require('tidyr')
require("lme4")
require("reshape2")
require("ggplot2")
require("readxl")
require('rstatix')
require('papaja')
require('cowplot')
require('car')
loadfonts(device="pdf")
par(family = "LM Roman 10")

# loading data ----- 
dfCompare <- data.frame(read.csv("/Users/victoriamousley/Desktop/dfComparison.csv"))#; View(cleandata)
dfHabComparison <- data.frame(read.csv("/Users/victoriamousley/Desktop/pointOfHabComparison.csv"))
dfCompare <- merge(dfCompare, dfHabComparison, by = 'participant')
behavedata <- data.frame(read_excel('/Users/victoriamousley/Documents/MATLAB/BehaviouralData.xlsx'))

# reliability between methods -----
valid <- dfCompare %>% filter(dfCompare$issueNotes == '' 
                              & !grepl('TestBlock2.Same', excluded) 
                              & !grepl('TestBlock2.Switch', excluded) 
                              & dfCompare$switchNote != 'No data for switch'
                              & !grepl('<9 hab', excluded)
                              & !grepl('>33 hab', excluded))

valid$totalTestTobii <- valid$sameLookTobii + valid$switchLookTobii
valid$totalTestFBF <- valid$sameLookFBF + valid$switchLookFBF
valid$totalTestDiff <- valid$totalTestFBF - valid$totalTestTobii

meanDiff <- mean(valid$totalTestDiff, na.rm = TRUE)
sdDiff <- sd(valid$totalTestDiff, na.rm = TRUE)
twosds <- sdDiff * 2
highbound <- meanDiff+twosds

measuresCorr <- cor.test(valid$totalTestFBF, valid$totalTestTobii)

measureReliabilityPlot <- valid %>% ggplot(aes(x = totalTestTobii, y = totalTestFBF)) +
  geom_point() + 
  labs(y='FBF LT', x = 'ET LT', subtitle = 'Measurement Comparison') +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = 14, margin = margin(t = 10, b = 10)),
         plot.subtitle = element_text(hjust = 0.5, family = 'LM Roman 10', size = 12, margin = margin(t = 10, b = 10)),
         axis.title.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10)),
         axis.text.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10)),
         axis.title.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(r = 10)),
         axis.text.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(b = 10)),
         legend.position = 'none') +
  geom_smooth(method = 'lm', se = T); measureReliabilityPlot

distDiffTestPhasePlot <- valid %>% ggplot(aes(x = totalTestDiff)) +
  geom_density(fill = 'lightgrey', alpha = 5.0, bins = 30) + 
  labs(x = 'Difference Score (seconds)', y = 'Density', subtitle = 'Distribution of Difference Scores') +
  geom_vline(aes(xintercept = meanDiff, linetype = 'dotted')) + 
  geom_vline(aes(xintercept = highbound, linetype = 'longdash')) + 
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = 14, margin = margin(t = 10, b = 10)),
        plot.subtitle = element_text(hjust = 0.5, family = 'LM Roman 10', size = 12, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10)),
        axis.text.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(r = 10)),
        axis.text.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(b = 10)),
        legend.position = 'none'); distDiffTestPhasePlot

reliabilityPlots <- cowplot::plot_grid(measureReliabilityPlot, distDiffTestPhasePlot, labels = "AUTO",
                             label_fontfamily = 'LM Roman 10', label_fontface = "bold"); reliabilityPlots

ggsave('PerceptualNarrowingReliabilityPlots.jpeg', 
       path = "/Users/victoriamousley/trajectories-perceptual-narrowing/Analysis/", width = 10, height = 8, device='tiff', dpi=300)

finalSwitch <- vector()
finalSame <- vector()
finalID <- vector()
finalISI <- vector()
finalAttenGet1 <- vector()
finalAttenGet2 <- vector()
finalHabitTrial <- vector()
data <- vector()
reasonForSwitch <- vector()

for (row in 1:nrow(valid)) {
  
  id <- toString(valid[row, "participant"])  
  TobiiSwitch <- valid[row, "switchLookTobii"] 
  TobiiSame <- valid[row, "sameLookTobii"] 
  FBFSwitch <- valid[row, "switchLookFBF"] 
  FBFSame <- valid[row, "sameLookFBF"]
  totalTestDiff <- valid[row, "totalTestDiff"]
  TobiiISI <- valid[row, "ISILookTobii"]
  FBFISI <- valid[row, "ISILookFBF"]
  TobiiAtten1 <- valid[row, "firstAttenGetLookTobii"]
  FBFAtten1 <- valid[row, "firstAttenGetLookFBF"]
  TobiiAtten2 <- valid[row, "secondAttenGetLookTobii"]
  FBFAtten2 <- valid[row, "secondAttenGetLookFBF"]
  habitTrialTobii <- valid[row, "habitTrialTobii"]
  habitTrialFBF <- valid[row, "habitTrialFBF"]
  
  if(!is.na(totalTestDiff) & (TobiiSwitch < 1 | TobiiSame < 1 | totalTestDiff >= highbound)) {
    finalSwitch <- c(finalSwitch, FBFSwitch)
    finalSame <- c(finalSame, FBFSame)
    finalID <- c(finalID, id)
    finalISI <- c(finalISI, FBFISI)
    finalAttenGet1 <- c(finalAttenGet1, FBFAtten1)
    finalAttenGet2 <- c(finalAttenGet2, FBFAtten2)
    finalHabitTrial <- c(finalHabitTrial, habitTrialFBF)
    data <- c(data, 'FBF')
    
    reason <- vector()
    if(!is.na(totalTestDiff) & (TobiiSwitch < 1)) {
    reason <- c(reason, 'tobiiSwich <1')}
    if(!is.na(totalTestDiff) & (TobiiSame < 1)) {
      reason <- c(reason, 'tobiiSame <1')}
    if(!is.na(totalTestDiff) & (totalTestDiff >= highbound)) {
      reason <- c(reason, 'over higherbound')}
    
    reasonForSwitch <- c(reasonForSwitch, reason)
    
  } else { # 
    finalSwitch <- c(finalSwitch, TobiiSwitch)
    finalSame <- c(finalSame, TobiiSame)
    finalID <- c(finalID, id)
    finalISI <- c(finalISI, TobiiISI)
    finalAttenGet1 <- c(finalAttenGet1, TobiiAtten1)
    finalAttenGet2 <- c(finalAttenGet2, TobiiAtten2)
    finalHabitTrial <- c(finalHabitTrial, habitTrialTobii)
    data <- c(data, 'Tobii')
    reasonForSwitch <- c(reasonForSwitch, '')
    next 
  }
}

df <- data.frame(finalID, finalSame, finalSwitch, finalISI, finalAttenGet1, finalAttenGet2, finalHabitTrial, data, reasonForSwitch)
names(df)[names(df)=="finalID"] <- "id"

dfTobiiOnly <- dfCompare %>% filter(dfCompare$issueNotes != '' 
                     & !grepl('TestBlock2.Same', excluded) 
                     & !grepl('TestBlock2.Switch', excluded) 
                     & dfCompare$switchNote != 'No data for switch')
dfTobiiOnly <- select(dfTobiiOnly, 'participant', 'sameLookTobii', 'switchLookTobii', 'ISILookTobii',
                          'firstAttenGetLookTobii', 'secondAttenGetLookTobii', 'habitTrialTobii' )
dfTobiiOnly$reasonForSwitch = ''

names(dfTobiiOnly)[names(dfTobiiOnly)=="participant"] <- "id"
names(dfTobiiOnly)[names(dfTobiiOnly)=="sameLookTobii"] <- "finalSame"
names(dfTobiiOnly)[names(dfTobiiOnly)=="switchLookTobii"] <- "finalSwitch"
names(dfTobiiOnly)[names(dfTobiiOnly)=="ISILookTobii"] <- "finalISI"
names(dfTobiiOnly)[names(dfTobiiOnly)=="firstAttenGetLookTobii"] <- "finalAttenGet1"
names(dfTobiiOnly)[names(dfTobiiOnly)=="secondAttenGetLookTobii"] <- "finalAttenGet2"
names(dfTobiiOnly)[names(dfTobiiOnly)=="habitTrialTobii"] <- "finalHabitTrial"
dfTobiiOnly$data <- 'Tobii'

dfFinal <- rbind(df, dfTobiiOnly)

dfFinal <- dfFinal %>% filter(finalSame >= 1 
                              & finalSwitch >= 1
                              & finalHabitTrial > 6
                              & finalHabitTrial < 34) 

dfFinal$finalTest <- dfFinal$finalSame + dfFinal$finalSwitch

replacedDF <- dfFinal %>% filter(data == 'FBF')

names(behavedata)[names(behavedata)=="ID"] <- "id"
names(behavedata)[names(behavedata)=="Age"] <- "age"
names(behavedata)[names(behavedata)=="Group"] <- "group"

pndf <- merge(dfFinal, behavedata)
pndf <- pndf %>% filter(Degree.of.bi > .2 | is.na(Degree.of.bi))

FBFallPNPart <- pndf %>% filter(data == 'FBF')

# pre-reg hyp: mixed effects modelling -----

# per Csibra et al. (2016), log transform looking time before
# modeling or checking assumptions 

pndf$finalSameLog <- log(pndf$finalSame)
pndf$finalSwitchLog <- log(pndf$finalSwitch)

ltdf <- pndf %>% select(id, group, age, finalSameLog, finalSwitchLog) %>%
  pivot_longer(cols = c('finalSameLog','finalSwitchLog'), names_to = 'trial', values_to = 'LT')

pnPlot <- ltdf %>% ggplot(aes(x = age, y = LT, colour = group)) +
  geom_point() + 
  facet_wrap(~ trial) +
  labs(y='Looking time (log transformed)', x = 'Age (days)', title = '', subtitle = '', colour = 'Group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'LM Roman 10', size = (12)),
        strip.text.x = element_text(family = 'LM Roman 10', size = 12, face = 'bold.italic'),
        axis.title.x = element_text(family = 'LM Roman 10', size = 12),
        axis.text.x = element_text(family = 'LM Roman 10', size = 12),
        axis.title.y = element_text(family = 'LM Roman 10', size = 12),
        axis.text.y = element_text(family = 'LM Roman 10', size = 12),
        legend.title = element_text(family = 'LM Roman 10', size = 12),
        legend.text = element_text(family = 'LM Roman 10', size = 12),
        legend.position = 'right') + 
  scale_colour_manual(values = c('grey60', 'black'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T); pnPlot

ggsave('PerceptualNarrowingResult.jpeg', 
       path = "/Users/victoriamousley/trajectories-perceptual-narrowing/Analysis/", width = 13, height = 8, device='tiff', dpi=300)

## model
preRegM <- lmer(LT ~ age*group*trial + (1|id), data = ltdf); summary(preRegM)
step_result <- step(preRegM)

par(mfrow = c(2,2))
plot(preRegM, id.n = NULL)

int <- coef(preRegM)$id[,1]
sd(int, na.rm = TRUE)

aov <- anova(preRegM)

outlierTest(preRegM)

# Influential Observations
# added variable plots
avPlots(preRegM)

# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(valid)-length(preRegM$coefficients)-1))
plot(preRegM, which=4, cook.levels=cutoff)

influencePlot(preRegM,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Influence measures
inf.m <- influence.measures(preRegM)
par(mfrow=c(3,3))
for(i in 1:(dim(inf.m$infmat)[2]-1)){
  hist(inf.m$infmat[,i],
       main=dimnames(inf.m$infmat)[[2]][i])
}

# Identifying outliers by ID number
valid$participant[cooks.distance(preRegM) > 0.0615]

## Plot of model/residuals
# layout(matrix(c(1,2,3,4),2,2))
# plot(m0)

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

# standardized residuals versus fitted values by group
plot(preRegM, resid(., scaled=TRUE) ~ fitted(.) | group, abline = 0)

# box-plots of residuals by subject
plot(preRegM, id ~ resid(., scaled = TRUE))

# observed versus fitted values by subject
plot(preRegM, LT ~ fitted(.) | id, abline = c(0.1))

# residuals by age, separated by subject
plot(preRegM, resid(., scaled = TRUE) ~ age | id, abline=0)
