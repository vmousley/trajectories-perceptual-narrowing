# "Developmental trajectories of perceptual narrowing among monolingual and bilingual infants"
# Stage 1: Registered Report
# XXXXXXXXXXXXXXXXXXXXX
# Submitted to Developmental Science

# Dependencies ------------------------------------------------------------
setwd("~/trajectories-perceptual-narrowing")
require("praise")
require("plyr")
require("dplyr")

# Importing Data ----------------------------------------------------------
# Eye-tracking ANALYSIS data, cleaned by codeDataClean.py file (from a raw file containing all participants).
    # NOTE: exclusion occurs in codeDataClean.py
cleandata <- data.frame(read.csv("cleandata.csv")); View(cleandata); praise()

# Then import behavioural data frames, which include all hand-scored 
# data required for analyses (e.g., questionnaires, Mullens, etc)
manualdata <- data.frame(read.csv("test.csv")); View(manualdata); praise ()

# merge them to create a clean & complete analysis data set
mydata <- merge(cleandata, manualdata, by.x = 1, by.y = 0, all.x = TRUE); View(mydata)

mydata <- data.frame(read.csv("XXXXX.csv"))

# Pre-Processing ----------------------------------------------------------

# Numeric to Categorical (Group)
mydata$Group <- as.factor(mydata$Group)
mydata$Group <- revalue(mydata$Group, 
                        c("0"="monolingual", "1"="bilingual"))

# Define outcome: total looking time to test phase
mydata$TotalTest <- cbind(mydata$Same + mydata$Switch)

#### Graphical Analysis ####

# Age x Difference Score
plotAgexDiff <- plot(mydata$Age, mydata$TotalTest, 
                     xlab="Age (days)", 
                     ylab="Total LT to Test", 
                     cex=1, pch="X", 
                     cex.axis=1, 
                     cex.lab=1, col='blue')

# Pairs: Difference Score x Age
pairs(mydata)
pairs(~TotalTest+Age, data=mydata)

# Plot: Difference Score and Group
plot(mydata$TotalTest, mydata$Group)

# Language Group x Difference Score (****** ISSUE WITH MEANS ******)
groupscatter <- plot(as.numeric(mydata$Group), mydata$DiffScore,
                     ylab="Difference Score (Same - Switch)", xaxt="n",
                     xlab="Language Group",
                     cex=1.5, pch="*", 
                     cex.axis=1, 
                     cex.lab=1, col='purple')
axis(1,1:2, labels=levels(mydata$Group), cex.axis=1)
groupscatter <- points(c(1,2), 
                       c(mean(mydata$DiffScore[mydata$Group=='monolingual']), 
                         mean(mydata$DiffScore[mydata$Group=='bilingual'])), pch="---", cex=4)


# Difference score distribution
hist <- hist(mydata$DiffScore, 
             xlab = "Difference Score (Same - Switch)", 
             ylab = "Frequency")

#### Basic Functions ####

# Descriptive Statistics
descriptives <- summary(mydata)
dim(mydata)
names(mydata)

# Corr between Age and Difference Score
cor(mydata$TotalTest, mydata$Age, method="pearson")

# Associations between Difference Score ~ Group
tapply(mydata$TotalTest, mydata$Group, summary)
t.test(TotalTest ~ Group, data=mydata)
summary(aov(TotalTest ~ Group, data=mydata))
wilcox.test(TotalTest ~ Group, data=mydata)
kruskal.test(TotalTest ~ Group, data=mydata)

#### Linear Modelling ####

## Note to self: ASSUMPTIONS! 
# All observations independent
# y is linearly associated with predictors
# errors are normally distributed around 0 with constant variance across all predicted y values

m0 <- lm(TotalTest ~ 1, data=mydata)
m1 <- lm(TotalTest ~ Age*Group*Switch, data=mydata)
summary(m1)

names(summary(m1))

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

plot(as.numeric(mydata$Group), mydata$TotalTest, 
     ylab = "Total Test LT",
     xlab = "Language Group", 
     cex=1, pch="*", xaxt="n", col="blue", cex.axis=1, 
     cex.lab=1)
axis(1, 1:2, labels=c("Monolingual", "Bilingual"), cex.axis=1)
points(mydata$Group, fit.m, pch="---", cex=2)

residuals(m1)
rstandard(m1)
rstudent(m1)

# Explore residuals
hist(rstandard(m1))
hist(dffits(m1))
hist(dfbetas(m1)[,"Age"], nclass=10)
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
require(car)
vif(m1)
