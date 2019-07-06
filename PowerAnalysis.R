# "Developmental trajectories of perceptual narrowing among monolingual and bilingual infants"
# Stage 1: Registered Report
# XXXXXXXXXXXXXXXXXX
# Submitted to Developmental Science

# Dependencies ------------------------------------------------------------
# devtools::install_github("Lakens/ANOVApower", force = TRUE)
require("ANOVApower")
# https://github.com/Lakens/ANOVApower

# Background --------------------------------------------------------------

# Calculating the power required to detect effect 
# reported in Casaus et al. (unpublished thesis)
# at 15 months
# Group x Trial interaction, F(1, 42) = 6.27, 
# p = 0.02. Driven by increased sensitivity
# to the non-native contrast in bilingual compared
# to monolingual group.

# Per Lakens (2013), calculated partial eta-squared
# value using the formula: 
# partial eta^2 = (F * dfeffect) / (F * dfeffect) + (dferror))

# plugging in values from effect reported above      
# partial eta^2 = (6.27 * 1) / ((6.27*1) + (42))
# partial eta^2 = 0.13

# Then convert from partial eta square to effect size F (Cohen, 1988)
# cohen's f = sqrt(eta^2/(1-eta^2)) (Cohen, 1988)
# replacing values above
# sqrt(0.13/(1-0.13)) = effect size f of 0.3865557

# Definition of partial eta squared: 
# the variance attributable to an effect divided by the variance that 
# could have been attributable to the effect.

# Setting up --------------------------------------------------------------

# design = 2 between x 2 within = 2b*2w
# n = sample size for between subject condition (per group)
# mu = vector with means for each condition
# sd = population standard deviation
# r = correlation for within designs or 0 for between
# labelnames = factor/level names
# plot = logical 

#### Specific parameters for paper
# 2 between (mono vs bilingual) x 2 within (same vs swtich)
# a = group (mono vs bilingul); b = trial (same vs switch)

design_result <- ANOVA_design("2w*2b",
                              n = 37,
                              mu = c(7, 7, 7, 7.3865557), 
                              c("group", "monolingual", "bilingual", "trial", "same", "switch"),
                              sd = 0.5,
                              r = 0.5)

# Simulation-based power calculations -------------------------------------

### Repeatedly simulate data for each condition based on means,
# sample size, standard deviation, and correlation
# using ANOVA_power. This performs power analyses based on 
# repeatedly simulating normally distributed data.

ANOVA_power(design_result, alpha = 0.05, nsims = 1000)
# Power for Trial*Group int = 90.50 with an n = 37

### Simulate a dataset that has exactly the desired properties
# every cell of the design has n datapoints that have the 
# desired mean and standard deviation, and correlation between groups 
# (for a within design). By performing an ANOVA on this dataset, 
# we can calculate the required statistics from the ANOVA result 
# used to calculate the statistical power. 

ANOVA_exact(design_result)
# Power for Trial*Group int = 90.67 with an n = 37

### Visual representation of power and sample size relationship
plot_power(design_result, min_n = 8, max_n = 50)
