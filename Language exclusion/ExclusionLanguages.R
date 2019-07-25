# "Developmental trajectories of perceptual narrowing among monolingual and bilingual infants"
# Stage 1: Registered Report
# XXXXXXXXXXXXXXXXXX
# Submitted to Developmental Science

#############################
### Language exclusion ###
#############################

require(rlist)
# import all lists of languages that contain 10 retroflex consonants from Phoible

# "LATIN SMALL LETTER T WITH RETROFLEX HOOK" #
r1 <- read.csv("RETROFLEX1.csv"); r1 <- list(r1$name)

# "LATIN SMALL LETTER N WITH RETROFLEX HOOK" #
r2 <- read.csv("RETROFLEX2.csv"); r2 <- list(r2$name)

# "LATIN SMALL LETTER L WITH RETROFLEX HOOK" #
r3 <- read.csv("RETROFLEX3.csv"); r3 <- list(r3$name)

# "LATIN SMALL LETTER T WITH RETROFLEX HOOK - MODIFIER LETTER SMALL H" #
r4 <- read.csv("RETROFLEX4.csv"); r4 <- list(r4$name)

# "LATIN SMALL LETTER T WITH RETROFLEX HOOK - LATIN SMALL LETTER S WITH HOOK" #
r5 <- read.csv("RETROFLEX5.csv"); r5 <- list(r5$name)

# "LATIN SMALL LETTER Z WITH RETROFLEX HOOK" #
r6 <- read.csv("RETROFLEX6.csv"); r6 <- list(r6$name)

# "LATIN SMALL LETTER T WITH RETROFLEX HOOK - LATIN SMALL LETTER S WITH HOOK - MODIFIER LETTER SMALL H" #
r7 <- read.csv("RETROFLEX7.csv"); r7 <- list(r7$name)

list()# "LATIN SMALL LETTER D WITH TAIL - LATIN SMALL LETTER Z WITH RETROFLEX HOOK" #
r8 <- read.csv("RETROFLEX8.csv"); r8 <- list(r8$name)

# "LATIN SMALL LETTER T WITH RETROFLEX HOOK - COMBINING LEFT ANGLE BELOW" #
r9 <- read.csv("RETROFLEX9.csv"); r9 <- list(r9$name)

# LATIN SMALL LETTER T WITH RETROFLEX HOOK - COMBINING DOUBLE VERTICLE LINE BELOW" #
r10 <- read.csv("RETROFLEX10.csv"); r10 <- list(r10$name)

# make a list of all languages 
exclang <- list(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)

# how many languages included for each retroflex consonant?
View(exclang)
  # 481 [[1]]
  # 399 [[2]]
  # 359 [[3]]
  # 139 [[4]]
  # 106 [[5]]
  # 91 [[6]
  # 55 [[7]]
  # 46 [[8]]
  # 46 [[9]]
  # 44 [[10]]

# duplicates?
anyDuplicated(exclang)
  # 0 duplicates

# I'm sure there's a better way to flatten, but this works so life goes on
exclang <- unlist(exclang)

# alphabetise
exclang <- list.sort(exclang, na.last=NA)

# save as a searchable yaml file to use cntrl + f whilst recruiting babies
list.save(exclang, 'ExclusionLanguages.yaml')



