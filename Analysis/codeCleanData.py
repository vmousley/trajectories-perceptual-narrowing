# "Developmental trajectories of perceptual narrowing among monolingual and bilingual infants"
# Stage 1: Registered Report
# XXXXXXXXXXXXXXXXXXXXX
# Submitted to Developmental Science


# DESCRIPTION:
# This code processes all output from PyHab into two csv files. The two csv files are identical, but one has processed
# data from ALL participants and one has processed data from all NON-EXCLUDED participants
# The files contain participant ID, sex, how many trials the baby took to habituate, and different total
# looking times across experiment phases. Looking time is shortened to LT throughout.

# HOW DOES IT WORK:
# When extracting data from the PyHab output, this code uses indexes. For example, PyHAB output has sex on column 4,
# the code will therefore assign the value on the second row in column 4 to variable 'sex':
# sex = dataExtracted[1][3] (indexing starts at 0)

# BEFORE RUNNING THIS CODE:
# Make sure the codeCleanData.py file and the data-folder is in the same directory. This code is set-up to run on the
# sample data in folder 'SampleData'. With a new data-set, names/paths need to be adjusted on line 26.

import os
from os import listdir
import csv

path = ('SampleData')

# OBTAINING LIST OF NON-VERBOSE CSV FILES IN FOLDER
listFiles = listdir(path) #'listFiles' is now a list of filenames within the 'DATA' folder
allCleanData = []

removeThese = []
for file in listFiles:
    # Of the output files from PyHab we are only interested in non-verbose and non-stats files.
    # Next line of code filters this.
    if not (file[-8:-6]) == '20':
        removeThese.append(file)

for file in removeThese:
    listFiles.remove(file)

## EXTRACTING DATA FROM EACH FILE IN DATAFOLDER
for file in listFiles:  # For every file (corresponding to one participant) in data-folder, the following code runs:
    with open(path+'/'+file) as rawDataFile:
        reader = csv.reader(rawDataFile)
        rawData = []  # This list will contain all information in participant file.
        rowCount=0 # This is to keep track of how many rows the csv file is. Later on the code uses this number when
        # indexing to avoid indexing a row that does not exist.
        for row in reader:
            rawData.append(row) # I add each row in the file to the rawData list.
            rowCount += 1

    # DEMOGRAPHICS (partID, group, age, sex, condition):
    partID = (rawData[1][0])  # collects participant number (which is on second row of first column in csv file)
    sex = rawData[1][3]  # collects sex on row 2 column 4. NOTE: 1 is male, 2 is female
    condition = int(rawData[1][5][-2:])  # This obtains the sound condition, by getting the integer value of the
    # last two letters of the label e.g. "LEAP07" => 7

    # HABITUATION TRIAL NUMBER, AND LOOKING TIME DATA
    # Number of habituation trials is collected through counting the amount of times "Hab" is mentioned in
    # TrialType column.
    # LT is collected through summing the LT on each trial for each testing phase (i.e. pre-test, same, switch,
    # post-test). In the csv file, this data is under "SumOnA" (column 12)

    habTrials = 0  # Counts habituation trials
    LTAttenGet = [0,1]  # Counts LT on attention getters (pre and post)
    LTSame = 0  # Counts LT on same trials
    LTSwitch = 0  # Counts LT on switch trials
    overallLT = 0  # Counts overall experiment LT

    for row in range(1,rowCount):  #The program will analyse the csv file row by row (except header), looking for keywords
        # "Hab", "AttenGet", "Same" or "Switch" to extract relevant info. Overall time also collected

        if rawData[row][8] == "Hab":
            habTrials += 1 # This counts number of habituation trials

        if rawData[row][8] == "AttenGet":
            LTAttenGet.append(float(rawData[row][11])) # LT on attention getters, both pre and post

        if rawData[row][8] == "Same":
            LTSame += float(rawData[row][11])  # LT on same condition


        if rawData[row][8] == "Switch":
            LTSwitch +=float(rawData[row][11])  # LT on switch condition

        # Collecting overall looking time:
        overallLT += float(rawData[row][11])  # This collects overall LT

    PreTestLT = LTAttenGet[0]  # LT at pre-test (first attention getter)
    PostTestLT = LTAttenGet[1]  # LT at post-test (second attention getter)
    totalPrePostLT = sum(LTAttenGet)  # Total LT both pre-test and post-test
    totalSameSwitchLT = LTSame + LTSwitch  # Total LT both same and switch condition
    diffScoreSwitchSame = LTSwitch-LTSame  # LT in switch minus same condition

    # EXCLUSION CRITERIA;
    # The criteria is: 1) baby took more than 33 trials or less than 9 trials to habituate,
    # 2) LT at same or switch trials totalled to less than 1 second.
    if habTrials > 33 or habTrials < 9 or LTSame < 1 or LTSwitch < 1:
        # Participant excluded
        areTheyExcluded = True
    else:
        # Participant not excluded
        areTheyExcluded = False

    # ADDING ALL CLEAN DATA TO ONE LIST
    # The order of variables in the list will be the order of information in the new
    # participant row in the resulting csv file. The order of column headers should match the order in the list.

    cleanDataList = []
    cleanDataList.extend((partID, sex, condition, habTrials, PreTestLT, LTSame,
                          LTSwitch, totalSameSwitchLT, PostTestLT, totalPrePostLT, diffScoreSwitchSame,
                          overallLT, areTheyExcluded))
    allCleanData.append(cleanDataList) # I add the list of data for each participant to one superior list

allCleanData.sort()  # This superior list contains all participants' data, and i sort it according to participant number
# in ascending order

headers = ["Participant ID", "Sex (1 = male, 2 = female)", "Sound Condition",
               "Habituation Trials", "LT Pre-test", "LT Same Trials",
               "LT Switch Trials", "LT both same and switch", "LT Post-test", "LT total of pre and post-test",
               "Difference score switch-same", "Overall Experiment LT"]

# NEW CSV FILES WITH  CLEAN DATA

cleanDataFile = open("allCleanData.csv",'w', newline='')  # Creating CSV file for all clean participant data
exclFile = open("analysisData.csv", 'w', newline='')  # Creating CSV file for clean non-excluded participant data

# Writing the header to the csv file with all participants:
allDataWriter = csv.writer(cleanDataFile)
allDataWriter.writerow(headers)

# Adding the header to the csv file with only non-excluded participants :
exclFileWriter = csv.writer(exclFile)
exclFileWriter.writerow(headers)

# Writing the processed data into the two csv files:
for participant in allCleanData:
    allDataWriter.writerow(participant[:-1])  # adding each participant data to csv with all participants
    if participant[-1] == False:  # If participant not excluded -
        exclFileWriter.writerow(participant[:-1])  # Adding each non-excluded participant to second csv.

# CLOSING FILES:
rawDataFile.close()
cleanDataFile.close()
exclFile.close()
