# "Developmental trajectories of perceptual narrowing among monolingual and bilingual infants"
# Stage 1: Registered Report
# XXXXXXXXXXXXXXXXXXXXX
# Submitted to Developmental Science


## DESCRIPTION:
# This code processes all output from PyHab into two csv files. The two csv files are identical, but one has
# pre-processed data from ALL participants and one has pre-processed data from all NON-EXCLUDED participants
# The files contain participant ID, age, sex, sound condition, how many trials the baby took to habituate,
# and different total looking times across experiment phases. Looking time is shortened to LT throughout.

## HOW DOES IT WORK:
# When extracting data from the PyHab output, this code uses the variable names in the header.
# If these variable names change, then the script needs to be updated under 'VARIABLE NAMES'.

## BEFORE RUNNING THIS CODE:
# Make sure the codeCleanData.py file and the data-folder is in the same directory. Data from PyHab will be organised
# in folders where each folder is one participant. The path to these folders can be adjusted on line 26, and line 29 -
# but it is currently set up to work only if the codecleandata.py file is in the same folder as all participant-folders.

import os
from os import listdir
import csv

foldername = ''# CHANGE ACCORDING TO THE FOLDER THE DATA IS IN


# List of NParticipant folders
listAllFolders =listdir()# ADD THE PATH IN THE LISTDIR FUNCTION. CURRENTLY THE SCRIPT IS RUNNING IN THE SAME FOLDER AKA NOT NECESSARY.
listPartFolders = []
for file in listAllFolders:
    if file[4:10] == 'PYTHON':
        listPartFolders.append(file) #one folder per participant

NSubj = len(listPartFolders)
print('Found ' + str(NSubj) + ' participants')

# OBTAINING LIST OF NON-VERBOSE CSV FILES IN FOLDER

#SETUP
allCleanData = []
listNamesExcluded = []
listNamesNoData = []
listNamesIncluded = []

# VARIABLE NAMES: CHANGE IF THE HEADERS IN CSV CHANGE (FIRST ROW)
months_VarName      = 'months' #csv variable name for sex
days_VarName        = 'days' #csv variable name for sex
sex_VarName         = 'sex' #csv variable name for sex
condLabel_VarName   = 'condLabel' #csv variable name for sex
trialType_VarName   = 'trialType' #csv variable name for sex
sumOnA_VarName      = 'sumOnA'


for file in listPartFolders:

    #OBTAINING PARTICIPANT ID FROM THE FIRST 4 CHARACTERS IN FOLDER NAME
    SubjNum = file[0:4]
    print('Processing participant ' + str(SubjNum))

    # OPENING THE NON-VERBOSE DATA FILE
    listFiles = listdir(file)#list all files in participant folder
    wordsNotWanted = ['VERBOSE', 'Stats']
    for dataFile in listFiles:
        if wordsNotWanted[0] not in dataFile and wordsNotWanted[1] not in dataFile:
            dataFile_correct = dataFile
            print('found non-verbose, non-stats datafile: ' + dataFile_correct)

    with open(file+'/'+dataFile_correct) as rawDataFile:
        print('Starting Pre-processing...')
        reader = csv.reader(rawDataFile)
        rawData = []  # This list will contain all information in participant file.
        rowCount = 0  # This is to keep track of how many rows the csv file is. Later on the code uses this number when
        # indexing to avoid indexing a row that does not exist.
        for row in reader:
            rawData.append(row)  # I add each row in the file to the rawData list.
            rowCount += 1
    if rowCount < 10:
        print('participant did not finish experiment - processing is aborted.')
        listNamesNoData.append(SubjNum)
        continue #skips this participant and goes to next participant


    # DEMOGRAPHICS (SubjNum, group, age, sex, condition):
    # SEX
    sexIndex = rawData[0].index(sex_VarName)
    sex = rawData[1][sexIndex] # collects sex on row 2 column 4. NOTE: 1 is male, 2 is female

    # AGE
    monthsIndex = rawData[0].index(months_VarName)
    daysIndex = rawData[0][monthsIndex]

    #Sound condition
    condIndex = rawData[0].index(condLabel_VarName)
    condition = rawData[1][condIndex]  # This obtains the sound condition, by getting the integer value of the
    # last two letters of the label e.g. "LEAP07" => 7

    # HABITUATION TRIAL NUMBER, AND LOOKING TIME DATA
    # Number of habituation trials is collected through counting the amount of times "Hab" is mentioned in
    # TrialType column.
    # LT is collected through summing the LT on each trial for each testing phase (i.e. pre-test, same, switch,
    # post-test). In the csv file, this data is under "SumOnA" (column 12)

    habTrials = 0  # Counts habituation trials
    LTAttenGet = []  # Counts LT on attention getters (pre and post)
    LTSame = 0  # Counts LT on same trials
    LTSwitch = 0  # Counts LT on switch trials
    overallLT = 0  # Counts overall experiment LT

    trialTypeIndex      = rawData[0].index(trialType_VarName)
    sumOnAIndex         = rawData[0].index(sumOnA_VarName)

    for row in range(1,rowCount):  #The program will analyse the csv file row by row (except header), looking for keywords
        # "Hab", "AttenGet", "Same" or "Switch" to extract relevant info. Overall time also collected

        if rawData[row][trialTypeIndex] == "Hab":
            habTrials += 1 # This counts number of habituation trials

        if rawData[row][trialTypeIndex] == "AttenGet":
            LTAttenGet.append(float(rawData[row][sumOnAIndex])) # LT on attention getters, both pre and post

        if rawData[row][trialTypeIndex] == "Same":
            LTSame += float(rawData[row][sumOnAIndex])  # LT on same condition


        if rawData[row][trialTypeIndex] == "Switch":
            LTSwitch +=float(rawData[row][sumOnAIndex])  # LT on switch condition

        # Collecting overall looking time:
        overallLT += float(rawData[row][sumOnAIndex])  # This collects overall LT

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
        listNamesExcluded.append(SubjNum)
    else:
        # Participant not excluded
        areTheyExcluded = False
        listNamesIncluded.append(SubjNum)

    # ADDING ALL CLEAN DATA TO ONE LIST
    # The order of variables in the list will be the order of information in the new
    # participant row in the resulting csv file. The order of column headers should match the order in the list.

    cleanDataList = []
    cleanDataList.extend((SubjNum, sex, condition, habTrials, PreTestLT, LTSame,
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

# Writing the pre-processed data into the two csv files:
for participant in allCleanData:
    allDataWriter.writerow(participant[:-1])  # adding each participant data to csv with all participants
    if participant[-1] == False:  # If participant not excluded -
        exclFileWriter.writerow(participant[:-1])  # Adding each non-excluded participant to second csv.



# CLOSING FILES:
rawDataFile.close()
cleanDataFile.close()
exclFile.close()


# SUMMARY:
print('\n\nPre-processing completed! Here is a summary: \n ')

print('Participants included for analysis : ')
for name in listNamesIncluded:
    print(name)

print('\nParticipants Excluded for analysis : ')
for name in listNamesExcluded:
    print(name)

print('\nParticipants with no data : ')
for name in listNamesNoData:
    print(name)