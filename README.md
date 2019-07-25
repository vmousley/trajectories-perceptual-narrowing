This project contains materials (e.g., scripts, practice data files, etc.) to replicate the analyses conducted in: Developmental trajectories among monolingual and bilingual infants (Stage 1: Registered Report, submitted to Developmental Science).

The folder called 'Analysis' contains a folder of 10 sample data sets (from an adult co-author), as well as the pre-processing script called 'codeCleanData.py'. If you run 'this'codeCleanData.py' on your local machine, it will clean the sample data sets and create two new .csv files: 'allCleanData' and 'analysisData' (though note that if you wish to re-create them yourself, you need to erase the existing versions in the downloadable folder.) To understand what the pre-processing script does and what each .csv contains, please read the annotations found within the 'codeCleanData.py' script. You can then open the 'analysiscode.R' script and run the analyses on the 'analysisData.csv.' The 'analysiscode.R' script also calls the sample behavioural data from the 'behaviouralData.csv' in the 'Analysis' folder. The analyses conducted in the 'analysiscode.R' script correspond with the 'Analysis Pipeline' section of the Stage 1 Registered Report. 

The 'Language exclusion' folder contains materials referenced in the 'Participants' section of the Stage 1 Registered Report. Specifically, there are 10 csv's downloaded according to the explanation in our paper. The 'ExclusionLanguages.R' script organises these 10 csvs and produces the document called 'ExclusionLanguages.yaml,' also included in the folder. All should be replicable. 

The 'Quesitonnaires' folder includes background language questionnaires mentioned in the Stage 1 Registered Report.

The 'Stimuli presentation' folder contains the Python scripts that constitute the PyHab software used for this study. While you can download and read these scripts for more information about the software, you cannot run them because we do not own (and therefore cannot distribute) the stimuli files used in this study.

The other relevant document in the master branch is 'PowerAnalysis.R' which contains our power analysis. 
