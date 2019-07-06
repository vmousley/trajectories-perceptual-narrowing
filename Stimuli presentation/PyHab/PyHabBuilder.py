from psychopy import visual, event, core, gui, monitors, tools, sound,__version__
from psychopy.app import coder
import wx, random, csv, shutil, os, sys, threading
from math import *


class PyHabBuilder:
    """
    Changelist from 0.4:

    MaxOff and minOn are now trial-type-specific

    Now supports dynamic peak habituation criteiron and moving vs. fixed-window habituation evaluation.

    Now supports different types of stimuli - Movies, images, audio, and images with audio.

    Changed how stimuli are added to experiment - you now create a "stim library" and separately associate items in it
    with different trial types.

    TODO: Option for habituation over whole meta-trials not just the "hab" portion
    """
    def __init__(self, loadedSaved=False, settingsDict={}):
        """

        :param loadedSaved: Are we loading from a saved settings file?
        :type loadedSaved: bool
        :param settingsDict: If we are loading from a saved file, this is the content of that file passed by the launcher
        :type settingsDict: dict
        """

        self.loadSave = loadedSaved #For easy reference elsewhere
        self.dirMarker = os.sep
        if os.name is 'posix': #glorious simplicity of unix filesystem
            otherOS = '\\'
        elif os.name is 'nt': #Nonsensical Windows-based contrarianism
            otherOS = '/'
        #The base window
        width = 1080
        height = 600
        self.win = visual.Window((width,height),fullscr=False, allowGUI=True, rgb=[-1,-1,-1], units='norm') #Using normalized units.
        self.flowArea = [-1,.75,1,0] #norm units go from -1 to +1. To cover the top half of the screen would be -1 to 1, and 1 to 0. X,X,Y,Y
        self.flowRect = visual.Rect(self.win, width=self.flowArea[1]-self.flowArea[0],height=self.flowArea[3]-self.flowArea[2], fillColor='grey',
                pos=[self.flowArea[0]+float(abs(self.flowArea[1]-self.flowArea[0]))/2,self.flowArea[2]-float(abs(self.flowArea[3]-self.flowArea[2]))/2])
        self.paletteArea = [.75,float(1),float(1),0] #a trial type pallette, bottom right for now.
        self.paletteRect = visual.Rect(self.win, width=self.paletteArea[1]-self.paletteArea[0],height=self.paletteArea[3]-self.paletteArea[2], fillColor='white',
                pos=[self.paletteArea[0]+float(abs(self.paletteArea[1]-self.paletteArea[0]))/2,self.paletteArea[2]-float(abs(self.paletteArea[3]-self.paletteArea[2]))/2])
        self.aspect = float(height)/float(width) #Determine aspect ratio width/height. Impt. for using norm.
        #A bunch of useful stuff for drawing the interface
        self.colorsArray= ['red','blue','green','purple','brown','LightSeaGreen','gold','Magenta'] #colors for dif trial types. Will eventually need an arbitrary number...
        self.flowWidMult = .07
        self.flowWidthObj = self.flowWidMult*float(abs(self.flowArea[1]-self.flowArea[0])) #Width of one item in the flow, though this will possibly have to change...
        self.flowHeightObj = (self.flowWidthObj/self.aspect)*.8
        self.typeWidthObj = .4*float(abs(self.paletteArea[1]-self.paletteArea[0])) #Width of one item in the flow, though this will possibly have to change...
        self.typeHeightObj = (self.typeWidthObj/self.aspect)*.6
        self.typeLocs =[]
        self.flowLocs =[]
        self.overFlowLocs = [] # For >20 trials, go up to 40
        self.flowGap = .09 # A easy reference for the horizontal spacing of items in the flow
        self.condDict = {} #For creating conditions
        self.mouse = event.Mouse()
        for x in [.25,.75]: #Two columns of trial types
            for z in range(1,5):
                self.typeLocs.append([self.paletteArea[0]+x*(self.paletteArea[1]-self.paletteArea[0]),
                                      self.paletteArea[2]+.2*(self.paletteArea[3]-self.paletteArea[2])+z*.15*(self.paletteArea[3]-self.paletteArea[2])])
        for y in [.25,.75]: #two rows for the study flow.
            for z in range(1,11):
                    self.flowLocs.append([self.flowArea[0]+z*(self.flowArea[1]-self.flowArea[0])*self.flowGap,
                                          self.flowArea[2]+y*(self.flowArea[3]-self.flowArea[2])])
        for y in [.2, .4, .6, .8]:  # two rows for the study flow.
            for z in range(1, 11):
                self.overFlowLocs.append([self.flowArea[0] + z * (self.flowArea[1] - self.flowArea[0]) * self.flowGap,
                                      self.flowArea[2] + y * (self.flowArea[3] - self.flowArea[2])])
        # loadedSaved is "is this a new experiment or are we operating inside an existing experiment's folder?"
        if not loadedSaved:  # A new blank experiment
            # Load some defaults to start with.
            self.settings = {'dataColumns': ['sNum', 'months', 'days', 'sex', 'cond','condLabel', 'trial','GNG','trialType','stimName','habCrit','sumOnA','numOnA','sumOffA','numOffA','sumOnB','numOnB','sumOffB','numOffB'],
                                                        'prefix': 'PyHabExperiment',
                                                        'dataloc':'data'+self.dirMarker,
                                                        'maxDur': {},
                                                        'playThrough': {},
                                                        'movieEnd': [],
                                                        'maxOff': {},
                                                        'minOn': {},
                                                        'blindPres': '0', 
                                                        'autoAdvance': [],
                                                        'randPres': '0', 
                                                        'condPath': '', 
                                                        'condFile': '', 
                                                        'condList': [],
                                                        'trialOrder': [], 
                                                        'maxHabTrials': '14',
                                                        'setCritWindow': '3', 
                                                        'setCritDivisor': '2.0',
                                                        'setCritType': 'First',
                                                        'habThresh': '5.0',
                                                        'metCritWindow': '3', 
                                                        'metCritDivisor': '1.0',
                                                        'metCritStatic': 'Moving',
                                                        'habTrialList':[],
                                                        'stimPres': 0,  #Will be set on each run anyways.
                                                        'stimPath': 'stimuli'+self.dirMarker,
                                                        'stimNames': {},
                                                        'stimList': {},
                                                        'screenWidth': 1080, 
                                                        'screenHeight': 700,
                                                        'screenColor': 'black',
                                                        'movieWidth': 800, 
                                                        'movieHeight': 600, 
                                                        'screenIndex': '1', 
                                                        'ISI': {},
                                                        'freezeFrame': '0.0',
                                                        'playAttnGetter': {},
                                                        'attnGetterList':{'PyHabDefault':{'stimType':'Audio',
                                                                                          'stimName':'upchime1.wav',
                                                                                          'stimDur':2,
                                                                                          'stimLoc':'PyHab' + self.dirMarker + 'upchime1.wav',
                                                                                          'shape':'Rectangle',
                                                                                          'color':'yellow'}},
                                                        'folderPath':'',
                                                        'trialTypes':[],
                                                        'prefLook':'0',
                                                        'startImage':'',
                                                        'endImage':'',
                                                        'nextFlash':'0'}
            self.studyFlowArray={'lines':[],'shapes':[],'text':[],'labels':[]} # an array of objects for the study flow.
            self.trialTypesArray={'shapes':[],'text':[],'labels':[]}
        else:
            self.settings = settingsDict
            if 'nextFlash' not in self.settings.keys():
                self.settings['nextFlash'] = '0'
            # Settings requiring evaluation to get sensible values. Mostly dicts.
            evalList = ['dataColumns','maxDur','condList','movieEnd','playThrough','trialOrder','stimNames', 'stimList', 'ISI',
                        'maxOff','minOn','autoAdvance','playAttnGetter','attnGetterList','trialTypes','habTrialList','nextFlash']
            for i in evalList:
                self.settings[i] = eval(self.settings[i])
                if i in ['stimList','attnGetterList']:
                    for [q,j] in self.settings[i].items():
                        try:
                            j['stimLoc'] = ''.join([self.dirMarker if x == otherOS else x for x in j['stimLoc']])
                        except KeyError: # For image/audio pairs
                            j['audioLoc'] = ''.join([self.dirMarker if x == otherOS else x for x in j['audioLoc']])
                            j['imageLoc'] = ''.join([self.dirMarker if x == otherOS else x for x in j['imageLoc']])
            # Backwards compatibility for ISI
            if type(self.settings['ISI']) is not dict:
                tempISI = {}
                for q in range(0,len(self.settings['trialTypes'])):
                    tempISI[self.settings['trialTypes'][q]] = str(self.settings['ISI'])
                self.settings['ISI'] = tempISI
            # Backwards compatibility for startImage and endImage
            if 'startImage' not in self.settings.keys():
                self.settings['startImage'] = ''
                self.settings['endImage'] = ''
            if 'habThresh' not in self.settings.keys():
                self.settings['habThresh'] = '1.0'
            self.settings['dataloc'] = ''.join([self.dirMarker if x == otherOS else x for x in self.settings['dataloc']])
            self.settings['stimPath'] = ''.join([self.dirMarker if x == otherOS else x for x in self.settings['stimPath']])
            self.trialTypesArray = self.loadTypes()
            self.studyFlowArray = self.loadFlow()
            # Get conditions!
            if self.settings['randPres'] in [1,'1','True',True] or len(self.settings['condFile'])>0: #If there is a random presentation file...
                if os.path.exists(self.settings['condFile']):
                    testReader=csv.reader(open(self.settings['condFile'],'rU'))
                    testStuff=[]
                    for row in testReader:
                        testStuff.append(row)
                    testDict = dict(testStuff)
                    for i in testDict.keys():
                        testDict[i] = eval(testDict[i])
                    self.condDict = testDict
                else:
                    self.condDict={}
            self.settings['folderPath'] = os.getcwd()+self.dirMarker  # On load, reset the folder path to wherever you are now.
        self.folderPath = self.settings['folderPath']  # The location where all the pieces are saved.
        self.allDataColumns = ['sNum', 'months', 'days', 'sex', 'cond','condLabel', 'trial','GNG','trialType','stimName','habCrit','sumOnA','numOnA','sumOffA','numOffA','sumOnB','numOnB','sumOffB','numOffB']
        self.allDataColumnsPL = ['sNum', 'months', 'days', 'sex', 'cond','condLabel','trial','GNG','trialType','stimName','habCrit', 'sumOnL','numOnL','sumOnR','numOnR','sumOff','numOff']
        self.stimSource={}  # A list of the source folder(s) for each stimulus file, a dict where each key is the filename in stimNames?
        self.delList=[] # A list of stimuli to delete if they are removed from the experiment library.
        self.allDone=False
        # Various main UI buttons, put into a dict of lists for easy looping through.
        self.buttonList={'shapes':[],'text':[],'functions':[]}#Yes, python means we can put the functions in there too.
        if len(self.folderPath) > 0:
            #Make a "save" button, not just a "save as" button, but only if there is a place to save to!
            saveButton = visual.Rect(self.win,width=.15, height=.67*(.15/self.aspect), pos=[-.52,-.9],fillColor="green")
            saveText = visual.TextStim(self.win, text="SAVE",color="black",height=saveButton.height*.5, pos=saveButton.pos)
            self.buttonList['shapes'].append(saveButton)
            self.buttonList['text'].append(saveText)
            self.buttonList['functions'].append(self.saveEverything)
        saveAsButton = visual.Rect(self.win,width=.15, height=.67*(.15/self.aspect), pos=[-.22,-.9],fillColor="green")
        saveAsText = visual.TextStim(self.win, text="Save as",color="black",height=saveAsButton.height*.3, pos=saveAsButton.pos)
        self.buttonList['shapes'].append(saveAsButton)
        self.buttonList['text'].append(saveAsText)
        self.buttonList['functions'].append(self.saveDlg)
        newTrialTypeButton = visual.Rect(self.win, width=.9*(self.paletteArea[1]-self.paletteArea[0]),height=abs(self.paletteArea[3]-self.paletteArea[2])*.10, fillColor="yellow", lineColor="black",
                pos=[self.paletteArea[0]+float(abs(self.paletteArea[1]-self.paletteArea[0]))/2,self.paletteArea[2]-float(abs(self.paletteArea[3]-self.paletteArea[2])/12)])
        newTrialTypeText=visual.TextStim(self.win, alignHoriz='center', alignVert='center', text = "New Trial Type",height=.55*newTrialTypeButton.height, pos=newTrialTypeButton.pos,color="black")
        self.buttonList['shapes'].append(newTrialTypeButton)
        self.buttonList['text'].append(newTrialTypeText)
        self.buttonList['functions'].append(self.trialTypeDlg)
        delTrialTypeButton = visual.Rect(self.win, width=.9*(self.paletteArea[1]-self.paletteArea[0]),height=abs(self.paletteArea[3]-self.paletteArea[2])*.10, fillColor="red", lineColor = "black",
                pos=[self.paletteArea[0]+float(abs(self.paletteArea[1]-self.paletteArea[0]))/2,self.paletteArea[3]+float(abs(self.paletteArea[3]-self.paletteArea[2])/12)])
        delTrialTypeText=visual.TextStim(self.win, alignHoriz='center', alignVert='center', text = "Delete a trial type",height=.45*delTrialTypeButton.height, pos=delTrialTypeButton.pos,color="black")
        self.buttonList['shapes'].append(delTrialTypeButton)
        self.buttonList['text'].append(delTrialTypeText)
        self.buttonList['functions'].append(self.delTrialTypeDlg)
        addHabButton = visual.Rect(self.win, width=.9*(self.paletteArea[1]-self.paletteArea[0]),height=abs(self.paletteArea[3]-self.paletteArea[2])*.10, fillColor="yellow", lineColor="black",
                pos=[self.paletteArea[0]+float(abs(self.paletteArea[1]-self.paletteArea[0]))/2,self.paletteArea[2]-float(abs(self.paletteArea[3]-self.paletteArea[2])*.2)])
        if 'Hab' in self.settings['trialTypes']:
            txt = 'Mod Hab Block'
        else:
            txt = 'Add Hab Block'
        addHabText=visual.TextStim(self.win, alignHoriz='center', alignVert='center', text = txt,height=.55*addHabButton.height, pos=addHabButton.pos,color="black")
        self.buttonList['shapes'].append(addHabButton)
        self.buttonList['text'].append(addHabText)
        self.buttonList['functions'].append(self.addHabBlock)
        quitButton = visual.Rect(self.win,width=.15, height=.67*(.15/self.aspect), pos=[-.82,-.9],fillColor="red")
        quitText = visual.TextStim(self.win, text="QUIT",color="black",height=quitButton.height*.5, pos=quitButton.pos)
        self.buttonList['shapes'].append(quitButton)
        self.buttonList['text'].append(quitText)
        self.buttonList['functions'].append(self.quitFunc)
        USetButton = visual.Rect(self.win, width=.3, height=.5*(.2/self.aspect),pos=[-.75,-.2], fillColor="white")
        USetText = visual.TextStim(self.win, text="Universal \nsettings",color="black",height=USetButton.height*.3, alignHoriz='center', pos=USetButton.pos)
        self.buttonList['shapes'].append(USetButton)
        self.buttonList['text'].append(USetText)
        self.buttonList['functions'].append(self.univSettingsDlg)
        dataSetButton = visual.Rect(self.win, width=.3, height=.5*(.2/self.aspect),pos=[-.75,-.6], fillColor="white")
        dataSetText = visual.TextStim(self.win, text="Data \nsettings",color="black",height=dataSetButton.height*.3, alignHoriz='center', pos=dataSetButton.pos)
        self.buttonList['shapes'].append(dataSetButton)
        self.buttonList['text'].append(dataSetText)
        self.buttonList['functions'].append(self.dataSettingsDlg)
        stimSetButton = visual.Rect(self.win, width=.3, height=.5*(.2/self.aspect),pos=[-.25,-.2], fillColor="white")
        stimSetText = visual.TextStim(self.win, text="Stimuli \nsettings",color="black",height=stimSetButton.height*.3, alignHoriz='center', pos=stimSetButton.pos)
        self.buttonList['shapes'].append(stimSetButton)
        self.buttonList['text'].append(stimSetText)
        self.buttonList['functions'].append(self.stimSettingsDlg)
        condSetButton = visual.Rect(self.win, width=.3, height=.5*(.2/self.aspect),pos=[-.25,-.6], fillColor="white")
        condSetText = visual.TextStim(self.win, text="Condition \nsettings",color="black",height=condSetButton.height*.3, alignHoriz='center', pos=condSetButton.pos)
        self.buttonList['shapes'].append(condSetButton)
        self.buttonList['text'].append(condSetText)
        self.buttonList['functions'].append(self.condSettingsDlg)
        habSetButton = visual.Rect(self.win, width=.3, height=.5*(.2/self.aspect),pos=[.25,-.2], fillColor="white")
        habSetText = visual.TextStim(self.win, text="Habituation \nsettings",color="black",height=habSetButton.height*.3, alignHoriz='center', pos=habSetButton.pos)
        self.buttonList['shapes'].append(habSetButton)
        self.buttonList['text'].append(habSetText)
        self.buttonList['functions'].append(self.habSettingsDlg)

        attnGetterButton = visual.Rect(self.win, width=.3, height=.5*(.2/self.aspect), pos=[.75, -.2], fillColor = "white")
        attnGetterText = visual.TextStim(self.win, text="Customize \nattention-getters",color="black",height=attnGetterButton.height*.3,alignHoriz='center', pos=attnGetterButton.pos)
        self.buttonList['shapes'].append(attnGetterButton)
        self.buttonList['text'].append(attnGetterText)
        self.buttonList['functions'].append(self.attnGetterDlg)

        stimLibraryButton = visual.Rect(self.win, width=.3, height=.5*(.2/self.aspect), pos=[.25, -.6], fillColor = "white")
        stimLibraryText = visual.TextStim(self.win, text="Add/remove stimuli \nto/from exp. library",color="black",height=stimLibraryButton.height*.3,alignHoriz='center', pos=stimLibraryButton.pos)
        self.buttonList['shapes'].append(stimLibraryButton)
        self.buttonList['text'].append(stimLibraryText)
        self.buttonList['functions'].append(self.addStimToLibraryDlg)

        if len(list(self.settings['stimList'].keys())) > 0:
            addMovButton = visual.Rect(self.win, width=.3, height=.5 * (.2 / self.aspect), pos=[.75, -.6],
                                       fillColor="white")
            addMovText = visual.TextStim(self.win, text="Add stimulus files \nto trial types", color="black",
                                         height=addMovButton.height * .3, alignHoriz='center', pos=addMovButton.pos)
            self.buttonList['shapes'].append(addMovButton)
            self.buttonList['text'].append(addMovText)
            self.buttonList['functions'].append(self.addStimToTypesDlg)
        
        self.workingRect = visual.Rect(self.win, width=1, height=.5, pos=[0,0], fillColor = 'green') #Because there are certain things that take a while.
        self.workingText = visual.TextStim(self.win, text="Working...", height= .3, bold=True, alignHoriz='center', pos=[0,0])


    def run(self):
        """
        Exists exclusively to be called to start the main loop.

        :return:
        :rtype:
        """
        self.mainLoop()
    
    def mainLoop(self):
        """
        Main loop of the whole program.

        :return:
        :rtype:
        """
        while self.allDone==False:        
            self.showMainUI()
            self.win.flip()
            #Check all the stand-alone buttons
            for i in range(0, len(self.buttonList['shapes'])):
                if self.mouse.isPressedIn(self.buttonList['shapes'][i],buttons=[0]): #left-click only
                    self.showMainUI()
                    self.workingRect.draw()
                    self.workingText.draw()
                    self.win.flip()
                    if os.name is not 'posix':
                        while self.mouse.getPressed()[0] == 1:
                            pass
                        self.win.winHandle.set_visible(visible=False)
                    if self.buttonList['functions'][i] == self.addHabBlock: #one special case
                        if self.buttonList['text'][i].text == "Mod Hab Block":
                            self.addHabBlock(makeNew=False)
                        else:
                            self.addHabBlock()
                    else:
                        self.buttonList['functions'][i]() #It should not be this easy, BUT IT IS. Python is great.
                    if os.name is not 'posix':
                        self.win.winHandle.set_visible(visible=True)
            #Check all the trial type elements.
            for j in range(0,len(self.trialTypesArray['shapes'])):
                if self.mouse.isPressedIn(self.trialTypesArray['shapes'][j],buttons=[0]): #Left-click, add to study flow, at end.
                    self.settings['trialOrder'].append(self.trialTypesArray['labels'][j])
                    self.studyFlowArray=self.loadFlow() #Reloads the study flow with the new thing added.
                    while self.mouse.isPressedIn(self.trialTypesArray['shapes'][j],buttons=[0]): #waits until the mouse is released before continuing.
                        pass
                elif self.mouse.isPressedIn(self.trialTypesArray['shapes'][j],buttons=[1,2]): #Right-click, modify trial type info.
                    self.showMainUI()
                    self.workingRect.draw()
                    self.workingText.draw()
                    self.win.flip()
                    if os.name is not 'posix': #There's an issue with windows and dialog boxes, don't ask.
                        while 1 in self.mouse.getPressed():
                            pass
                        self.win.winHandle.set_visible(visible=False)
                    self.trialTypeDlg(trialType = self.trialTypesArray['labels'][j],makeNew = False)
                    if os.name is not 'posix':
                        self.win.winHandle.set_visible(visible=True)
            for k in range(0, len(self.studyFlowArray['shapes'])):
                if self.mouse.isPressedIn(self.studyFlowArray['shapes'][k]):
                    self.moveTrialInFlow(k)
                    break
        self.win.close()
        
            
    def showMainUI(self):
        """
        Main draw loop of the primary builder interface

        :return:
        :rtype:
        """
        self.flowRect.draw()        #Draw flow area and study flow
        for i in range(0, len(self.studyFlowArray['lines'])):
            self.studyFlowArray['lines'][i].draw()
        for j in range(0,len(self.studyFlowArray['labels'])):
            self.studyFlowArray['shapes'][j].draw()
            self.studyFlowArray['text'][j].draw()
        #Buttons for trial types, etc.
        self.paletteRect.draw() #Palette of trial types.
        for i in range(0, len(self.trialTypesArray['labels'])):
            self.trialTypesArray['shapes'][i].draw()
            self.trialTypesArray['text'][i].draw()
        #General buttons
        for i in range(0, len(self.buttonList['shapes'])):
            self.buttonList['shapes'][i].draw()
            self.buttonList['text'][i].draw()
    
    def trialTypeDlg(self, trialType="TrialTypeNew", makeNew=True, prevInfo=[]):
        """
        Dialog for creating OR modifying a trial type. Allows you to set
        the maximum duration of that trial type as well as remove movies
        from it, and also set whether the trial type is gaze contingent.
        Now also sets whether the study should auto-advance into this
        trial and whether the built-in attention-getter should be used.

        The dialog by default outputs a list with 8 items in it.
        0 = trial type name

        1 = Maximum duration of trials of this type

        [if movies assigned to trial type already, they occupy 2 - N]

        2/-7 = Gaze-contingent trial type?

        3/-6 = Maximum continuous looking-away to end trial of type

        4/-5 = Minimum on-time to enable off-time criterion (not continuous)

        5/-4 = Auto-advance into trial?

        6/-3 = Attention-getter selection

        7/-2 = End trial on movie end or mid-movie

        8/-1 = inter-stimulus interveral (ISI) for this trial type

        :param trialType: Name of the trial type
        :type trialType: str
        :param makeNew: Making a new trial type or modifying an existing one?
        :type makeNew: bool
        :param prevInfo: If user attempts to create an invalid trial type, the dialog is re-opened with the previously entered information stored and restored
        :type prevInfo: list
        :return:
        :rtype:
        """
        self.showMainUI()
        self.workingRect.draw()
        self.workingText.draw()
        self.win.flip()
        #For when a trial is right-clicked, or a new one created, open a dialog with info about it.
        skip = False
        if len(self.trialTypesArray['labels']) == 7:
            errDlg = gui.Dlg(title="Max trial types reached!")
            errDlg.addText("PyHab's builder currently supports a maximum of 7 trial types + hab trials.")
            errDlg.show()
        else:
            typeDlg = gui.Dlg(title="Trial Type " + trialType)
            typeDlg.addField("Trial type name: ", trialType)
            if not makeNew:  # if this modifying an existing trial type, pre-fill the existing info.
                if len(prevInfo) == 0:  # allows for removal of movies from the trial type
                    typeDlg.addField("Max duration", self.settings['maxDur'][trialType])
                    maxOff = self.settings['maxOff'][trialType]
                    minOn = self.settings['minOn'][trialType]
                    ISI = self.settings['ISI'][trialType]
                    if len(self.settings['stimNames'][trialType]) > 0:
                        typeDlg.addText("Current movie files in trial type (uncheck to remove)")
                        for i in range(0,len(self.settings['stimNames'][trialType])):
                            typeDlg.addField(self.settings['stimNames'][trialType][i], initial=True)
                else:
                    typeDlg.addField("Max duration", prevInfo[1])
                    maxOff = prevInfo[-6]
                    minOn = prevInfo[-5]
                    ISI = prevInfo[-1]
                    if len(prevInfo) > 9:  # If there were no movies to start with, this will have a length of 9.
                        typeDlg.addText("Current stimuli in trial type (uncheck to remove)")
                        for i in range(0,len(self.settings['stimNames'][trialType])):
                            typeDlg.addField(self.settings['stimNames'][trialType][i], initial=prevInfo[i+2])
                # Find the index of the existing trial type in the study flow and type pane.
                flowIndexes=[]
                for i in range(0,len(self.studyFlowArray['labels'])):
                    if self.studyFlowArray['labels'][i] == trialType:
                        flowIndexes.append(i) 
                typeIndex =self.trialTypesArray['labels'].index(trialType)
                if self.settings['playThrough'][trialType] == 2:
                    chz = ["No", "OnOnly", "Yes"]
                elif self.settings['playThrough'][trialType] == 1:
                    chz = ["OnOnly", "Yes", "No"]
                else:
                    chz = ["Yes", "OnOnly", "No"]
            elif len(prevInfo) > 0:
                typeDlg.addField("Max duration", prevInfo[1])
                maxOff = prevInfo[-5]
                minOn = prevInfo[-4]
                if prevInfo[4] == 2:
                    chz = ["No", "OnOnly", "Yes"]
                elif prevInfo[4] == 1:
                    chz = ["OnOnly", "Yes", "No"]
                else:
                    chz = ["Yes", "OnOnly", "No"]
            else:  # if there are no existing indexes to refer to
                typeDlg.addField("Max duration", 60.0)
                maxOff = 2.0
                minOn = 1.0
                ISI = 0.0
                chz = ["Yes", "OnOnly", "No"]
            typeDlg.addField("Gaze-contingent trial type (next two lines ignored otherwise)", choices=chz)
            typeDlg.addField("Number of continuous seconds looking away to end trial", maxOff)
            typeDlg.addField("Minimum time looking at screen before stimuli can be ended (not consecutive)", minOn)
            if trialType in self.settings['autoAdvance']:
                chz2 = True
            else:
                chz2 = False
            typeDlg.addField("Auto-advance INTO trial without waiting for expeirmenter?", initial=chz2)
            if trialType not in self.settings['playAttnGetter']:
                ags = list(self.settings['attnGetterList'].keys())
                chz3 = [x for x in ags if x is not 'PyHabDefault']
                if makeNew:
                    chz3.insert(0, 'None')
                    chz3.insert(0, 'PyHabDefault')  # Defaults to...well, the default
                else:
                    chz3.insert(0, 'PyHabDefault')
                    chz3.insert(0, 'None') # Only default to default if this is a new trial type, not if "none" was selected before.
            elif trialType in self.settings['playAttnGetter']:
                chz3 = [x for x in list(self.settings['attnGetterList'].keys()) if x is not self.settings['playAttnGetter'][trialType]]
                chz3.insert(0, 'None')
                chz3.insert(0, self.settings['playAttnGetter'][trialType])
            typeDlg.addField("Attention-getter for this trial type (Stim presentation mode only)", choices = chz3)
            if trialType in self.settings['movieEnd']:
                chz4 = True
            else:
                chz4 = False
            typeDlg.addField("Only end trial on end of movie repetition? (Only works when presenting stimuli)", initial = chz4)
            typeDlg.addField("Inter-stimulus interval on loops (pause between end of one loop and start of next)", ISI)
            typeInfo = typeDlg.show()
            if typeDlg.OK:
                # Check if all the things that need to be numbers are actually numbers.
                for i in [1, len(typeInfo) - 5, len(typeInfo) - 4]:
                    if not isinstance(typeInfo[i], float) and not isinstance(typeInfo[i], int):
                        try:
                            typeInfo[i] = eval(typeInfo[i])
                        except:
                            warnDlg = gui.Dlg(title="Warning!")
                            warnDlg.addText(
                                "Number expected, got text instead. \nPlease make sure maximum duration, minimum on-time, and maximum off-time are all numbers!")
                            warnDlg.show()
                            skip = True
                            self.trialTypeDlg(str(typeInfo[0]), makeNew, typeInfo)
                # Update all the things, or create them.
                if not skip:
                    typeInfo[0] = str(typeInfo[0])  # Ditch PyQT mess I hope.
                    if typeInfo[0] is not trialType:  # First, do we need to change the trial type label for an existing type?
                        if not makeNew and typeInfo[0] not in self.trialTypesArray['labels']:
                            # change all the dicts and everything.
                            self.settings['stimNames'][typeInfo[0]] = self.settings['stimNames'].pop(trialType)
                            self.settings['maxDur'][typeInfo[0]]=self.settings['maxDur'].pop(trialType)
                            self.settings['playThrough'][typeInfo[0]] = self.settings['playThrough'].pop(trialType)
                            self.settings['maxOff'][typeInfo[0]] = self.settings['maxOff'].pop(trialType)
                            self.settings['minOn'][typeInfo[0]] = self.settings['minOn'].pop(trialType)
                            # update trial type and study flow too
                            numChar = len(typeInfo[0])
                            if numChar <= 3:
                                numChar = 4  # Maximum height
                            for i in flowIndexes:
                                self.studyFlowArray['labels'][i] = typeInfo[0]
                                self.studyFlowArray['text'][i].text = typeInfo[0]
                                self.studyFlowArray['text'][i].height = self.flowHeightObj/(.42*numChar) #Update text height for new length
                            self.trialTypesArray['labels'][typeIndex] = typeInfo[0]
                            self.trialTypesArray['text'][typeIndex].text = typeInfo[0]
                            self.trialTypesArray['text'][typeIndex].height=self.typeHeightObj/(.33*numChar)
                            self.settings['trialTypes'] = [typeInfo[0] if x == trialType else x for x in self.settings['trialTypes']]
                            self.settings['trialOrder'] = [typeInfo[0] if x == trialType else x for x in self.settings['trialOrder']]
                        elif typeInfo[0] in self.trialTypesArray['labels']:
                            #warning dialog, start over with all info entered so far.
                            warnDlg = gui.Dlg(title="Warning!")
                            warnDlg.addText("New trial type label matches an existing trial type! Please choose a different name for this trial type.")
                            warnDlg.show()
                            skip = True
                            self.trialTypeDlg(typeInfo[0], makeNew,typeInfo)
                        trialType = typeInfo[0]
                    if not skip:

                        self.settings['maxDur'][trialType] = typeInfo[1] #Update maxDur
                        self.settings['maxOff'][trialType] = typeInfo[len(typeInfo)-6]
                        self.settings['minOn'][trialType] = typeInfo[len(typeInfo)-5]
                        self.settings['ISI'][trialType] = typeInfo[len(typeInfo)-1]

                        # Gaze-contingency settings
                        if trialType not in self.settings['playThrough'].keys(): #Initialize if needed.
                            self.settings['playThrough'][trialType] = 0
                        if typeInfo[len(typeInfo)-7] == "Yes" and self.settings['playThrough'][trialType] is not 0: #gaze-contingent trial type, not already tagged as such.
                            self.settings['playThrough'][trialType] = 0
                        elif typeInfo[len(typeInfo)-7] == "No" and self.settings['playThrough'][trialType] is not 2:
                            self.settings['playThrough'][trialType] = 2
                        elif typeInfo[len(typeInfo)-7] == "OnOnly" and self.settings['playThrough'][trialType] is not 1:
                            self.settings['playThrough'][trialType] = 1

                        # Auto-advance settings
                        if typeInfo[len(typeInfo)-4] in [False,0,'False','0'] and trialType in self.settings['autoAdvance']: #gaze-contingent trial type, not already tagged as such.
                            self.settings['autoAdvance'].remove(trialType)
                        elif typeInfo[len(typeInfo)-4] in [True, 1, 'True', '1'] and not trialType in self.settings['autoAdvance']:
                            self.settings['autoAdvance'].append(trialType)

                        # Attention-getter settings
                        if typeInfo[len(typeInfo)-3] == 'None':
                            if trialType in self.settings['playAttnGetter']:
                                del self.settings['playAttnGetter'][trialType]
                        else:
                            if trialType not in self.settings['playAttnGetter']:  # If it did not have an attngetter before.
                                agname = typeInfo[len(typeInfo)-3]
                                self.settings['playAttnGetter'][trialType] = agname
                            elif typeInfo[len(typeInfo) - 3] is not self.settings['playAttnGetter'][trialType]:
                                # If a different attention-getter has been selected
                                agname = typeInfo[len(typeInfo) - 3]
                                self.settings['playAttnGetter'][trialType] = agname

                        # End-trial-on-movie-end settings
                        if typeInfo[len(typeInfo)-2] in [False,0,'False','0'] and trialType in self.settings['movieEnd']:
                            self.settings['movieEnd'].remove(trialType)
                        elif typeInfo[len(typeInfo)-2] in [True, 1, 'True', '1'] and not trialType in self.settings['movieEnd']:
                            self.settings['movieEnd'].append(trialType)


                        # Remove stimuli if needed
                        if len(typeInfo) > 9: #Again, if there were movies to list.
                            tempMovies = [] #This will just replace the stimNames list
                            for i in range(0,len(self.settings['stimNames'][trialType])):
                                if typeInfo[i+2]:
                                    tempMovies.append(self.settings['stimNames'][trialType][i])
                            self.settings['stimNames'][trialType] = tempMovies

                        # If we need to update the flow pane, it's taken care of above. Here we update the type pallette.
                        if makeNew:
                            i = len(self.trialTypesArray['labels']) #Grab length before adding, conveniently the index we need for position info etc.
                            self.trialTypesArray['labels'].append(typeInfo[0])
                            tempObj = visual.Rect(self.win,width=self.typeWidthObj, height=self.typeHeightObj, fillColor=self.colorsArray[i], pos=self.typeLocs[i])
                            numChar = len(typeInfo[0])
                            if numChar <= 3:
                                numChar = 4 #Maximum height
                            tempTxt = visual.TextStim(self.win, alignHoriz='center', alignVert='center', bold=True, height=self.typeHeightObj/(.38*numChar), text=typeInfo[0], pos=self.typeLocs[i])
                            self.trialTypesArray['shapes'].append(tempObj)
                            self.trialTypesArray['text'].append(tempTxt)
                            self.settings['trialTypes'].append(typeInfo[0])
                            self.settings['stimNames'][typeInfo[0]] = []
                            # If there exists a condition file or condition settings, warn the user that they will need to be updated!
                            if self.settings['condFile'] is not '':
                                warnDlg = gui.Dlg(title="Update conditions")
                                warnDlg.addText("WARNING! UPDATE CONDITION SETTINGS AFTER ADDING STIMULI TO THIS TRIAL TYPE! \nIf you do not update conditions, the experiment will crash whenever it reaches this trial type.")
                                warnDlg.show()
                self.studyFlowArray = self.loadFlow()
                self.showMainUI()
                self.win.flip()
    
    def addHabBlock(self, makeNew = True):
        """
        Creates a hab trial type, which consists of a hab trial plus, now, some other number of trials
        It essentially needs to create a sub-flow.

        0 = Maximum duration

        1 = Maximum continuous off-time

        2 = Minimum on-time

        [If stimulus files associated with type, these occupy 3-N]

        3/-5 = Auto-advance into trial

        4/-4 = Select attention-getter

        5/-3 = Inter-stimulus interval (ISI)

        6/-2 = Use sub-block structure?

        7/-1 = Number of trial types in sub-block, including hab

        TODO: Compute hab over whole thing versus just 'hab' trial.


        :return:
        :rtype:
        """
        #Some stuff is predetermined, specifically name and gaze-contingency
        typeDlg = gui.Dlg(title="Hab block creator")
        typeDlg.addText("Hab trial settings")
        if not makeNew:
            typeDlg.addField("Maximum duration", self.settings['maxDur']['Hab'])
            typeDlg.addField("Number of continuous seconds looking away to end trial", self.settings['maxOff']['Hab'])
            typeDlg.addField("Minimum time looking at screen before stimuli can be ended (not consecutive)",
                             self.settings['minOn']['Hab'])
            ISI = self.settings['ISI']['Hab']
            if len(self.settings['stimNames']['Hab']) > 0:
                typeDlg.addText("Current movie files in trial type (uncheck to remove)")
                for i in range(0, len(self.settings['stimNames']['Hab'])):
                    typeDlg.addField(self.settings['stimNames']['Hab'][i], initial=True)

        else:
            typeDlg.addField("Maximum duration", 60.0)
            typeDlg.addField("Number of continuous seconds looking away to end trial", 2.0)
            typeDlg.addField("Minimum time looking at screen before stimuli can be ended (not consecutive)", 1.0)
            ISI = 0.0
        if 'Hab' in self.settings['autoAdvance']:
            chz2 = True
        else:
            chz2 = False
        typeDlg.addField("Auto-advance INTO trial without waiting for expeirmenter?", initial=chz2)
        if 'Hab' not in self.settings['playAttnGetter']:
            ags = list(self.settings['attnGetterList'].keys())
            chz3 = [x for x in ags if x is not 'PyHabDefault']
            chz3.insert(0, 'None')
            chz3.insert(0, 'PyHabDefault')  # Defaults to...well, the default
        elif 'Hab' in self.settings['playAttnGetter']:
            chz3 = [x for x in list(self.settings['attnGetterList'].keys()) if
                    x is not self.settings['playAttnGetter']['Hab']]
            chz3.insert(0, 'None')
            chz3.insert(0, self.settings['playAttnGetter']['Hab'])
        typeDlg.addField("Attention-getter for this trial type (Stim presentation mode only)", choices=chz3)
        typeDlg.addField("Inter-stimulus interval on loops (pause between end of one loop and start of next)", ISI)
        typeDlg.addText("Hab block sub-trials")
        if not makeNew:
            if len(self.settings['habTrialList']) > 0:
                chz = True
                numSub = len(self.settings['habTrialList'])
            else:
                chz = False
                numSub = 1
        else:
            chz = False
            numSub = 1
        typeDlg.addField("Use sub-trials? (If checked, new dialog will open)", initial=chz)
        typeDlg.addField("Number of sub-trials (INCLUDING Hab trial; only matters if box is checked)", numSub)
        habInfo = typeDlg.show()

        if typeDlg.OK:
            skip = False
            # On OK, create a new ui with a drop-down from trialtypes that includes hab.
            for i in [0,1,2]:
                if not isinstance(habInfo[i], float) and not isinstance(habInfo[i], int):
                    try:
                        habInfo[i]=eval(habInfo[i])
                    except:
                        warnDlg = gui.Dlg(title="Warning!")
                        warnDlg.addText(
                            "Number expected, got text instead. \nPlease make sure maximum duration, minimum on-time, and maximum off-time are all numbers!")
                        warnDlg.show()
                        skip = True
                        self.addHabBlock(makeNew)
            if not skip:
                # Need to change text of hab button
                self.settings['playThrough']['Hab'] = 0 # This will always be the case
                x = self.buttonList['functions'].index(self.addHabBlock) # gets index
                self.buttonList['text'][x].text="Mod Hab Block" #Updates button text
                self.settings['maxDur']['Hab'] = habInfo[0]
                self.settings['maxOff']['Hab'] = habInfo[1]
                self.settings['minOn']['Hab'] = habInfo[2]
                self.settings['ISI']['Hab'] = habInfo[len(habInfo) - 3]
                if habInfo[len(habInfo) - 5] in [False,0,'False','0'] and 'Hab' in self.settings['autoAdvance']:
                    self.settings['autoAdvance'].remove('Hab')
                elif habInfo[len(habInfo) - 5] in [True, 1, 'True', '1'] and not 'Hab' in self.settings['autoAdvance']:
                    self.settings['autoAdvance'].append('Hab')

                if habInfo[len(habInfo) - 4] == 'None':
                    if 'Hab' in self.settings['playAttnGetter']:
                        del self.settings['playAttnGetter']['Hab']
                else:
                    if 'Hab' not in self.settings['playAttnGetter']:  # If it did not have an attngetter before.
                        agname = habInfo[len(habInfo) - 4]
                        self.settings['playAttnGetter']['Hab'] = agname
                    elif habInfo[len(habInfo) - 4] is not self.settings['playAttnGetter']['Hab']:
                        # If a different attention-getter has been selected
                        agname = habInfo[len(habInfo) - 4]
                        self.settings['playAttnGetter']['Hab'] = agname


                if makeNew:
                    i = len(self.trialTypesArray['labels'])  # Grab length before adding, conveniently the index we need for position info etc.
                    self.trialTypesArray['labels'].append('Hab')
                    tempObj = visual.Rect(self.win, width=self.typeWidthObj, height=self.typeHeightObj,
                                          fillColor=self.colorsArray[i], pos=self.typeLocs[i])
                    numChar = len('Hab')
                    if numChar <= 3:
                        numChar = 4  # Maximum height
                    tempTxt = visual.TextStim(self.win, alignHoriz='center', alignVert='center', bold=True,
                                              height=self.typeHeightObj / (.38 * numChar), text='Hab',
                                              pos=self.typeLocs[i])
                    self.trialTypesArray['shapes'].append(tempObj)
                    self.trialTypesArray['text'].append(tempTxt)
                    self.settings['trialTypes'].append('Hab')
                    self.settings['stimNames']['Hab'] = []
                #Check about movies.
                if len(habInfo) > 7:  # Again, if there were movies to list.
                    tempMovies = []  # This will just replace the stimNames list
                    for i in range(0, len(self.settings['stimNames']['Hab'])):
                        if habInfo[i + 3]:
                            tempMovies.append(self.settings['stimNames']['Hab'][i])
                    self.settings['stimNames']['Hab'] = tempMovies
                #Check if we need to make a set of sub-trials.
                if habInfo[-2] in [True, 1, 'True', '1'] and habInfo[-1] > 1:
                   self.setHabSubTrials(habInfo[-1])
                elif habInfo[-2] in [True, 1, 'True', '1'] and habInfo[-1] <= 1:
                    errDlg = gui.Dlg("No sub-block created")
                    errDlg.addText("Sub-block of 1 or 0 defaults to single hab trial. Hab trial saved.")
                    errDlg.show()
                elif len(self.settings['habTrialList']) > 0 and habInfo[-2] in [False,0,'False','0'] :
                    self.settings['habTrialList'] = [] #If the sub-block functionality was on and is now off

    def setHabSubTrials(self,numHab):
        """
        Groups trial types into hab blocks. Hab blocks can have multiple trial types, but one must always be Hab.
        This function doesn't care if you've made a hab block before, it just overwrites whatever exists. This is under
        the assumption that hab blocks will have few trials.

        :param numHab: Number of trials in a hab block
        :type numHab: int
        :return:
        :rtype:
        """
        subBlockDlg = gui.Dlg("Hab Sub-trials")
        subBlockDlg.addText("EXACTLY ONE must be set to Hab")
        for i in range(0, numHab):
            subBlockDlg.addField('sub-trial ' + str(i + 1), choices=self.settings['trialTypes'])
        subBlockInfo = subBlockDlg.show()
        if subBlockDlg.OK:
            if 'Hab' not in subBlockInfo:
                errDlg = gui.Dlg("Invalid sub-trials!")
                errDlg.addText("Sub-trials must include AT LEAST one Hab trial!")
                errDlg.show()
                self.setHabSubTrials(numHab)
            elif subBlockInfo.count('Hab') > 1:
                errDlg = gui.Dlg("Invalid sub-trials!")
                errDlg.addText("Sub-trials must include NO MORE THAN one Hab trial!")
                errDlg.show()
                self.setHabSubTrials(numHab)
            else:
                tempList = []
                for i in range(0, len(subBlockInfo)):
                    tempList.append(subBlockInfo[i])
                self.settings['habTrialList'] = tempList

    
    def delTrialTypeDlg(self):
        """
        Dialog for deleting a trial type, and all instances of that trial type in the study flow

        :return:
        :rtype:
        """
        self.showMainUI()
        self.workingRect.draw()
        self.workingText.draw()
        self.win.flip()
        delTypeDlg = gui.Dlg(title="Choose trial type to delete.")
        delTypeDlg.addText("Warning: Cannot be undone. All instances of this trial type in study flow will also be removed!")
        delTypeDlg.addField("Choose trial type to delete, then hit OK.", choices=self.trialTypesArray['labels'])
        delInfo=delTypeDlg.show()
        if delTypeDlg.OK:
            dType=delInfo[0]
            #Reassign colors if neccessary.
            if self.settings['trialTypes'].index(dType) < len(self.settings['trialTypes']):
                nowColor = self.colorsArray.pop(self.settings['trialTypes'].index(dType))
                self.colorsArray.insert(len(self.settings['trialTypes'])-1, nowColor)
            self.settings['trialTypes'].remove(dType)  # remove the type from the list of trial types.
            del self.settings['stimNames'][dType]  # remove from stimNames
            del self.settings['maxDur'][dType]  # remove from maxdur
            if dType in self.settings['playThrough']:  # if it was in playThrough, remove it from there too.
                self.settings['playThrough'].pop(dType, None)
            if dType in self.settings['habTrialList']: #If it was in a hab meta-trial.
                while dType in self.settings['habTrialList']:
                    self.settings['habTrialList'].remove(dType)
            self.trialTypesArray=self.loadTypes()  # easiest to just reload the trial types.
            # For the study flow, it's easiest just to remove it from the trial order and reload the study flow.
            if dType in self.settings['trialOrder']:
                while dType in self.settings['trialOrder']:
                    self.settings['trialOrder'].remove(dType)
            self.studyFlowArray=self.loadFlow() #To update colors if needed.

    def moveTrialInFlow(self,flowIndex):
        """
        A function for when a trial is clicked in the study flow, allowing you to either swap it or remove it.

        :param flowIndex: The index in the flowArray of the trial being modified
        :type flowIndex: int
        :return:
        :rtype:
        """
        #Display a text tooltip at the bottom of the flow area.
        instrText = visual.TextStim(self.win, text="Click another trial to swap positions, or click the remove button to delete from the study flow, click anywhere else to cancel.", bold=True,
                    height = abs(self.flowArea[3]-self.flowArea[2])*.05, pos=[-.5, self.flowArea[3]+.12*float(abs(self.flowArea[3]-self.flowArea[2]))],alignHoriz='center',alignVert='center')
        #highlight the selected object.
        removeTrialShape = visual.Rect(self.win, fillColor='red', width=.1*float(abs(self.flowArea[1]-self.flowArea[0])), height=.1*float(abs(self.flowArea[3]-self.flowArea[2])), 
                    pos=[self.flowArea[0]+float(abs(self.flowArea[1]-self.flowArea[0]))*.85,self.flowArea[3]+float(abs(self.flowArea[3]-self.flowArea[2]))/9])
        removeTrialText = visual.TextStim(self.win, text = "REMOVE", bold=True, height=removeTrialShape.height*.7,pos=removeTrialShape.pos)
        self.studyFlowArray['shapes'][flowIndex].lineColor="yellow"
        self.studyFlowArray['shapes'][flowIndex].lineWidth=5
        core.wait(.1) #Short delay to clear any old mouse press.
        #loop until mouse press
        while 1 not in self.mouse.getPressed():
            self.showMainUI()
            instrText.draw()
            removeTrialShape.draw()
            removeTrialText.draw()
            self.win.flip()
        for i in range(0, len(self.studyFlowArray['shapes'])):
            if self.mouse.isPressedIn(self.studyFlowArray['shapes'][i]) and not i == flowIndex: #clicked a different thing in the study flow
                #Swap the selected trial and the clicked trial, reload study flow.
                tempTrial=self.settings['trialOrder'][i]
                self.settings['trialOrder'][i] = self.settings['trialOrder'][flowIndex]
                self.settings['trialOrder'][flowIndex] = tempTrial
                self.studyFlowArray = self.loadFlow()
        if self.mouse.isPressedIn(removeTrialShape):
            #pop that trial out of trial order, reload flow.
            del self.settings['trialOrder'][flowIndex]
            self.studyFlowArray=self.loadFlow()
        else:
            self.studyFlowArray['shapes'][flowIndex].lineColor="white"
            self.studyFlowArray['shapes'][flowIndex].lineWidth=1.5
        core.wait(.1)

    
    def loadFlow(self):
        """
        This creates the array of objects in the study flow display

        :return:
        :rtype:
        """
        tOrd = self.settings['trialOrder']
        numItems = len(tOrd)
        tTypes = self.settings['trialTypes']
        for i in range(0,len(tOrd)):
            if tOrd[i] == 'Hab':
                numItems += 1 #Double-size for habs
        outputDict = {'lines':[],'shapes':[],'text':[],'labels':[]}  #Labels allows us to index the others while still keeping order.
        j = 0 # This serves a purpose, trust me. It's for rendering hab blocks.
        if len(tOrd) < 21:  # Past 20 we can't render it, but it won't crash.
            flowSpace = self.flowLocs
        else:
            flowSpace = self.overFlowLocs
        for i in range(0, len(tOrd)):
            #Now, actually build the list of objects to render.
            if i < 39 or (i == 39 and len(tOrd) == 40):
                c=tTypes.index(tOrd[i]) # find the trial type, get color index
                if tOrd[i] == 'Hab': # The special category
                    if j+1 % 10 == 0 and j < 40:
                        j += 1 # Just in case we're at the point where it would loop around to the second row. We don't want that.
                    lx1 = flowSpace[j][0]
                    j += 1
                    lx2 = flowSpace[j][0]
                    lx = (lx2+lx1)/2 # Ideally putting it square in between the two places.
                    loc = [lx,flowSpace[j][1]]
                    tempObj = visual.Rect(self.win,width=self.flowWidthObj*2, height=self.flowHeightObj, fillColor=self.colorsArray[c], pos=loc)
                elif tOrd[i] in self.settings['autoAdvance'] and j not in [0, 10, 20, 30]:
                    # Make it adjacent to the last one, unless it would start a row, in which case leave it.
                    loc = [flowSpace[j][0]-abs(self.flowArea[1]-self.flowArea[0])*((self.flowGap-self.flowWidMult)/2), flowSpace[j][1]]
                    tempObj = visual.Rect(self.win, width=abs(self.flowArea[1]-self.flowArea[0])*(self.flowWidMult + (self.flowGap-self.flowWidMult)), height=self.flowHeightObj, fillColor=self.colorsArray[c], pos=loc)
                else:
                    tempObj = visual.Rect(self.win, width=self.flowWidthObj, height=self.flowHeightObj, fillColor=self.colorsArray[c], pos=flowSpace[j])
                numChar = len(tOrd[i])
                if numChar <= 3:
                    numChar = 4 #Maximum height
                tempTxt = visual.TextStim(self.win, alignHoriz='center', bold=True, alignVert='center',height=self.flowHeightObj/(.48*numChar), text=tOrd[i], pos=tempObj.pos)
                j += 1
                outputDict['shapes'].append(tempObj)
                outputDict['text'].append(tempTxt)
                outputDict['labels'].append(tOrd[i])
        if numItems == 0:
            pass #So we do not add a line if there is no line to draw!
        elif numItems < 11:
            tempLine = visual.Line(self.win, start=self.flowLocs[0], end=outputDict['shapes'][-1].pos)
            outputDict['lines'].append(tempLine)
        elif numItems < 21:
            tempLine = visual.Line(self.win, start=self.flowLocs[0], end=self.flowLocs[9])
            tempLine2 = visual.Line(self.win, start=self.flowLocs[10], end=outputDict['shapes'][-1].pos)
            outputDict['lines'].append(tempLine)
            outputDict['lines'].append(tempLine2)
        elif numItems < 31:
            tempLine = visual.Line(self.win, start=self.overFlowLocs[0], end=self.overFlowLocs[9])
            tempLine2 = visual.Line(self.win, start=self.overFlowLocs[10], end=self.overFlowLocs[19])
            tempLine3 = visual.Line(self.win, start=self.overFlowLocs[20], end=outputDict['shapes'][-1].pos)
            outputDict['lines'].append(tempLine)
            outputDict['lines'].append(tempLine2)
            outputDict['lines'].append(tempLine3)
        elif numItems < 41:
            tempLine = visual.Line(self.win, start=self.overFlowLocs[0], end=self.overFlowLocs[9])
            tempLine2 = visual.Line(self.win, start=self.overFlowLocs[10], end=self.overFlowLocs[19])
            tempLine3 = visual.Line(self.win, start=self.overFlowLocs[20], end=self.overFlowLocs[29])
            tempLine4 = visual.Line(self.win, start=self.overFlowLocs[30], end=outputDict['shapes'][-1].pos)
            outputDict['lines'].append(tempLine)
            outputDict['lines'].append(tempLine2)
            outputDict['lines'].append(tempLine3)
            outputDict['lines'].append(tempLine4)
        else:
            tempLine = visual.Line(self.win, start=self.overFlowLocs[0], end=self.overFlowLocs[9])
            tempLine2 = visual.Line(self.win, start=self.overFlowLocs[10], end=self.overFlowLocs[19])
            tempLine3 = visual.Line(self.win, start=self.overFlowLocs[20], end=self.overFlowLocs[29])
            tempLine4 = visual.Line(self.win, start=self.overFlowLocs[30], end=outputDict['shapes'][38].pos)
            outputDict['lines'].append(tempLine)
            outputDict['lines'].append(tempLine2)
            outputDict['lines'].append(tempLine3)
            outputDict['lines'].append(tempLine4)
            tempText = visual.TextStim(self.win, text="+" + str(numItems-39) + " more", pos=self.overFlowLocs[39])
            outputDict['lines'].append(tempText)
        return outputDict
    
    def loadTypes(self): #A "pallette" of trial types on one side, only needed when loading from save.
        """
        This function creates the trial types palette

        :return:
        :rtype:
        """
        if len(self.settings['trialTypes']) == 0:#if working w/ an old settings file or one that for w/e reason has no ttypes.
            tOrd = self.settings['trialOrder']
            tTypes = []#list of trial types
            for i in range(0,len(tOrd)):
                if tOrd[i] not in tTypes:
                    tTypes.append(tOrd[i]) #creating a  list of unique trial types
        else:
            tTypes = self.settings['trialTypes'] 
        outputDict=  {'shapes':[],'text':[],'labels':[]} #Dicts ain't ordered but lists within dicts sure are!
        #Create the same trial type squares we see in the flow, but wholly independent objects for drawing purposes (allowing one to change w/out the other)
        for i in range(0, len(tTypes)):
            #Now, actually build the list of objects to render.
            tempObj = visual.Rect(self.win,width=self.typeWidthObj, height=self.typeHeightObj, fillColor=self.colorsArray[i], pos=self.typeLocs[i])
            numChar = len(tTypes[i])
            if numChar <= 3:
                numChar = 4 #Maximum height
            tempTxt = visual.TextStim(self.win, alignHoriz='center', alignVert='center',bold=True,height=self.typeHeightObj/(.32*numChar),text=tTypes[i], pos=self.typeLocs[i])
            outputDict['shapes'].append(tempObj) #Might need to change this to be a dict of shapes and text, to make click-in-shape easier to manage later.
            outputDict['text'].append(tempTxt)
            outputDict['labels'].append(tTypes[i])
        return(outputDict)
    
    def quitFunc(self):
        """
        Simple function for quitting, checks if you want to save first (if there's anything to save).

        :return:
        :rtype:
        """
        self.allDone=True
        if len(self.trialTypesArray['labels']) > 0: #Don't save if no trial types have been created
            saveDlg = gui.Dlg(title="Save?",labelButtonOK='Yes', labelButtonCancel='No')
            saveDlg.addText("Save before quitting?")
            s = saveDlg.show()
            if saveDlg.OK:
                if len(self.folderPath) == 0:
                    self.saveDlg()
                else:
                    self.saveEverything()
            if not self.loadSave and len(self.folderPath)>0: #If this is the first time saving a new experiment, relaunch from launcher!
                self.win.close()
                launcherPath = self.folderPath+self.settings['prefix']+'Launcher.py'
                launcher = coder.ScriptThread(target=self._runLauncher(launcherPath), gui=self)
                launcher.start()
    
    def univSettingsDlg(self): #The universal settings button.
        """
        Settings that apply to every PyHab study regardless of anything else.

        0 = prefix: The prefix of the launcher and all data files.

        1 = blindPres: Level of experimenter blinding, 0 (none), 1 (no trial type info), or
            2 (only info is whether a trial is currently active.

        2 = prefLook: Whether the study is preferential-looking or single-target.

        3 = nextFlash: Whether to have the coder window flash to alert the experimenter they need to manually trigger
            the next trial

        :return:
        :rtype:
        """
        self.showMainUI()
        self.workingRect.draw()
        self.workingText.draw()
        self.win.flip()
        uDlg = gui.Dlg(title="Universal settings")
        # [default] blindpres, autoadvance, ISI,
        uDlg.addField("Experiment name", self.settings['prefix'])
        ch = []
        if self.settings['blindPres'] == '1' or self.settings['blindPres'] == 1:
            ch=['do not display next trial type','none','only show trial active/inactive']
        elif self.settings['blindPres'] == '2' or self.settings['blindPres'] == 2:
            ch=['show trial active/not active only','none','do not display next trial type']
        else:
            ch=['none','do not display next trial type','show trial active/not active only']
        uDlg.addField("Experimenter blinding:", choices=ch)
        ch2 = []
        if self.settings['prefLook'] in ['1',1]:# so it does not reset everytime you load this dialog.
            ch2 = ["Preferential looking","Single-target"]
        else:
            ch2=["Single-target", "Preferential looking"]
        uDlg.addField("Single-target or preferential looking?",choices=ch2)
        if self.settings['nextFlash'] in ['1',1,'True',True]:
            ch3 = ["Yes","No"]
        else:
            ch3= ["No","Yes"]
        uDlg.addField("Flash to alert experimenter to manually start next trial?", choices=ch3)
        uInfo = uDlg.show()
        if uDlg.OK:
            tryAgain = False
            self.settings['prefix'] = uInfo[0]
            if uInfo[1] == 'none':
                self.settings['blindPres'] = 0
            elif uInfo[1] == 'do not display next trial type':
                self.settings['blindPres'] = 1
            else:
                self.settings['blindPres'] = 2
            if uInfo[2] == "Preferential looking" and self.settings['prefLook'] in [0,'0','False',False]: #specifically switching, reactivate all data cols.
                self.settings['prefLook'] = 1
                self.settings['dataColumns'] = self.allDataColumnsPL
            elif uInfo[2] == "Single-target" and self.settings['prefLook'] in [1,'1','True',True]:
                self.settings['prefLook'] = 0
                self.settings['dataColumns'] = self.allDataColumns
            if uInfo[3] == "Yes":
                self.settings['nextFlash'] = 1
            else:
                self.settings['nextFlash'] = 0
            if tryAgain:
                self.univSettingsDlg()
        
    def dataSettingsDlg(self):
        """
        Which columns of data are recorded.
        Resets if the experiment type is switched to or from preferential looking.

        :return:
        :rtype:
        """
        self.showMainUI()
        self.workingRect.draw()
        self.workingText.draw()
        self.win.flip()
        dDlg = gui.Dlg(title="Data settings")
        dDlg.addText("Check all columns you would like to be recorded in your data files. ")
        dDlg.addText("ANYTHING UNCHECKED WILL NOT BE STORED IN ANY WAY!")
        if self.settings['prefLook'] in [1,'1',True,'True']:
            tempDataCols = self.allDataColumnsPL
        else:
            tempDataCols = self.allDataColumns
        for i in range(0, len(tempDataCols)):
            if tempDataCols[i] in self.settings['dataColumns']:
                dDlg.addField(tempDataCols[i],initial=True)
            else:
                dDlg.addField(tempDataCols[i],initial=False)
        datInfo = dDlg.show()
        if dDlg.OK:
            tempCols = []
            for j in range(0, len(datInfo)):
                if datInfo[j]:
                    tempCols.append(tempDataCols[j])
            self.settings['dataColumns'] = tempCols
        
    def stimSettingsDlg(self, lastSet=[], redo=False):
        """

        Settings relating to stimulus presentation. Indexes from the dialog

        0 = screenWidth: Width of stim window

        1 = screenHeight: Height of stim window

        2 = Background color of stim window

        3 = movieWidth: Width of movieStim3 object inside stim window. Future: Allows for movie default resolution?

        4 = movieWidth: Height of movieStim3 object inside stim window

        5 = screenIndex: Which screen to display the stim window on.

        6 = freezeFrame: If the attention-getter is used (for this trial type), this is the minimum time the first frame
        of the movie will be displayed after the attention-getter finishes.

        :param lastSet: Optional. Last entered settings, in case dialog needs to be presented again to fix bad entries.
        :type lastSet: list
        :param redo: Are we doing this again to fix bad entries?
        :type redo: boolean
        :return:
        :rtype:
        """

        self.showMainUI()
        self.workingRect.draw()
        self.workingText.draw()
        self.win.flip()

        sDlg = gui.Dlg(title="Stimulus presentation settings")
        if not redo:
            lastSet=[]
            # This order maps on to the order of the dialog box. This allows for carrying over from previous entries
            # if there's a problem (e.g., text where numbers should be). Done as separate lines basically for ease of
            # correspondence to the field definitions and the saving at the end.
            lastSet.append(self.settings['screenWidth'])
            lastSet.append(self.settings['screenHeight'])
            lastSet.append(self.settings['screenColor'])
            lastSet.append(self.settings['movieWidth'])
            lastSet.append(self.settings['movieHeight'])
            lastSet.append(self.settings['screenIndex'])
            lastSet.append(self.settings['freezeFrame'])
        colors = ['black', 'white', 'gray']
        colorchz = [x for x in colors if x != lastSet[2]]
        colorchz.insert(0, lastSet[2])
        sDlg.addField("Stimulus display width in pixels", lastSet[0])
        sDlg.addField("Stimulus display height in pixels", lastSet[1])
        sDlg.addField("Stimulus display background color", choices=colorchz)
        sDlg.addField("Width of movie stimuli in pixels", lastSet[3])
        sDlg.addField("Height of movie stimuli in pixels", lastSet[4])
        if lastSet[5] in [1, '1']:
            scrchz = [1, 0]
        else:
            scrchz = [0, 1]
        sDlg.addField("Screen index of presentation screen (0 = primary display, 1 = secondary screen)", choices=scrchz)
        sDlg.addField("Freeze first frame for how many seconds after attention-getter?", lastSet[6])

        stimfo = sDlg.show()
        if sDlg.OK:
            problem = False
            for i in [0, 1, 3, 4, 5, 6]:
                if not isinstance(stimfo[i], float) and not isinstance(stimfo[i], int):
                    try:
                        stimfo[i] = eval(stimfo[i])
                    except:
                        problem = True
            if not problem:
                self.settings['screenWidth'] = stimfo[0]
                self.settings['screenHeight'] = stimfo[1]
                self.settings['screenColor'] = stimfo[2]
                self.settings['movieWidth'] = stimfo[3]
                self.settings['movieHeight'] = stimfo[4]
                self.settings['screenIndex'] = stimfo[5]
                self.settings['freezeFrame'] = stimfo[6]
            else:
                warnDlg = gui.Dlg(title="Warning!")
                warnDlg.addText(
                    "Number expected, got text instead. \nPlease make sure window height/width, movie height/width, and freeze-frame duration are all numbers!")
                warnDlg.show()
                self.stimSettingsDlg(stimfo, redo=True)



    def addStimToLibraryDlg(self):
        """
        A series of dialog boxes which allows you to build a "library" of stimulus files for your experiment, which you
        can then assign to trial types in a separate dialog.

        Works a bit like the attention-getter construction dialogs, but different in that it allows audio or images alone.
        The image/audio pairs are complicated, but not worth splitting into their own function at this time.

        :return:
        :rtype:
        """
        add = 1
        if len(self.settings['stimList'].keys())>0:
            sDlg0 = gui.Dlg(title="Add/remove stimuli")
            sDlg0.addField("Add or remove stimuli?", choices=['Add','Remove'])
            sd0 = sDlg0.show()
            if sDlg0.OK and sd0[0] == 'Remove':
                add = -1
            elif sDlg0.OK:
                add = 1
            else:
                add = 0
        if add == 1:
            cz = ['Movie', 'Image', 'Audio', 'Image with audio']
            sDlg1 = gui.Dlg(title="Add stimuli to library, step 1")
            sDlg1.addField("What kind of stimuli would you like to add? (Please add each type separately)", choices=cz)
            sDlg1.addField("How many? (For image with audio, how many pairs?) You will select them one at a time.", 1)
            sd1 = sDlg1.show()
            allowedStrings = {'Audio': "Audio (*.aac, *.aiff, *.flac, *.m4a, *.mp3, *.ogg, *.raw, *.wav, *.m4b, *.m4p)",
                              'Movie': "Movies (*.mov, *.avi, *.ogv, *.mkv, *.mp4, *.mpeg, *.mpe, *.mpg, *.dv, *.wmv, *.3gp)",
                              'Image': "Images (*.jpg, *.jpeg, *.png, *.gif, *.bmp, *.tif, *.tiff)"}
            if sDlg1.OK and isinstance(sd1[1],int):
                stType = sd1[0]  # Type of stimuli (from drop-down).
                stNum = sd1[1]  # Number to add.
                NoneType = type(None)
                if stType != 'Image with audio':  # Image w/ audio is complicated, so we will take care of that separately.
                    for i in range(0, stNum):
                        stimDlg = gui.fileOpenDlg(prompt="Select stimulus file (only one!)")
                        if type(stimDlg) is not NoneType:
                            fileName = os.path.split(stimDlg[0])[1] # Gets the file name in isolation.
                            self.stimSource[fileName] = stimDlg[0]  # Creates a "Find this file" path for the save function.
                            self.settings['stimList'][fileName] = {'stimType': stType, 'stimLoc': stimDlg[0]}
                else:  # Creating image/audio pairs is more complicated.
                    for i in range(0, stNum):
                        stimDlg1 = gui.Dlg(title="Pair number " + str(i+1))
                        stimDlg1.addField("Unique name for this stimulus pair (you will use this to add it to trials later)", 'pairName')
                        stimDlg1.addText("Click OK to select the AUDIO file for this pair")
                        sd2 = stimDlg1.show()
                        if stimDlg1.OK:
                            a = True
                            if sd2[0] in list(self.settings['stimList'].keys()):  # If the pair name exists already
                                a = False
                                errDlg = gui.Dlg("Change existing pair?")
                                errDlg.addText("Warning: Pair name already in use! Press OK to overwrite existing pair, or cancel to skip to next pair.")
                                ea = errDlg.show()
                                if errDlg.OK:
                                    a = True
                            if a:
                                stimDlg = gui.fileOpenDlg(prompt="Select AUDIO file (only one!)")
                                if type(stimDlg) is not NoneType:
                                    fileName = os.path.split(stimDlg[0])[1] # Gets the file name in isolation.
                                    self.stimSource[fileName] = stimDlg[0]  # Creates a "Find this file" path for the save function.
                                    self.settings['stimList'][sd2[0]] = {'stimType': stType, 'audioLoc': stimDlg[0]}
                                    tempDlg = gui.Dlg(title="Now select image")
                                    tempDlg.addText("Now, select the IMAGE file for this pair (cancel to erase audio and skip pair)")
                                    t = tempDlg.show()
                                    if tempDlg.OK:
                                        stimDlg2 = gui.fileOpenDlg(prompt="Select IMAGE file (only one!)")
                                        if type(stimDlg2) is not NoneType:
                                            fileName2 = os.path.split(stimDlg2[0])[1] # Gets the file name in isolation.
                                            self.stimSource[fileName2] = stimDlg2[0]
                                            self.settings['stimList'][sd2[0]].update({'imageLoc': stimDlg2[0]})
                                    else:
                                        del self.settings['stimList'][sd2[0]]

            elif sDlg1.OK:
                errDlg = gui.Dlg(title="Warning, invalid value!")
                errDlg.addText("Number of files to add was not a whole number! Please try again.")
                e = errDlg.show()
            # When we are done with this dialog, if we have actually added anything, create the "add to types" dlg.
            if len(list(self.settings['stimList'].keys())) > 0 and self.addStimToTypesDlg not in self.buttonList['functions']:
                addMovButton = visual.Rect(self.win, width=.3, height=.5 * (.2 / self.aspect), pos=[.75, -.6],
                                           fillColor="white")
                addMovText = visual.TextStim(self.win, text="Add stimulus files \nto trial types", color="black",
                                             height=addMovButton.height * .3, alignHoriz='center', pos=addMovButton.pos)
                self.buttonList['shapes'].append(addMovButton)
                self.buttonList['text'].append(addMovText)
                self.buttonList['functions'].append(self.addStimToTypesDlg)

        elif add == -1: # Remove stimuli from library
            self.removeStimFromLibrary()

    def removeStimFromLibrary(self):
        """
        Presents a dialog listing every item of stimuli in the study library. Allows you to remove any number at once,
        removes from all trial types at same time. Deletes from stimuli folder on save if extant.


        :return:
        :rtype:
        """

        #Use self.folderpath to see if there's an extant save that needs to be updated.

        remDlg = gui.Dlg("Remove stimuli from library")
        orderList = []
        remDlg.addText("Uncheck any stimuli you want to remove. NOTE: This will remove stimuli from trial types as well.")
        for i in self.settings['stimList'].keys():
            remDlg.addField(i, initial=True)
            orderList.append(i) # Neccessary because dicts are un-ordered.

        remList=remDlg.show()

        if remDlg.OK:
            for j in range(0,len(remList)):
                if not remList[j]:
                    toRemove = orderList[j]
                    #Things to remove it from: stimlist, stimsource, stimNames(if assigned to trial types). Doesn't apply to attngetter, has its own system.
                    if self.settings['stimList'][toRemove]['stimType'] != 'Image with audio':
                        self.delList.append(toRemove)
                        if toRemove in self.stimSource.keys():
                            try:
                                del self.stimSource[toRemove]
                            except:
                                print("Could not remove from stimSource!")
                    else:
                        #If it's an image/audio pair, need to append both files.
                        tempAname = os.path.split(self.settings['stimList'][toRemove]['audioLoc'])[1]
                        tempIname = os.path.split(self.settings['stimList'][toRemove]['imageLoc'])[1]
                        self.delList.append(tempAname)
                        self.delList.append(tempIname)
                        if tempAname in self.stimSource.keys():
                            try:
                                del self.stimSource[tempAname]
                                del self.stimSource[tempIname]
                            except:
                                print("Could not remove from stimSource!")

                    try:
                        del self.settings['stimList'][toRemove]
                    except:
                        print("Could not remove from stimList!")
                    for q in self.settings['stimNames'].keys(): # Remove from trial types it has been assigned to.
                        self.settings['stimNames'][q] = [x for x in self.settings['stimNames'][q] if x != toRemove]





    def addStimToTypesDlg(self):
        """
        A series dialog boxes, the first selecting a trial type and the number of stimuli to add to it,
        a second allowing you to add stimuli from the stimulus library that is stimList in the settings.
        Also used for adding beginning and end of experiment images (?)

        :return:
        :rtype:
        """
        self.showMainUI()
        self.workingRect.draw()
        self.workingText.draw()
        self.win.flip()

        if len(self.trialTypesArray['labels']) > 0:
            d1 = gui.Dlg(title="Select trial type to add stimuli to")
            choiceList=[]
            for i in range(0, len(self.trialTypesArray['labels'])): # Not just copying list b/c it would add start/end to it
                choiceList.append(self.trialTypesArray['labels'][i])
            choiceList.append('Start and end of experiment screens')
            d1.addField("Trial type to add stimulus file to", choices=choiceList)
            d1.addField("Number of stimuli to add (you will select them in the next window)",1)
            d1.addText("Note: You can only select stimuli you have already added to the experiment library")
            d1.addText("Note: You can only REMOVE stimuli from a trial type in the trial type's own settings, this will add to whatever is already there")
            d = d1.show()
            if d1.OK and isinstance(d[1], int):
                self.showMainUI()
                self.workingRect.draw()
                self.workingText.draw()
                self.win.flip()
                tType = d[0]
                numAdd = d[1]
                if tType != 'Start and end of experiment screens':
                    d2 = gui.Dlg(title="Select stimuli")
                    stimKeyList = list(self.settings['stimList'].keys())
                    stimKeyList.sort()
                    for i in range(0, numAdd):
                        d2.addField("Stimulus no. " + str(i+1), choices=stimKeyList)
                    newList = d2.show()
                    if d2.OK:
                        for z in range(0, len(newList)):
                            self.settings['stimNames'][tType].append(newList[z])
                else:
                    d2 = gui.Dlg(title="Select image")
                    # Create a list of just image stim
                    startimgs = [x for x in list(self.settings['stimList'].keys()) if self.settings['stimList'][x]['stimType'] == 'Image']
                    startimgs.sort()
                    startimgs.insert(0,'None')
                    if self.settings['startImage'] != '':
                        startimgs = [x for x in startimgs if x != self.settings['startImage']]
                        startimgs.insert(0,self.settings['startImage'])
                    endimgs = [x for x in list(self.settings['stimList'].keys()) if self.settings['stimList'][x]['stimType'] == 'Image']
                    endimgs.sort()
                    endimgs.insert(0,'None')
                    if self.settings['endImage'] != '':
                        endimgs = [x for x in endimgs if x != self.settings['endImage']]
                        endimgs.insert(0, self.settings['endImage'])
                    d2.addField("Start of experiment image", choices=startimgs)
                    d2.addField("End of experiment image", choices=endimgs)

                    newList = d2.show()
                    if d2.OK:
                        if newList[0] is not 'None':
                            self.settings['startImage'] = newList[0]
                        else:
                            self.settings['startImage'] = ''
                        if newList[1] is not 'None':
                            self.settings['endImage'] = newList[1]
                        else:
                            self.settings['endImage'] = ''
            elif d1.OK:
                errDlg = gui.Dlg(title="Warning, invalid value!")
                errDlg.addText("Number of stimuli to add was not a whole number! Please try again.")
                e = errDlg.show()
        else:
            errDlg = gui.Dlg(title="No trial types!")
            errDlg.addText("No trial types to add movies to! Please create trial types first.")
            e = errDlg.show()


    def attnGetterAudioDlg(self):
        """
        A modular dialog for setting the options for an audio-based attention-getter

        :return: A dictionary containing all the info required for an audio attention-getter
        :rtype: dict
        """
        NoneType = type(None)
        aDlg3 = gui.Dlg(title="Audio attention-getter settings")
        aDlg3.addField("Looming shape type", choices=['Rectangle', 'Cross', 'Star'])
        aDlg3.addField("Looming shape color", choices=['yellow', 'green', 'red', 'blue', 'white', 'black'])
        ans3 = aDlg3.show()
        if aDlg3.OK:
            shape = ans3[0].split()  # Pulls out rectangle, cross, or star.
            fileSelectDlg = gui.fileOpenDlg(prompt="Select attention-getter audio file")
            if type(fileSelectDlg) is not NoneType:
                path, namething = os.path.split(fileSelectDlg[0])
                # Again an ugly but necessary solution for getting the duration.
                tempSound = sound.Sound(fileSelectDlg[0])
                tempGetter = {'stimLoc': fileSelectDlg[0], 'stimName': namething,
                                   'shape': shape[-1], 'color': ans3[1],
                                   'stimDur': tempSound.getDuration()}
                del tempSound
                return tempGetter
            else:
                return {}
        else:
            return {}

    def attnGetterVideoDlg(self):
        """
        A modular dialog for setting the options for a video-based attention-getter

        :return: A dictionary containing all the info required for a video attention-getter
        :rtype: dict
        """
        NoneType = type(None)
        fileSelectDlg = gui.fileOpenDlg(prompt="Select attention-getter movie file")
        if type(fileSelectDlg) is not NoneType:
            path, namething = os.path.split(fileSelectDlg[0])
            # Suboptimal solution for getting duration, but possibly only available.
            tempMovie = visual.MovieStim3(self.win, fileSelectDlg[0])
            tempGetter = {'stimLoc': fileSelectDlg[0], 'stimName': namething,
                               'stimDur': tempMovie.duration}
            del tempMovie
            return tempGetter
        else:
            return {}

    def attnGetterDlg(self):
        """
        The dialog window for customizing the attention-getters available to use for different trials.
        Two-stage: Modify existing attngetter or make new, then what do you do with ether of those.
        Allows audio with PsychoPy-produced looming shape or just a video file.

        :return:
        :rtype:
        """
        self.showMainUI()
        self.workingRect.draw()
        self.workingText.draw()
        self.win.flip()
        achz = list(self.settings['attnGetterList'].keys())
        # Remove default (which cannot be messed with).
        achz = [a for a in achz if a is not 'PyHabDefault']
        achz.insert(0, 'Make New')  # Top item will always be "make new"
        aDlg1 = gui.Dlg(title="Select attention-getter or make new")
        aDlg1.addField("Select attention-getter or 'Make New'", choices=achz)
        ans1 = aDlg1.show()
        NoneType = type(None)
        if aDlg1.OK:
            if ans1[0] is 'Make New':
                # New window to design an attention getter! Choose your own adventure a bit.
                aDlg2 = gui.Dlg(title="Make new attention-getter: step 1")
                aDlg2.addField("Attention-getter name: ", 'NewAttnGetter')
                aDlg2.addField("Audio (with built-in shape) or video?", choices=['Audio','Video'])
                ans2 = aDlg2.show()
                if aDlg2.OK:
                    tempGetter={'stimType': ans2[1]}
                    if tempGetter['stimType'] is 'Video':
                        newTempGet = self.attnGetterVideoDlg()
                    else:
                        newTempGet = self.attnGetterAudioDlg()
                    if len(newTempGet) > 0:
                        tempGetter.update(newTempGet)
                        self.settings['attnGetterList'][ans2[0]] = tempGetter

            else: # Modifying an existing AG. Little complex.
                aDlg2b = gui.Dlg(title="Change attention-getter properties")
                currAG = self.settings['attnGetterList'][ans1[0]] #The current attention-getter.
                aDlg2b.addField("Attention-getter name: ", ans1[0])
                if currAG['stimType'] is 'Audio':
                    chz = ['Audio', 'Video']
                else:
                    chz = ['Video, Audio']
                aDlg2b.addField("Attention-getter type: ", choices=chz)
                aDlg2b.addField("Change current file (%s)?" % currAG['stimName'], choices=["No","Yes"])
                if currAG['stimType'] is 'Audio':
                    allShapes = ['Rectangle','Cross','Star']
                    shapeChz = [x for x in allShapes if x is not currAG['shape']]
                    shapeChz.insert(0, currAG['shape'])
                    aDlg2b.addField("Looming shape type", choices=shapeChz)
                    allColors = ['yellow', 'green', 'red', 'blue', 'white', 'black']
                    colorChz = [x for x in allColors if x is not currAG['color']]
                    colorChz.insert(0, currAG['color']) # A more elegant shuffle because the words match
                    aDlg2b.addField("Looming shape color", choices=colorChz)
                ans2b = aDlg2b.show
                if aDlg2b.OK:
                    if ans2b[0] is not ans1[0]:  # Did they change the name?
                        self.settings['attnGetterList'][ans2b[0]] = self.settings['attnGetterList'].pop(ans1[0])
                        currAG = self.settings['attnGetterList'][ans2b[0]]
                    if ans2b[1] is not currAG['stimType']:  # if they change it from audio to video or the reverse...
                        tempGetter = {'stimType': ans2b[1]}
                        # 1. If going to audio, select shape then new file.
                        if currAG['stimType'] is 'Video':
                            newTempGet = self.attnGetterAudioDlg()
                        else:
                            newTempGet = self.attnGetterVideoDlg()
                        if len(newTempGet) > 0:
                            tempGetter.update(newTempGet)
                            self.settings['attnGetterList'][ans2b[0]] = tempGetter # Overwrite existing.
                    elif ans2b[2] is "Yes":  # Same stim type, change file. Ignore shape settings for now
                        fileSelectDlg = gui.fileOpenDlg(prompt="Select attention-getter file")
                        if type(fileSelectDlg) is not NoneType:
                            path, namething = os.path.split(fileSelectDlg[0])
                            if ans2b[1] is 'Video':
                                tempStim = visual.MovieStim3(self.win, fileSelectDlg[0])
                            else:
                                tempStim = sound.Sound(fileSelectDlg[0])
                            self.settings['attnGetterList'][ans2b[0]].update({'stimLoc': fileSelectDlg[0],
                                                                              'stimName': namething,
                                                                              'stimDur': tempStim.duration})
                            del tempStim
                    if len(ans2b) > 2:  # If we had shape/color settings
                        self.settings['attnGetterList'][ans2b[0]].update({'shape': ans2b[3], 'color': ans2b[4]})

    def condSettingsDlg(self): #Settings relating to conditions and randomization
        """
        The dialog window for "condition settings", not to be confused with the
        condition interface created by self.condMaker(). This determines whether
        condition randomization is used at all, a separate interface is used to
        define the conditions themselves.

        :return:
        :rtype:
        """
        cDlg = gui.Dlg(title="Conditions settings")
        # Welcome to waterloo. The simple version is just "make a list of conditions and set them after saving".
        # The ideal is that it reads off stimNames (or # movies) and all and lets you specify for each order...
        chkBox = False
        if self.settings['randPres'] in [1,'1',True,'True']:
            chkBox = True
        cDlg.addField("Use random presentation? If yes, a new interface will open",initial=chkBox)
        cDlg.addField("Pre-existing condition file (optional, leave blank to make new file called conditions.csv)", self.settings['condFile'])
        condInfo = cDlg.show()
        if cDlg.OK:
            self.settings['randPres'] = condInfo[0]
            if condInfo[0]:
                # A new interface that allows you to re-order things
                # Check if there are movies to re-order.
                allReady = True
                if len(self.trialTypesArray['labels']) == 0: # If there are no trial types
                    allReady = False
                for i in range(0,len(self.trialTypesArray['labels'])):
                    if self.trialTypesArray['labels'][i] not in self.settings['stimNames'].keys(): # If a trial type has no movies associated with it
                        allReady = False
                    elif len(self.settings['stimNames'][self.trialTypesArray['labels'][i]]) == 0: # Another way that it can have no movies associated with it.
                        allReady = False
                if allReady:
                    if len(condInfo[1]) > 0:
                        self.settings['condFile'] = condInfo[1]
                    else:
                        self.settings['condFile'] = "conditions.csv"
                    if os.name is not 'posix':
                        self.win.winHandle.set_visible(visible=True)
                    self.condMaker()
                else:
                    #Create err dlg.
                    errDlg = gui.Dlg(title="No stimuli!")
                    errDlg.addText("Not all trial types have stimuli!")
                    errDlg.addText("Please add stimuli to all trial types first and then set conditions for randomized presentation.")
                    errDlg.addField("For studies without stimuli, enter list of arbitrary condition labels here, each one in quotes, separated by commas, all inside the square brackets",self.settings['condList'])
                    # Make this less shit
                    e = errDlg.show()
                    if errDlg.OK:
                        self.settings['condList'] = e[0]
            else:
                self.settings['condFile'] = ''  # Erases one if it existed.
                self.settings['condList'] = []  # Gets rid of existing condition list to save trouble.
                self.condDict = {}  # Gets rid of conditions altogether.

    def condMaker(self, rep=False, currPage=1):
        """
        A whole separate interface for managing condition creation.

        Outputs settings condList (labels of each condition), condFile (save conditions to this file)
        and makes new structure condDict (mapping of each label to actual condition it applies to)

        :param rep: Basically whether we are recursing while editing conditions
        :type rep: bool
        :return:
        :rtype:
        """
        condHeader = visual.TextStim(self.win, text="Conditions",height=.1, bold=True,pos=[-.83,.9])
        divLinesV = [] # Vertical dividing lines (actually very thin rects)
        divLinesH = [] # Horizontal dividing lines
        tTypeHeaders = [] # Heads of each column for trial types.
        condLabels = [] # Labels
        condContent = [] # The content of each condition (a list of dicts).
        drawConds = [] # The text things for drawing.
        numPages = 1
        if os.path.exists(self.settings['condFile']) and not rep: #If we already have a pre-existing cond file and aren't in the process of looping.
            testReader=csv.reader(open(self.settings['condFile'],'rU'))
            testStuff=[]
            for row in testReader:
                testStuff.append(row)
            for i in range(0, len(testStuff)):
                condLabels.append(testStuff[i][0])
                condContent.append(eval(testStuff[i][1]))
            testDict = dict(testStuff) 
            for i in testDict.keys():
                testDict[i]=eval(testDict[i])
            self.condDict=testDict 
            self.settings['condList'] = condLabels
            numPages = ceil(float(len(self.settings['condList']))/4) #use math.ceil to round up.
            
        elif len(self.condDict) > 0: #If we already have something to build on
            numPages = ceil(float(len(self.condDict.keys()))/4) #use math.ceil to round up.
            for i in range(0, len(self.settings['condList'])):
                if self.settings['condList'][i] in self.condDict.keys():
                    condContent.append(self.condDict[self.settings['condList'][i]])
                else:
                    condContent.append({ })
        doneButton = visual.Rect(self.win, width=.25, height=.67*(.15/self.aspect),fillColor="green",pos=[.82,-.85])
        doneText = visual.TextStim(self.win, text="DONE", bold=True, pos=doneButton.pos)
        addCondButton = visual.Rect(self.win, width=.3, height=.67*(.15/self.aspect),fillColor="blue",pos=[-.82,-.85])
        addCondText = visual.TextStim(self.win, text="Add condition", bold=True, height=addCondButton.height*.3, pos=addCondButton.pos)
        deleteCondButton = visual.Rect(self.win, width=.3, height=.67*(.15/self.aspect),fillColor="red",pos=[-.5,-.85])
        deleteCondText = visual.TextStim(self.win, text="Delete condition", bold=True, height=deleteCondButton.height*.3, pos=deleteCondButton.pos)
        randomCondsButton = visual.Rect(self.win, width=.4, height=.67*(.15/self.aspect),fillColor="purple",pos=[-.13,-.85])
        randomCondsText = visual.TextStim(self.win, text="Randomize over subjects", bold=True, height=randomCondsButton.height*.3, pos=randomCondsButton.pos)
        instrText = visual.TextStim(self.win, text="Page: 1/"+str(numPages), height=.1, pos=[.4,-.9])
        downArrowVerts = [(0.05,0.3),(-.05,0.3),(-0.05,0.15),(-0.1,0.15),(0,0),(0.1,0.15),(0.05,0.15)]
        nextPageArrow = visual.ShapeStim(self.win, vertices=downArrowVerts, size=.5, lineColor='white', fillColor='white', pos=[.6,-.95])
        upArrowVerts = [(0.05,-0.3),(-.05,-0.3),(-0.05,-0.15),(-0.1,-0.15),(0,0),(0.1,-0.15),(0.05,-0.15)]
        lastPageArrow = visual.ShapeStim(self.win, vertices=upArrowVerts, size=.5, lineColor='white', fillColor='white', pos=[.2,-.8])

        intervalHoriz = 1.5/(len(self.trialTypesArray['labels']))
        intervalVert = 1.5/4  # Locked at this interval because pages of 4 conditions each.
        startH = -.5
        startV = .8
        tempLineH = visual.Line(self.win, start=[-.99,.82], end=[.99,.82]) # End lines slightly shy of the right edge.
        divLinesH.append(tempLineH)
        tempLineV = visual.Rect(self.win, width=.01, height=2, fillColor="white", pos=[-.65,.3])
        divLinesV.append(tempLineV)
        clickRanges=[] # For making it easier ot detect if a line was clicked in. Y only (horizontal doesn't matter)
        for i in range(0,len(self.trialTypesArray['labels'])):
            # populate column headers and lines.
            hpos = (i)*intervalHoriz + startH+intervalHoriz/3
            tempText = visual.TextStim(self.win, alignHoriz='center', text=self.trialTypesArray['labels'][i],height=(1-startV)*.3, pos=[hpos, .94])
            tTypeHeaders.append(tempText)
            tempLineV = visual.Line(self.win, start=[hpos+intervalHoriz/2,.99], end=[hpos+intervalHoriz/2,-.675])
            divLinesV.append(tempLineV)
        if len(self.settings['condList']) >= 4:
            q = 4
        else:
            q = len(self.settings['condList'])
        for i in range(0, q):
            vpos = startV - (i + 1) * intervalVert + intervalVert / 1.5
            tempLineH = visual.Line(self.win, start=[-.99, vpos - intervalVert / 2], end=[.99, vpos - intervalVert / 2])
            divLinesH.append(tempLineH)

        for j in range(0, len(self.settings['condList'])): #condition labels. Here's where rubber meets road!
            block = (j+1)%4
            if block == 0:
                block = 4
            vpos = startV - block * intervalVert + intervalVert/1.5
            tempText = visual.TextStim(self.win, text=self.settings['condList'][j], alignHoriz='center',height=intervalVert*.35, pos=[condHeader.pos[0],vpos])
            drawConds.append(tempText)
            # And now, finally, we have to populate each of those conditions.
            for q in range(0,len(self.trialTypesArray['labels'])):
                if self.trialTypesArray['labels'][q] in condContent[j].keys():
                    txt = condContent[j][self.trialTypesArray['labels'][q]]
                else:
                    txt = []
                tempText = visual.TextStim(self.win, text=txt, height=sqrt(intervalVert)*.4*(1/(len(txt)+1))*(sqrt(2)/(sqrt(len(self.trialTypesArray['labels'])+1))), wrapWidth=intervalHoriz*.9,pos=[tTypeHeaders[q].pos[0],vpos], alignHoriz='center')
                drawConds.append(tempText)
            tempRange = [vpos+intervalVert/2,vpos-intervalVert/2]
            clickRanges.append(tempRange)
        # Finally the main loop  of this new display.
        done = False
        while 1 in self.mouse.getPressed():
            pass
        while not done:
            condHeader.draw()
            doneButton.draw()
            doneText.draw()
            addCondButton.draw()
            addCondText.draw()
            deleteCondButton.draw()
            deleteCondText.draw()
            randomCondsButton.draw()
            randomCondsText.draw()
            instrText.draw()
            if numPages > 1:
                if currPage < numPages:
                    nextPageArrow.draw()
                if currPage > 1:
                    lastPageArrow.draw()
            for i in range(0, len(divLinesV)):
                divLinesV[i].draw()
            for j in range(0, len(divLinesH)):
                divLinesH[j].draw()
            for k in range(0, len(tTypeHeaders)):
                tTypeHeaders[k].draw()
            if len(drawConds) <= 4*(len(self.trialTypesArray['labels'])+1):  # Each row has one column for each trial type plus one for the label, so rows of n trial types + 1
                for l in range(0, len(drawConds)):
                    drawConds[l].draw()
            else:
                for l in range((currPage-1)*4*(len(self.trialTypesArray['labels'])+1), currPage*4*(len(self.trialTypesArray['labels'])+1)):
                    if l < len(drawConds):  # Easy safety cutoff for when we can't fill a page, so we don't go out of bounds
                        drawConds[l].draw()
            self.win.flip()
            if 1 in self.mouse.getPressed():
                for i in range((currPage-1)*4, currPage*4):
                    p=self.mouse.getPos()
                    if i < len(clickRanges) and p[1] <= clickRanges[i][0] and p[1] >= clickRanges[i][1]:
                        if os.name is not 'posix':
                            while 1 in self.mouse.getPressed(): # Work on mouseup, impt. for windows.
                                pass
                            self.win.winHandle.set_visible(visible = False)
                        self.condSetter(cond=self.settings['condList'][i],ex=True)
                        if os.name is not 'posix':
                            self.win.winHandle.set_visible(visible=True)
                        while 1 in self.mouse.getPressed():
                            # Making sure that clicking "OK" doesn't immediately trip a click inside the interface
                            pass
                        done = True
                        # Refresh the condition display
                        self.condMaker(rep=True,currPage=currPage)
                if self.mouse.isPressedIn(addCondButton):
                    if os.name is not 'posix':
                        while 1 in self.mouse.getPressed():
                            pass
                        self.win.winHandle.set_visible(visible = False)
                    self.condSetter(ex=False)
                    if os.name is not 'posix':
                        self.win.winHandle.set_visible(visible = True)
                    while 1 in self.mouse.getPressed():
                        pass
                    done = True
                    # Start this over...
                    self.condMaker(rep=True,currPage=currPage)
                if self.mouse.isPressedIn(deleteCondButton) and len(self.settings['condList'])>0:
                    if os.name is not 'posix':
                        while 1 in self.mouse.getPressed():
                            pass
                        self.win.winHandle.set_visible(visible=False)
                    self.delCond()
                    if os.name is not 'posix':
                        self.win.winHandle.set_visible(visible=True)
                    while len(self.mouse.getPressed()) < 0:
                        pass
                    done = True
                    # Start this over...
                    self.condMaker(rep=True,currPage=currPage)
                if self.mouse.isPressedIn(randomCondsButton) and len(self.settings['condList'])>0:
                    if os.name is not 'posix':
                        while 1 in self.mouse.getPressed():
                            pass
                        self.win.winHandle.set_visible(visible=False)
                    self.condRandomizer()
                    if os.name is not 'posix':
                        self.win.winHandle.set_visible(visible=True)
                    while len(self.mouse.getPressed()) < 0:
                        pass
                    done = True
                if self.mouse.isPressedIn(nextPageArrow):
                    currPage = currPage + 1
                    if currPage > numPages:
                        currPage = numPages # Safety. Shouldn't be necessary.
                    while 1 in self.mouse.getPressed():
                        pass # so it doesn't jump pages
                if self.mouse.isPressedIn(lastPageArrow):
                    currPage = currPage - 1
                    if currPage < 1:
                        currPage = 1  # Safety. Shouldn't be necessary
                    while 1 in self.mouse.getPressed():
                        pass
                if self.mouse.isPressedIn(doneButton):
                    done = True
                    while 1 in self.mouse.getPressed():
                        pass # Just to make it not auto-click something on return to the main window
                
    
    def condSetter(self, cond='NEW', ex=False):
        """
        One dialog per trial type. Each dialog has a list of all the movies in that type
        This is not intuitive under the hood. The output of this is a dict with a list of movies, in order, for each
        trial type. This makes it slightly more human-intelligible than the previous system, which had a list of indexes.


        :param cond: Condition name
        :type cond: str
        :param ex: Whether the condition already exists
        :type ex: bool
        :return:
        :rtype:
        """
        condDlg2=gui.Dlg(title="Define condition")
        condDlg2.addField("Condition label:", cond)
        condDlg2.addText("You will have separate dialogs to set the order of movies in each trial type. Press OK to begin")
        condDinfo = condDlg2.show()
        if condDlg2.OK:
            condDinfo[0] = str(condDinfo[0])
            if ex and condDinfo[0] != cond: # Renamed existing condition
                self.settings['condList'][self.settings['condList'].index(cond)] = condDinfo[0] 
                if cond in self.condDict.keys():
                    self.condDict[condDinfo[0]] = self.condDict.pop(cond)
            cond = condDinfo[0]
            outputDict = {}
            i = 0
            while i < len(self.trialTypesArray['labels']):
                tempType = self.trialTypesArray['labels'][i]
                condTyDlg = gui.Dlg(title="Trial type " + tempType)
                condTyDlg.addText("Enter order in which you want movies to appear. If you do not want a movie to appear in this condition, leave blank or put 0.")
                if ex: #If there is an existing trial that we are modifying.
                    try:
                        movieOrder = self.condDict[cond][tempType]
                        for z in range(0, len(movieOrder)):
                            if type(movieOrder[z]) is int: # Convert old condition files
                                tempNum = movieOrder[z]
                                movieOrder[z] = self.settings['stimNames'][tempType][tempNum-1]
                    except:
                        movieOrder = []
                        for k in range(0, len(self.settings['stimNames'][tempType])):
                            movieOrder.append(self.settings['stimNames'][tempType][k]) #default order.
                else:
                    movieOrder = []
                    for k in range(0, len(self.settings['stimNames'][tempType])):
                        movieOrder.append(self.settings['stimNames'][tempType][k]) #default order.
                for x in range(0, len(self.settings['stimNames'][tempType])): #Yeah we gotta loop it again.
                    thisMov = self.settings['stimNames'][tempType][x]
                    if thisMov in movieOrder: #If that movie appears in the movie order.
                        condTyDlg.addField(thisMov, movieOrder.index(thisMov)+1)
                    else:
                        condTyDlg.addField(thisMov)
                condTyInfo = condTyDlg.show()
                if condTyDlg.OK:
                    i += 1
                    # Now we need to reinterpret all that input ot make the output.
                    # First, code all non-numbers or invalid numbers as 0
                    condTyInfo = [0 if type(x) is not int or x <= 0 else x for x in condTyInfo] 
                    # Identify any doubles other than 0s, if so error msg and redo
                    maxNum = max(condTyInfo)
                    stop = False
                    for q in range(1, maxNum+1):
                        if condTyInfo.count(q) > 1:
                            errDlg = gui.Dlg(title="Warning, invalid order!")
                            errDlg.addText("Order has a repeat of the same number. Please re-enter.")
                            i -= 1 # This is why our for loop became a while loop.
                            irrel = errDlg.show()
                            stop = True
                    if maxNum == 0:
                        errDlg = gui.Dlg(title="Warning, invalid order!")
                        errDlg.addText("No stimuli selected. Please re-enter")
                        i -= 1
                        irrel = errDlg.show()
                        stop = True
                    if not stop:
                        # Go through and construct the new trial order.
                        tempOrder = []
                        for q in range(1, maxNum+1):
                            try:
                                tMov = condTyInfo.index(q) # Finds the movie index to add the movie to the order
                                tempOrder.append(self.settings['stimNames'][tempType][tMov])
                            except ValueError:
                                errDlg = gui.Dlg(title="Warning, invalid order!")
                                errDlg.addText("Non-consecutive numbering (e.g. 1,2,5). Please re-enter with consecutive numbering!")
                                i -= 1
                                irrel = errDlg.show()
                                stop = True
                                break
                        if not stop: # Stops it from accidentally saving bad orders due to non-consecutive numbering.
                            outputDict[tempType] = tempOrder
           #Finally, rewrite everything that needs rewriting.
            self.condDict[cond] = outputDict
            if not ex:
                self.settings['condList'].append(str(cond))
        
    def delCond(self):
        """
        Present list of existing conditions. Choose one to remove.
        """
        dCondDlg = gui.Dlg(title="Delete a condition")
        dCondDlg.addField('Choose a condition to delete', choices = self.settings['condList'])
        delCond = dCondDlg.show()
        if dCondDlg.OK:
            self.settings['condList'].remove(str(delCond[0]))
            del self.condDict[delCond[0]]          

    def condRandomizer(self):
        """
        This is based on other scripts I've made. Basically, say you have four conditions, and you want four participants
        to be assigned to each one, but you want to be totally blind to which condition a given participant is in. Here,
        once you have made your four conditions, you can tell it to create a condition list that it never shows you that
        has each condition X times, and that becomes the new condition file/list/etc.

        :return:
        :rtype:
        """

        rCondDlg = gui.Dlg(title="Create randomized condition list")
        rCondDlg.addField('How many repetitions of each condition? (Length of list will be this X # conditions)',1)
        rCondDlg.addField('Condition label prefix? (Will be followed by "01...N" in condition list)', self.settings['prefix'])
        randCond = rCondDlg.show()
        if rCondDlg.OK and isinstance(randCond[0],int):
            totalN = len(self.condDict.keys())*randCond[0]
            newCondList = [] # Will replace condlist.
            for i in range (1, totalN+1):
                if i < 10:
                    numStr = "0" + str(i)
                else:
                    numStr = str(i)
                newCondList.append(randCond[1]+numStr)
            listOfConds = [] # A list of the existing conditions that will be the items for the new condDict.
            for j, k in self.condDict.items():
                listOfConds.append(k)
            mixer = [] # The list with all repetitions, which we then shuffle.
            for q in range(0, len(listOfConds)):
                for r in range(0, randCond[0]):
                    mixer.append(listOfConds[q])
            random.shuffle(mixer)

            newCondDict={}
            for s in range(0, len(newCondList)):
                newCondDict[newCondList[s]] = mixer[s]

            self.condDict = newCondDict
            self.settings['condList'] = newCondList
        elif rCondDlg.OK:
            errDlg = gui.Dlg(title="Warning, invalid number!")
            errDlg.addText("Multiplier must be a whole number, no decimals. Please re-open condition settings and try again.")
            errDlg.show()
   
   
    def habSettingsDlg(self, lastSet=[],redo=False): #Habituation criteria
        """
        Dialog for settings relating to habituation criteria:

        0 = maxHabTrials (maximum possible hab trials if criterion not met)

        1 = setCritWindow (# trials summed over when creating criterion)

        2 = setCritDivisor (denominator of criterion calculation . e.g., sum of first 3 trials
            divided by 2 would have 3 for setCritWindow and 2 for this.)

        3 = setCritType (peak window, max trials, first N, or first N above threshold)

        4 = habThresh (threshold for N above threshold)

        5 = metCritWindow (# trials summed over when evaluating whether criterion has been met)

        6 = metCritDivisor (denominator of sum calculated when determining if criterion has been met)

        7 = metCritStatic (static or moving window?)



        :param lastSet: If information entered is invalid and the dialog needs to be shown again, this allows it to remember what was previously entered.
        :type lastSet: list
        :param redo: Checking if redoing last setting
        :type redo: boolean
        :return:
        :rtype:
        """
        if not redo:
            lastSet = []
            lastSet.append(self.settings['maxHabTrials'])
            lastSet.append(self.settings['setCritWindow'])
            lastSet.append(self.settings['setCritDivisor'])
            lastSet.append(self.settings['setCritType'])
            lastSet.append(self.settings['habThresh'])
            lastSet.append(self.settings['metCritWindow'])
            lastSet.append(self.settings['metCritDivisor'])
            lastSet.append(self.settings['metCritStatic'])

        hDlg = gui.Dlg(title="Habituation block settings")
        windowtypes = ['First', 'Peak', 'Max', 'Threshold']
        winchz = [x for x in windowtypes if x != lastSet[3]]
        winchz.insert(0, lastSet[3])
        if lastSet[6] == 'Fixed':
            evalChz = ['Fixed','Moving']
        else:
            evalChz = ['Moving', 'Fixed']

        hDlg.addField("Max number of habituation trials (if criterion not met)", self.settings['maxHabTrials'])
        hDlg.addField("Number of trials to sum looking time over when making hab criterion", self.settings['setCritWindow'])
        hDlg.addField("Number to divide sum of looking time by when computing criterion", self.settings['setCritDivisor'])
        hDlg.addField("Criterion window First trials, first trials above Threshold, dynamic Peak contiguous window, or the set of hab trials with Max looking time?", choices=winchz)
        hDlg.addField("Threshold value to use if 'Threshold' selected above (ignored otherwise)", self.settings['habThresh'])
        hDlg.addField("Number of trials to sum looking time over when determining whether criterion has been met", self.settings['metCritWindow'])
        hDlg.addField("Number to divide sum of looking time by when determining whether criterion has been met", self.settings['metCritDivisor'])
        hDlg.addField("Evaluate criterion over moving window or fixed windows?", choices=evalChz)
        habDat=hDlg.show()
        if hDlg.OK:
            skip = False
            intevals = [0,1,5]
            fevals = [2,4,6] # These can be floats
            for i in intevals:
                if not isinstance(habDat[i], int):
                    if isinstance(habDat[i], str):
                        try:
                            habDat[i]=eval(habDat[i])
                        except:
                            skip = True
                        if not isinstance(habDat[i], int):
                            skip = True
                    else:
                        skip = True
            for i in fevals:
                if not isinstance(habDat[i], float):
                    try:
                        habDat[i]=float(habDat[i])
                    except:
                        skip = True
            lastSet = habDat
            if not skip:
                self.settings['maxHabTrials'] = habDat[0]
                self.settings['setCritWindow'] = habDat[1]
                self.settings['setCritDivisor'] = habDat[2]
                self.settings['setCritType'] = habDat[3]
                self.settings['habThresh'] = habDat[4]
                self.settings['metCritWindow'] = habDat[5]
                self.settings['metCritDivisor'] = habDat[6]
                self.settings['metCritStatic'] = habDat[7]
            else:
                errDlg = gui.Dlg(title="Warning, invalid number!")
                errDlg.addText(
                    "Please make sure all values are valid numbers. Remember that any 'number of trials' field must be a whole number (no decimal).")
                errDlg.show()
                self.habSettingsDlg(habDat,redo=True)



    def saveDlg(self):
        """
        Opens a save dialog allowing you to choose where to save your project.
        Essentially sets self.folderPath

        :return:
        :rtype:
        """
        go = True
        if len(self.settings['trialOrder']) == 0:
            warnDlg = gui.Dlg("Warning: No trials in study flow!")
            warnDlg.addText("You haven't added any trials to the study flow yet! You won't be able to run this experiment.")
            warnDlg.addText("Hit 'OK' to save anyways (you can add trials later and save then) or cancel to go back.")
            warnDlg.show()
            if not warnDlg.OK:
                go = False
        if go:
            NoneType = type(None)
            if len(self.folderPath) > 0:
                for i,j in self.settings['stimList'].items():
                    if self.settings['stimList'][i]['stimType'] != 'Image with audio':
                       self.stimSource[i] = j['stimLoc'] # Re-initializes the stimSource dict to incorporate both existing and new stim.
                    else:
                        tempAname = os.path.split(j['audioLoc'])[1]
                        tempIname = os.path.split(j['imageLoc'])[1]
                        self.stimSource[tempAname] = j['audioLoc']
                        self.stimSource[tempIname] = j['imageLoc']
            sDlg = gui.fileSaveDlg(initFilePath=os.getcwd(), initFileName=self.settings['prefix'], prompt="Name a folder to save study into", allowed="")
            if type(sDlg) is not NoneType:
                self.settings['folderPath'] = sDlg + self.dirMarker #Makes a folder of w/e they entered
                #If there is no pre-selected prefix, make the prefix the folder name!
                if self.settings['prefix'] == "PyHabExperiment":
                    self.settings['prefix'] = os.path.split(sDlg)[1]
                self.folderPath=self.settings['folderPath']
                #Add save button if it does not exist.
                if self.saveEverything not in self.buttonList['functions']:
                    saveButton = visual.Rect(self.win,width=.15, height=.67*(.15/self.aspect), pos=[-.52,-.9],fillColor="green")
                    saveText = visual.TextStim(self.win, text="SAVE",color="black",height=saveButton.height*.5, pos=saveButton.pos)
                    self.buttonList['shapes'].append(saveButton)
                    self.buttonList['text'].append(saveText)
                    self.buttonList['functions'].append(self.saveEverything)
                self.saveEverything()
    
    def saveEverything(self):
        """
        Saves a PyHab project to a set of folders dictated by self.folderPath

        :return:
        :rtype:
        """
        if not os.path.exists(self.folderPath):
            os.makedirs(self.folderPath) # creates the initial folder if it did not exist.
        success = True  # Assume it's going to work.
        # Structure: Top-level folder contains script, then you have data and stimuli.
        dataPath = self.folderPath+'data'+self.dirMarker
        if not os.path.exists(dataPath):
            os.makedirs(dataPath)
        stimPath = self.folderPath+'stimuli'+self.dirMarker
        if not os.path.exists(stimPath):
            os.makedirs(stimPath)
        codePath = self.folderPath+'PyHab'+self.dirMarker
        if not os.path.exists(codePath):
            os.makedirs(codePath)
        srcDir = 'PyHab'+self.dirMarker
        # Condition file! Save if there are any conditions created.
        # Convoluted mess because the dict won't necessarily be in order and we want the file to be.
        if len(self.condDict)>0:
            tempArray = []
            for j in range(0, len(self.settings['condList'])):
                tempArray.append([self.settings['condList'][j],self.condDict[self.settings['condList'][j]]])
            with open(self.folderPath+self.settings['condFile'],'w') as co:
                secretWriter = csv.writer(co,lineterminator='\n')
                for k in range(0, len(tempArray)):
                    secretWriter.writerow(tempArray[k])
        # copy stimuli if there are stimuli.
        if len(self.stimSource) > 0:
            for i, j in self.stimSource.items():  # Find each file, copy it over
                try:
                    targPath = stimPath + i
                    shutil.copyfile(j, targPath)
                except:
                    success = False
                    print('Could not copy file ' + j + ' to location ' + targPath + '. Make sure both exist!')
                if success:
                    for q, r in self.settings['stimList'].items():
                        if r['stimType'] != 'Image with audio':
                            if q == i:  # For movies, images, or audio in isolation, the keys match.
                                r['stimLoc'] = 'stimuli' + self.dirMarker + q
                        else:  # Here we have to look at the file paths themselves
                            if os.path.split(r['audioLoc'])[1] == i:
                                r['audioLoc'] = 'stimuli' + self.dirMarker + os.path.split(j)[1]
                            elif os.path.split(r['imageLoc'])[1] == i:
                                r['imageLoc'] = 'stimuli' + self.dirMarker + os.path.split(j)[1]

        if len(list(self.settings['attnGetterList'].keys())) > 0:  # This should virtually always be true b/c default attngetter.
            for i, j in self.settings['attnGetterList'].items():
                try:
                    targPath = stimPath + 'attnGetters' + self.dirMarker
                    if not os.path.exists(targPath):
                        os.makedirs(targPath)
                    if not os.path.exists(targPath + j['stimName']):
                        shutil.copyfile(j['stimLoc'], targPath + j['stimName'])
                        j['stimLoc'] = 'stimuli' + self.dirMarker + 'attnGetters' + self.dirMarker + j['stimName']
                except:
                    success = False
                    print('Could not copy attention-getter file ' + j['stimLoc'] + ' to location ' +  targPath + '. Make sure both exist!')
        if not success:
            errDlg = gui.Dlg(title="Could not copy stimuli!")
            errDlg.addText("Some stimuli could not be copied successfully. See the output of the coder window for details.")
            errDlg.show()
        # Delete any removed stimuli
        if len(self.delList) > 0:
            for i in range(0, len(self.delList)):
                delStimPath = os.path.join(stimPath, self.delList[i])
                if os.path.exists(delStimPath):
                    try:
                        os.remove(delStimPath)
                    except:
                        print("Could not remove stimuli " + delStimPath + " from experiment folder.")
                else:
                    print("Stimuli " + delStimPath + " does not exist to be removed!")
            self.delList = []
        # create/overwrite the settings csv.
        settingsPath = self.folderPath+self.settings['prefix']+'Settings.csv'
        with open(settingsPath,'w') as so: # this is theoretically safer than the "close" system.
            settingsOutput = csv.writer(so, lineterminator='\n')
            for i, j in self.settings.items(): # this is how you write key/value pairs
                settingsOutput.writerow([i, j])

        # Copy over the class and such. Since these aren't modified, make sure they don't exist first.
        classPath = 'PyHabClass.py'
        classTarg = codePath+classPath
        classPLPath = 'PyHabClassPL.py'
        classPLTarg = codePath+classPLPath
        buildPath = 'PyHabBuilder.py'
        buildTarg = codePath+buildPath
        initPath = '__init__.py'
        initTarg = codePath+initPath
        try:
            if not os.path.exists(classTarg):
                shutil.copyfile(srcDir+classPath, classTarg)
            if not os.path.exists(classPLTarg):
                shutil.copyfile(srcDir+classPLPath, classPLTarg)
            if not os.path.exists(buildTarg):
                shutil.copyfile(srcDir+buildPath, buildTarg)
            if not os.path.exists(initTarg):
                shutil.copyfile(srcDir+initPath, initTarg)
        except:
            # error dialog
            errDlg = gui.Dlg(title="Could not copy PyHab and builder!")
            errDlg.addText("Failed to copy PyHab class scripts and builder script. These can be copied manually from the PyHab folder and will be needed!")
            errDlg.show()
            success=False
            print("Could not copy PyHab and builder!")
        # copy/create the launcher script
        launcherPath = self.folderPath+self.settings['prefix']+'Launcher.py'
        if not os.path.exists(launcherPath):
            try:
                # the location of the pyHabLauncher template file
                if os.path.exists(srcDir+'PyHab Launcher.py'):
                    launcherSource = srcDir+'PyHab Launcher.py'
                else:
                    # It'll end in launcher.py. So need to find a file in the working directory with the last 11 characters 'Launcher.py'
                    fileList = os.listdir()
                    for i in range(0, len(fileList)):
                        if fileList[i][-11:] == 'Launcher.py':
                            launcherSource = fileList[i]
                # Open file and find line 5, aka the path to the settings file, replace it appropriately
                with open(launcherSource,'r') as file:
                    launcherFile = file.readlines()
                newLine = 'setName = \"' + self.settings['prefix']+'Settings.csv\"\r\n'  # Simplified, so it always runs the settings file in that folder.
                launcherFile[6] = newLine
                launcherFile[7] = "#Created in PsychoPy version " + __version__ + "\r\n"
                # now write the new file!
                with open(launcherPath, 'w') as t:
                    t.writelines(launcherFile)
            except:
                errDlg = gui.Dlg(title="Could not save!")
                errDlg.addText("Launcher script failed to save! Please try again or manually copy the launcher script and change the settings line.")
                errDlg.show()
                success=False
                print('creating launcher script failed!')
                if len(launcherSource) == 0:
                    print('Could not find launcher template file!')
                else:
                    print('Found launcher template, still could not save launcher script')
        if success:
            saveSuccessDlg = gui.Dlg(title="Experiment saved!")
            saveSuccessDlg.addText("Experiment saved successfully to" + self.folderPath)
            saveSuccessDlg.show()


    def _runLauncher(self,launcherPath):
        """
        Flagrantly copied from psychopy's own app/coder interface, where it is used for running scripts in the IDE.
        Basically it's like hitting the "run" button in the coder interface, but from within an actively-running script.

        :param launcherPath: Full path to launcher file created by builder
        :type launcherPath: string
        :return:
        :rtype:
        """
        fullPath = launcherPath
        path, scriptName = os.path.split(fullPath)
        importName, ext = os.path.splitext(scriptName)
        # set the directory and add to path
        os.chdir(path)  # try to rewrite to avoid doing chdir in the coder
        sys.path.insert(0, path)

        # do an 'import' on the file to run it
        # delete the sys reference to it (so we think its a new import)
        if importName in sys.modules:
            sys.modules.pop(importName)
        exec ('import %s' % (importName))  # or run first time
