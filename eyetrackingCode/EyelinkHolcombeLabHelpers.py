# EyeLinkHolcombeLabHelpers.py
#
# Started 2011 Wing (Wei-Ying Chen), modified from pylink API
# Modified by Chris Fajou, pre-April 2015.
# Modified by Alex, November 2023 onward
# Provides functions for using an eye tracker that allows experiment code to be simple and For EyeLink1000

import pylink
try: #This only works if the code executing is one folder up, making eyetrackingCode a sub-folder
    from eyetrackingCode import EyeLinkCoreGraphicsPsychoPy #imports from eyetrackingCode subfolder the file provided by Eyelink
except Exception as e:
    print("An exception occurred in EyelinkHolcombeLabHelpers.py:",str(e))
    print('Could not import EyeLinkCoreGraphicsPsychoPy.py (you need that file to be in the eyetrackingCode subdirectory, which needs an __init__.py file in it too)')
import sys, os, gc, string
from psychopy import visual, info, misc, monitors, event, core
from numpy import array, hstack
 
RIGHT_EYE = 1
LEFT_EYE = 0
BINOCULAR = 2
HIGH = 1
LOW = 0
WHITE = (255,255,255)
GRAY = GREY = (128,128,128)
BLACK = (0,0,0)
spath = os.path.dirname(sys.argv[0])
if len(spath) !=0: os.chdir(spath)
use_retina = False
 
class EyeLinkTrack_Holcombe(): 
    def __init__(self, win, clock, sj = "TEST", saccadeSensitivity = HIGH, calibrationType = 'HV9',calibrationTargetColor = WHITE,calibrationBgColor = BLACK, CalibrationSounds = False,screen=(1024,768)):
        '''win: psychopy visual window used for the experiment
          clock: psychopy time clock recording time for whole experiment
          sj: Subject identifier string (affects EDF filename)
          saccadeSensitivity:
            HIGH: Pursuit and neurological work
            LOW:  Cognitive research
          calibrationType:
            H3: Horizontal 3-point
            HV3: 3-point calibration, poor linearization
            HV5: 5-point calibration, poor at corners
            HV9: 9-point calibration, best overall
        calibrationTargetColor and calibrationBgColor:
            RGB tuple, i.e., (255,0,0) for Red
            One of: BLACK, WHITE, GRAY
        calibrationSounds:
            True: enable feedback sounds when calibrating'''
        self.edfFileName = str(sj)+".EDF"   # EDF filename on tracker machine can only be 8 characters!
        # check if the filename is valid (length <= 8 & no special char)
        allowed_char = string.ascii_letters + string.digits + '_' + '.'
        if not all([c in allowed_char for c in self.edfFileName]):
            print('ERROR: Invalid EDF filename characters in',self.edfFileName)
        if len(self.edfFileName) > 8:
            print('ERROR: EDF eyetracker machine filename should not exceed 8 characters, shortening it to first eight:',self.edfFileName[0:8])
            self.edfFileName = self.edfFileName[0:8]
        print("Eyetracker PC filename will be (shortened to 8 characters if you entered more than 8):",self.edfFileName)
        
        print("Connecting to eyetracker.")
        try:
            self.tracker = pylink.EyeLink() #pylink.EyeLink('100.1.1.1')
        except RuntimeError as err:
            fix = '\nFailed to connect to eyetracker. Try checking ethernet connection and the IP address of the stimulus presentation PC!'
            print(err, fix)
            core.quit()
                
        self.timeCorrection = clock.getTime() - self.tracker.trackerTime()

        print("Trying to enable Eyelink to draw calibration targets by creating or linking to graphics window")
        oldSchoolWayOfEyelinkDrawingToScreen = False
        if oldSchoolWayOfEyelinkDrawingToScreen:
            # Instantiate a graphics environment (genv) just to draw calibration targets on experiment computer screen
            genv = EyeLinkCoreGraphicsPsychoPy.EyeLinkCoreGraphicsPsychoPy(self.tracker, win)
            #genv = EyeLinkCoreGraphicsPsychoPyHolcombeLab.EyeLinkCoreGraphicsPsychoPy(self.tracker, win)
    
            # Set background and foreground colors for calibration
            foreground_color = (-1, -1, -1)
            background_color = win.color
            genv.setCalibrationColors(foreground_color, background_color)
    
            # The target could be a "circle" (default), a "picture", a "movie" clip,
            # or a rotating "spiral".
            genv.setTargetType('circle')
            # Configure the size of the calibration target (in pixels)
            genv.setTargetSize(24)
    
            #Set the calibration settings:
            #pylink.setCalibrationColors(WHITE, BLACK) # Sets the calibration target and background color(foreground_color, background_color)
            #AH November 2023 why does the below not work? It says the Psychopy object doesn't have a setCalibrationSounds function, but genv is what's used in eyeTrackerBasedOnPicture.py
            # if CalibrationSounds:
            #     genv.setCalibrationSounds("", "", "")
            #     genv.setDriftCorrectSounds("", "off", "off")
            # else:
            #     genv.setCalibrationSounds("off", "off", "off")
            #     genv.setDriftCorrectSounds("off", "off", "off")
    
            if use_retina:
                genv.fixMacRetinaDisplay()
            # Request Pylink to use the genv PsychoPy window we opened above for calibration
            pylink.openGraphicsEx(genv)
        else:
            #More modern way of doing it? Tries to use existing graphics window opened by Psychopy
            #If there is already an active Pygame window, Pylink will use it for calibration when we call pylink.openGraphics().
            pylink.openGraphics()  
            #pygame.display.set_mode((SCN_W, SCN_H), DOUBLEBUF | FULLSCREEN) #https://link.springer.com/chapter/10.1007/978-3-030-82635-2_4#Sec15

        # open data file on eyetracking PC
        self.tracker.openDataFile(self.edfFileName)

        #EyeLink Tracker Configuration
        pylink.flushGetkeyQueue();# Initializes the key queue used by getkey(). It may be called at any time to get rid any of old keys from the queue.
        self.tracker.setOfflineMode();#Recommended that first place EyeLink tracker in off-line (idle) mode.
        self.tracker.sendCommand("screen_pixel_coords =  0 0 %d %d"%( tuple(screen) ))
        self.tracker.setCalibrationType(calibrationType)
        self.tracker.sendCommand("driftcorrect_cr_disable=AUTO") #OFF: enables drift correction; AUTO: Turns on drift check (not correction); ON: Turns off both
        #If driftcorrect_cr_disable is OFF, drift correction is enabled and one needs to set a landmark:
        #self.tracker.sendCommand("online_dcorr_refposn = %d %d"%( tuple(screen/2) ))
        #I'm not sure if the next line is necessary or not
        #self.tracker.sendCommand("online_dcorr_button=ON") #ON Next the “Drift Corr” button is enabled onthe Recording Screen by turning it on. The drift correction will be by clicking the button. 

        #self.tracker.sendCommand("generate_default_targets = NO") 
        #self.tracker.sendCommand("calibration_targets = 512,384 512,417 512,351 402,384 622,384 402,417 622,417 402,351 622,351")
        #self.tracker.sendCommand("validation_targets = 512,384 512,417 512,351 402,384 622,384 402,417 622,417 402,351 622,351")

        self.tracker.sendMessage("DISPLAY_COORDS  0 0 %d %d"%( tuple(screen) ))
        eyelink_ver = self.tracker.getTrackerVersion()
        if eyelink_ver == 3:
            tvstr = self.tracker.getTrackerVersionString()
            vindex = tvstr.find("EYELINK CL")
            tracker_software_ver = int(float(tvstr[(vindex + len("EYELINK CL")):].strip()))
        else: tracker_software_ver = 0
        if eyelink_ver>=2:
            self.tracker.sendCommand("select_parser_configuration %d" %saccadeSensitivity)
        else:
            if saccadeSensitivity == HIGH:svt, sat = 22, 5000  
            else: svt, sat = 30, 9500
            self.tracker.sendCommand("saccade_velocity_threshold = %d" %svt)   
            self.tracker.sendCommand("saccade_acceleration_threshold = %d" %sat)
        if eyelink_ver == 2: #turn off scenelink camera stuff
            self.tracker.sendCommand("scene_camera_gazemap = NO")
 
        # set EDF file contents
        self.tracker.setFileEventFilter("LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON")
        if tracker_software_ver>=4:self.tracker.setFileSampleFilter("LEFT,RIGHT,GAZE,AREA,GAZERES,STATUS,HTARGET")
        else:self.tracker.setFileSampleFilter("LEFT,RIGHT,GAZE,AREA,GAZERES,STATUS")
        
        # set link data (used for gaze cursor)
        self.tracker.setLinkEventFilter("LEFT,RIGHT,FIXATION,SACCADE,BLINK,BUTTON")
        if tracker_software_ver>=4:self.tracker.setLinkSampleFilter("LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,HTARGET")
        else:self.tracker.setLinkSampleFilter("LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS")
        
        #self.tracker.setAcceptTargetFixationButton(1) # This programs a specific button for use in drift correction.
        
        print("Beginning tracker setup")
        try:
            self.tracker.doTrackerSetup() #This brings up the grey screen and tries to do the calibration, drawing calibration targets
        except RuntimeError as err:
            print('ERROR when trying to calibrate:', err)
            self.tracker.exitCalibration()
 
    def sendMessage(self, msg):
        '''Record a message to the tracker'''
        print(msg)
        self.tracker.sendMessage(msg)
 
    def sendCommand(self, msg):
        '''Send command to the tracker'''
        print(msg)
        self.tracker.sendCommand(message)
 
    def resetEventQue(self):
        '''Reset the eyetracker event cue
            usage: use this prior to a loop calling recordFixation() so
            that old fixations or other events are cleared from the 
            buffer.'''
        self.tracker.resetData()
 
    def getStatus(self):
        """Return the status of the connection to the eye tracker"""
        if self.tracker.breakPressed():
            return("ABORT_EXPT")
        if self.tracker.escapePressed():
            return("SKIP_TRIAL")
        if self.tracker.isRecording()==0:
            return("RECORDING")
        if self.tracker.isConnected():
            return("ONLINE")
        else:
            return("OFFLINE")
        return("UNKNOWN STATUS: " + str(self.tracker.getStatus()) )
 
    def startEyeTracking(self,trial, calibTrial,widthPix,heightPix):  
        #Set up each trial with the eye tracker
        if calibTrial: cond = "Test/Calibration Trial"
        else: cond = "Non-test/no calibration trial"
        message ="record_status_message 'Trial %d %s'"%(trial+1, cond)
        self.tracker.sendCommand(message)
        msg = "TRIALID %s"%trial
        self.tracker.sendMessage(msg)
        #The following does drift correction at the begin of each trial
        if calibTrial:# Does drift correction and handles the re-do camera setup situations
            self.tracker.enableAutoCalibration()
            while True:
                try:
                    # Eyelink recommends drift-check at the beginning of each trial
                    # the doDriftCorrect() function requires target position in integers
                    # the last two arguments:
                    # draw_target (1-default, 0-you draw the target then call doDriftCorrect)
                    # allow_setup (1-press ESCAPE to recalibrate, 0-not allowed)                    
                    error = self.tracker.doDriftCorrect(widthPix/2,heightPix/2,1,1) 
                    if error != 27:
                        #self.tracker.applyDriftCorrect
                        break
                    else:
                        self.tracker.doTrackerSetup() #this starts the actual calibration, bringing up the gray calibration screen
                except:
                    print("Exception")
                    break
        self.tracker.disableAutoCalibration()
        #self.tracker.sendCommand('start_drift_correction DATA =1 1 1 1') #CF - start drift correct??
        #self.tracker.applyDriftCorrect() #CF - added to actually correct for drift
        self.tracker.setOfflineMode() #CF adds this to stop skipping trials due to not recording
        pylink.msecDelay(50)
        error = self.tracker.startRecording(1,1,1,1)#start to recording (File_samples, File_events, Link_samples, Link_events); if 1, writes something to EDF file. If 0, disables something recording.
        if error: return error;
        pylink.beginRealTimeMode(100)
 
    def stopEyeTracking(self):
        #Ends recording: adds 100 msec of data to catch final events
        pylink.endRealTimeMode()
        pylink.pumpDelay(100)
        self.tracker.stopRecording()
        
    def closeConnectionToEyeTracker(self,eyeMoveFile):
        #Clean everything up, save data and close connection to tracker
        msgs = ""
        if self.tracker != None:
            # File transfer and cleanup!
            self.tracker.setOfflineMode();
            core.wait(0.5)
            #Close the file and transfer it to Display PC
            self.tracker.closeDataFile()

            try:
                # Download the EDF data file from the Host PC to a local data folder
                # parameters: source_file_on_the_host, destination_file_on_local_drive
                self.tracker.receiveDataFile(self.edfFileName,eyeMoveFile) 
            except RuntimeError as error:
                msgs = msgs + " Tried to get EDF file and save it locally as " + eyeMoveFile
                msgs = msgs + 'but received this ERROR with receiveDataFile: ' + error

            self.tracker.close();
            #Close the experiment graphics
            pylink.closeGraphics()
            msgs = msgs + "Eyelink connection closed successfully"
        else:
            msgs = msgs + "Eyelink not available, not closed properly"
        return (msgs)

 
