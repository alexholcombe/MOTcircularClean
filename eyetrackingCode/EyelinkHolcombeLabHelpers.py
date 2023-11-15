# EyeLinkHolcombeLabHelpers.py
#
# Started 2011 Wing (Wei-Ying Chen), modified from pylink ATI and open source code
# Modified by Chris Fajou, pre-April 2015.
# Modified by Alex, November 2023
# Provides functions for using an eye tracker that allows experiment code to be simple and For EyeLink1000

import pylink
try:
    from eyetrackingCode import EyeLinkCoreGraphicsPsychoPyHolcombeLab #imports from eyetrackingCode subfolder the file provided by Eyelink
except Exception as e:
    print("An exception occurred in EyelinkHolcombeLabHelpers.py:",str(e))
    print('Could not import EyeLinkCoreGraphicsPsychoPyHolcombeLab.py (you need that file to be in the eyetrackingCode subdirectory, which needs an __init__.py file in it too)')
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
        if len(edf_fname) > 8:
            print('ERROR: EDF eyetracker machine filename should not exceed 8 characters, shortening it to first eight:',
                   self.edfFileName[0:8])
            self.edfFileName = self.edfFileName[0:8]
        print("Eyetracker PC filename will be:",self.edfFileName)
        print("Connecting to eyetracker.")
        self.tracker = pylink.EyeLink()
        self.timeCorrection = clock.getTime() - self.tracker.trackerTime()
        print("Loading custom graphics")
        #Initializes Experiment Graphics
        genv = EyeLinkCoreGraphicsPsychopyHolcombeLab.EyeLinkCoreGraphicsPsychoPy(self.tracker, win, screen)
        pylink.openGraphicsEx(genv)
        # opendatafile
        self.tracker.openDataFile(self.edfFileName)
        
        #EyeLink Tracker Configuration
        pylink.flushGetkeyQueue();# Initializes the key queue used by getkey(). It may be called at any time to get rid any of old keys from the queue.
        self.tracker.setOfflineMode();#Recommended that first place EyeLink tracker in off-line (idle) mode.
        self.tracker.sendCommand("screen_pixel_coords =  0 0 %d %d"%( tuple(screen) ))
        self.tracker.setCalibrationType(calibrationType)
        self.tracker.sendCommand("driftcorrect_cr_disable=OFF") #CF - OFF: turns on drift CORRECT; AUTO: Turns on drift CHECK; ON: Turns off both
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
        
          #Set the calibration settings:
        #pylink.setCalibrationColors(WHITE, BLACK) # Sets the calibration target and background color(foreground_color, background_color)
        if CalibrationSounds:
            pylink.setCalibrationSounds("", "", "")
            pylink.setDriftCorrectSounds("", "off", "off")
        else:
            pylink.setCalibrationSounds("off", "off", "off")
            pylink.setDriftCorrectSounds("off", "off", "off")
            
        print("Beginning tracker setup")
        self.tracker.doTrackerSetup()
 
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
        if self.tracker != None:
            # File transfer and cleanup!
            self.tracker.setOfflineMode();
            core.wait(0.5)
            #Close the file and transfer it to Display PC
            self.tracker.closeDataFile()
            self.tracker.receiveDataFile(self.edfFileName,eyeMoveFile) 
            self.tracker.close();
            #Close the experiment graphics
            pylink.closeGraphics()
            return "Eyelink connection closed successfully"
        else:
            return "Eyelink not available, not closed properly"

 
