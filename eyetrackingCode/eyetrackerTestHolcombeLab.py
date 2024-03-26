#Test connection to Eyelink eyetracker using routines used by Holcombe lab
try:
    import pylink #to turn off eyetracker graphics environment after eyetracker calibration. pylink comes from Eyelink Developers Kit download
except Exception as e:
    print("When trying to import Eyelink's pylink library, an exception occurred:",str(e))
    print('pylink is not included in PsychoPy download, you have to download and install the Eyelink Developers Kit from the SR Research Forum website.')

try:
    #from eyetrackingCode 
    import EyelinkHolcombeLabHelpers #imports from eyetrackingCode subfolder.
    #EyeLinkTrack_Holcombe class originally created by Chris Fajou to combine lots of eyelink commands to create simpler functions
except Exception as e:
    print("Failure to import EyelinkHolcombeLabHelpers.py (error:",str(e), 
          "You need that file to be in the eyetrackingCode subdirectory, which needs an __init__.py file in it too")

import psychopy.core, psychopy.monitors
import sys
sys.path.append('..')  # Appends the parent directory to the system path, so can import modules from the parent directory
import helpersAOH

#Need to open graphics window to send to EyeLink
monitorname = 'testMonitor' # 'mitsubishi' #in psychopy Monitors Center
monitorwidth = 38; viewdist = 57
widthPixRequested = widthPix = 800
heightPixRequested = heightPix = 600
mon = psychopy.monitors.Monitor(monitorname,width=monitorwidth, distance=viewdist)#fetch the most recent calib for this monitor
mon.setSizePix( (widthPixRequested,heightPixRequested) )
bgColor = [0,0,0]; allowGUI = True; units = 'deg'; fullscr=False; scrn=0; waitBlank=False; autoLogging=False
myWin = helpersAOH.openMyStimWindow(mon,widthPixRequested,heightPixRequested,bgColor,allowGUI,units,fullscr,scrn,waitBlank,autoLogging)
myWin.setRecordFrameIntervals(False)

trialClock = psychopy.core.Clock()
subject = 'eyeltest'
#EDF_fname_local=('EyeTrack_'+subject+'_'+timeAndDateStr+'.EDF')
print('Trying to connect to Eyelink')
try:
    my_tracker = EyelinkHolcombeLabHelpers.EyeLinkTrack_Holcombe(myWin,trialClock,subject,1, 'HV5',(255,255,255),(0,0,0),False,(widthPix,heightPix))
except Exception as e:
    print( "Failure of call to  EyelinkHolcombeLabHelpers.EyeLinkTrack_Holcombe (error:",str(e), ")" )
