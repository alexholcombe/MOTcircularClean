Eyetracking
==============

## Communication from Psychopy to Eyelink

Call functions in `EyelinkEyetrackerForPsychopySUPA3.py` to open communication with eyetracker, to generate the calibration display and calibrate, to close the eyetracker and retrieve the eyetracking file from the Eyelink machine. That SUPA3 file is not out of date and I'm updating it. 

- See https://www.sr-research.com/support/thread-8713.html for explanation of location of Eyelink commands.

### Avoid going through EDF decoding

Long term, instead of using separate machine, communicate directly with eyetracker via pylink, which is part of Psychopy. Actually we are already using modified pylink code to start and stop the eyetracker.
http://www.psychopy.org/api/hardware/pylink.html

A simple example of getting individual data from the eyetracker and a gaze-contingent experiment is [here](https://github.com/ryancotterell/WilsonLab/blob/master/WilsonLab/pylink/eyeTracker.py)
but preferably you'd use ioHub built in Psychopy eye_tracker/run.py which is accessed via Demos->Select Tracker

We had that working in 2023 in [TessHons](https://github.com/alexholcombe/TessHons), but then the student said it wasn't working so she went with getting the EDF file manually off the host PC.

## Preprocessing of the Eyelink (EDF) file

Eyelink now recommends [Pastukhov's eylinkReader R package](https://cran.r-project.org/package=eyelinkReader) for reading EDF files.

See [EDF_file_processing_with_R_eyelinkReader.md](EDF_file_processing_with_R_eyelinkReader.md).

There are other older packages in this [longer list](https://www.sr-research.com/support/thread-7769.html) including https://github.com/dahtah/eyelinker. 

I got that working for TessHons? and trying to get it working for MOT


# OLD pre-eyelinkReader WORKFLOW

Get the EDF file off the eyetracking machine (reboot eyetracking machine, select Windows), it's located in Eyelink:elcl:data and has the name of the participant.

### Fixation report

- On a machine with the Eyelink Dataviewer and dongle driver installed, put the dongle in the USB port and open Eyelink DataViewer.
- choose Analysis->Reports-> and then if you are only interested in when fixation was broken, choose ->Fixation Report.
- Choose which variables you want in the Output Report. In the case of checking whether fixation was broken, they are

RECORDING_SESSION_LABEL TRIAL_LABEL TRIAL_INDEX	CURRENT_FIX_BLINK_AROUND	CURRENT_FIX_X CURRENT_FIX_Y

This results in a text file in wide format, that should name e.g. CF_10Jun2015_12-14EyetrackingReport.txt  

TRIAL_INDEX is better because than TRIAL_LABEL because not spit out the "Trial: " part.

#### summariseEyelinkData.R started by Chris Fajou
summariseEyelinkData.R takes the Fixation Report and goes through all the rows (events reported by Eyelink) for a given trial and works out whether the trial should be discarded.
	
### getting X,Y of every eyetracker sample
- When you want X,Y for each frame, that's called a Sample Report. 
- In the Inspector box, choose Preferences instead of Data and click Data Loading and tick Load Samples. This affects import of EDF files. If you have already imported an EDF file, you will not have the samples. You must re-import.
- To reimport, click on the Data name, e.g. LT_01June2015 and right-click and select Delete. Then go back to operating system and double-click on the EDF file to re-import
- Then generate a report, Analysis->Reports->Sample Report and include LEFT_GAZE_X and LEFT_GAZE_Y and possibly VIDEO_FRAME_INDEX for the time index ("index of the video frame that was visible at the samnple time"), although I'm not certain that's as accurate as TIMESTAMP ("time stamp of the sample (in milliseconds since Eyelink tracker was activated"), LEFT_PUPIL_SIZE.

## Pupillometry

- Eyelink Data Viewer it lets you choose as a field to output Current_Fix_Pupil
- Consider whether the eye is LEFT_IN_BLINK or LEFT_IN_SACCADE because you wouldn't have accurate pupil readings during that time.

## Juno processing pipeline with EDF file
- his C program takes the output of edf2asc.exe (with certain flags, Juno will let us know when he gets additional data)
 (asc2msg) reads the first word on each line and use that as an indicator of what's on that line, basically the XY
- messages carry the stimulus information
- Split it into two files, one for eye position and one for stimulus information. Each file has one row for every 0.5 ms.

- Then, Juno has R program that reads in the two files output from asc2msg, then it looks for certain events which denote the time interval of interest.

## Further automation

EDF2ASC can do at least two different things. Split out different things.

Search the SR Research support forums for EDFVIEW, which is mentioned here 
"A sample analyzer using this toolkit has also been included. This is the source file sac_proc.c which processes the sample data file data.asc. These can be used as a template for your own analyzers."

http://download.sr-support.com/dispdoc/page25.html

EyetrackingR http://www.eyetracking-r.com/
