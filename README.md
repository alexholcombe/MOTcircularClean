Multiple object tracking experiment
==============
This program is super-complicated, legacy of many different papers using tracking. I don't recommend you use it without my guidance. My other repositories, such as attentional-blink, are more user-friendly.

In the long term, want to set up exchange of objects among rings. Necessary to conduct an identity-tracking experiment. Can also push harder on speed limit that way.

## Analysis of this data outside of this repo

When I did it with Bayesian I switched to [this repo](https://github.com/alexholcombe/brms_psychometric_variableGuessRate_lapseRate), but didn’t get farther than model recovery, and then I made this [repo](https://github.com/alexholcombe/analyseRoudaiaHolcombeMOT) I think when I was trying to transition to real data, including joint analysis the Roudaia lab data.
When I did it with Bayesian I switched to [this repo], but didn’t get farther than model recovery



## Analysis workflow inside this repo

*	In the [dataPreprocess](dataPreprocess) folder, its Rproj loadAnonymiseSaveData.R is used to match up Psychopy datafiles with EDF files, and possibly combine multiple sessions together.
*	the [analysis](analysis) directory with its Rproj is used to analyze the data.


## Eyetracking

### Eyetracking data analysis 

New way: Use eyelinkReader R package, see [notes](dataPreprocess/eyetracking/EDF_file_processing_with_R_eyelinkReader.md)

Old way: For [Fixation Report](https://docs.google.com/document/d/1o3IirKJTU_yfOrlNHWl6cmx1fj1j6vFzhXw-UFje_jw/edit) analysis from EDF files (instructions about making the fixation report [here](https://github.com/alexholcombe/MOTcircularClean/tree/master/dataPreprocess/eyetracking)), see [this code](https://github.com/alexholcombe/MOTcircular/tree/master/dataPreprocess)

Drift correction: will need to be done at the analysis stage because we are not able to re-calibrate the eyetracker due to it not being able to draw to the same graphicsEnvironment even though it's supposed to be able to. So we'd have to close Psychopy grahpics environment and open an eyetracker one for each calibration.

### Eyetracking problems Nov 2023 - Jan 2024

On Alex's Mac, it never connects to the eye tracker, like an ethernet problem. Eyelink provides some advice on how to configure the IP address to remedy that, which we reprint [here](https://docs.google.com/document/d/1o3IirKJTU_yfOrlNHWl6cmx1fj1j6vFzhXw-UFje_jw/), which I haven't tried.


While Eyelink's example program, picture.py in their EyeLink Developers Kit, works on some machines for Eyelink and Psychopy sharing a graphics environment (so both Eyelink can draw its calibration targets and Psychopy can draw things), we never got that to work with MOTcircular.py, even though it was working years ago - since then, Eyelink has updated their Developers Kit. 

So, we instead have the Eyelink quit drawing (pylink. ) after initial calibration, even though it does work on Windows, you just have to tab between windows.

On Joshua's Win laptop, we had some sounds crashing errors.
	* the sounds crash if I use the old genv way of allowing the Eyelink to draw the calibration targets, but not if I use the             pylink.openGraphics()  
	* Seemed to be fixed after Josh changed the sound encoding of all sound files to be at the same sampling rate.

On Joshua's Win laptop, if oldSchoolWayOfEyelinkDrawingToScreen=False, then Eyelink successfully starts drawing the calibration targets in a separate graphics window you need to tab betwen.

## Subsidiary experiments

- Old versus young

### In [old repository](https://github.com/alexholcombe/MOTcircular), separate from this new Clean repo 

- [Centered vs. peripheral](https://github.com/alexholcombe/MOTcircular/tree/master/experiment_specific/rps_limit), including data

- [Square traj, modulations](https://github.com/alexholcombe/MOTcircular/blob/master/experiment_specific/rps_limit/square.md), including data

- [Transient attention abandoned with Ingrid and Arni](https://github.com/alexholcombe/MOTcircular/tree/master/experiment_specific/transient_attention)
