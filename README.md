Multiple object tracking experiment
==============
This program is super-complicated, legacy of many different papers using tracking. I don't recommend you use it without my guidance. My other repositories, such as attentional-blink, are more user-friendly.

In the long term, want to set up exchange of objects among rings. Necessary to conduct an identity-tracking experiment. Can also push harder on speed limit that way.

### Issues
-Improve anonymisation algorithm


## Eyetracking Nov 2023

Try drift correction and try without it

On Alex's Mac, it never connects to the eye tracker, like an ethernet problem.

On Joshua's Win laptop, the sounds crash if I use the old genv way of allowing the Eyelink to draw the calibration targets, but not if I use the             pylink.openGraphics()  

On Joshua's Win laptop, if oldSchoolWayOfEyelinkDrawingToScreen=False, then Eyelink successfully starts drawing the calibration targets, but it never goes back to drawing the experiment stimuli and it seems to be a separate graphics window, as revealed by 

## Eyetracking


To the self-test of eyetrackingCode/EyeLinkCoreGraphicsPsychoPyHolcombeLab.py, I've tried to add a trial. Don't think that version is tested yet.

For fixation report analysis from EDF files, see https://github.com/alexholcombe/MOTcircular/tree/master/dataPreprocess file

## Subsidiary experiments

- Old versus young

### In [old repository](https://github.com/alexholcombe/MOTcircular), separate from this new Clean repo 

- [Centered vs. peripheral](https://github.com/alexholcombe/MOTcircular/tree/master/experiment_specific/rps_limit), including data

- [Square traj, modulations](https://github.com/alexholcombe/MOTcircular/blob/master/experiment_specific/rps_limit/square.md), including data

- [Transient attention abandoned with Ingrid and Arni](https://github.com/alexholcombe/MOTcircular/tree/master/experiment_specific/transient_attention)