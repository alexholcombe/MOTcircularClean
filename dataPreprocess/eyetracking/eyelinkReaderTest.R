#To get the eyetracking file that this file processes,

#To get eyelinkReader to work, did following:
#library("devtools")
#install_github("alexander-pastukhov/eyelinkReader", dependencies=TRUE)
#Executed usethis::edit_r_environ() to edit the .Renviron file and add the two lines on the eyelinkReader github page:
#EDFAPI_LIB="/Library/Frameworks"
#EDFAPI_INC="/Library/Frameworks/edfapi.framework/Headers"

library(eyelinkReader)

EDF_example <- "/Users/alex/Documents/attention_tempresltn/multiple_object_tracking/newTraj/MOTcircular_repo/dataRaw/circleOrSquare_twoTargets/AM/AM_11Jun2015_11-51.EDF"
gaze <- read_edf(EDF_example)

if (length(gaze)) {
  cat('Success!')
} #Worked 31 Aug 2023

#March 2023, somehow the path is slightly screwed so library(eyelinkReader) fails, with redundant path attempt, e.g.
#tried: '/Library/Frameworks/edfapi.framework/edfapi.framework/Versions/A/edfapi' (no such file),
#Just need to drop one of the edfapi.frameworks

#View(gaze$saccades)

plot(gaze, trial = 1, show_fixations = TRUE, show_saccades = TRUE)

#Need to go through all trials and check largest fixation deviation from center,
# and merge with PsychoPy data file.

#To do this, I could either set up an area of interest maybe, or manually