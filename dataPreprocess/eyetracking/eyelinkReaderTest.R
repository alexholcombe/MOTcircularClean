#To get the eyetracking file that this file processes,

#To get eyelinkReader to work in 2023, did following:
#library("devtools")
#install_github("alexander-pastukhov/eyelinkReader", dependencies=TRUE)
#Executed usethis::edit_r_environ() to edit the .Renviron file and add the two lines on the eyelinkReader github page:
#EDFAPI_LIB="/Library/Frameworks"
#EDFAPI_INC="/Library/Frameworks/edfapi.framework/Headers"

#To get eyelinkReader to work in 2024 on Mac, we did
# devtools::install_github("alexholcombe/eyelinkReader", dependencies=TRUE, build_vignettes=TRUE)
# See EDF_file_processing_with_R_eyelinkReader.md

library(eyelinkReader)


EDF_example <- file.path("dataForTestingOfCode", "A20b.EDF") # "A421.EDF" #"/Users/alex/Documents/attention_tempresltn/multiple_object_tracking/newTraj/MOTcircular_repo/dataRaw/circleOrSquare_twoTargets/AM/AM_11Jun2015_11-51.EDF"
gaze <- read_edf(EDF_example)

if (length(gaze)) {
  cat('Success!')
} #Worked 31 Aug 2023, then broke until Alex's fix May 2024

#View(gaze$saccades)

eyelinkReader::plot(gaze, trial = 1, show_fixations = TRUE, show_saccades = TRUE)
#Need to go through all trials and check largest fixation deviation from center,
# and merge with PsychoPy data file.

#eyelinkReader also allows using ggplot for more control
# extracting fixations and saccades for the first trial
fixations <- gaze$fixations[gaze$fixations$trial == 1, ]
saccades <- gaze$saccades[gaze$saccades$trial == 1, ]

ggplot() +
  # enforce same scaling, as on the screen
  coord_equal(expand=FALSE) +
  
  # define screen limits and INVERSE y-axis to match Eyelink
  scale_x_continuous(name = "x", limits = gaze$display_coords[c(1, 3)]) +
  scale_y_reverse(name = "y", limits = gaze$display_coords[c(4, 2)]) +
  
  # draw fixations as circles
  geom_point(data = fixations, aes_string(x = "gavx", y = "gavy", size = "duration"), alpha=0.3) +
  
  # draw saccades as line segments
  geom_segment(data = saccades, aes_string(x = "gstx", y = "gsty", xend = "genx", yend = "geny", color = "sttime_rel")) +
  
  # better legend titles
  labs(size = "Fixation duration [ms]",
       color = "Saccade onset [ms]")

#To do this, I could either set up an area of interest maybe, or manually



