library(eyelinkReader)
fexample <- file.path("dataForTestingOfCode", "E463wontOpen.EDF") # "A421.EDF" #"/Users/alex/Documents/attention_tempresltn/multiple_object_tracking/newTraj/MOTcircular_repo/dataRaw/circleOrSquare_twoTargets/AM/AM_11Jun2015_11-51.EDF"

#See what goes wrong by calling actual compiled C function

consistency = 'check consistency and report'
import_events = TRUE
import_recordings = TRUE
import_samples = FALSE
sample_attributes = NULL
start_marker = 'TRIALID'
end_marker = 'TRIAL_RESULT'
import_saccades = TRUE
import_blinks = TRUE
import_fixations = TRUE
import_variables = TRUE
verbose = TRUE
fail_loudly = TRUE

# figuring out which sample attributes to import, if any
sample_attr_flag <- eyelinkReader:::logical_index_for_sample_attributes(import_samples, sample_attributes)
import_samples <- sum(sample_attr_flag) > 0

# converting consistency to integer constant that C-code understands
requested_consistency <- eyelinkReader:::check_consistency_flag(consistency)
  
edf_recording <- eyelinkReader::read_edf_file(fexample,
                                              requested_consistency,
                                              import_events,
                                              import_recordings,
                                              import_samples,
                                              sample_attr_flag,
                                              start_marker,
                                              end_marker,
                                              verbose)
#This is the output:

#loadEvents = 1
#error reading edf file 7a r = 1 
#End of file Exception