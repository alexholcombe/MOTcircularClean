eyelinkReader
==============

Eyelink now recommends [Pastukhov's R package](https://cran.r-project.org/package=eyelinkReader) for reading EDF files and also has a [longer list](https://www.sr-research.com/support/thread-7769.html). https://github.com/dahtah/eyelinker 
I got that working now for TessHons and trying to get it working for MOT

## Problem on Alex’s machine with eyelinkReader

Executing library(eyelinkReader) after installation give the error that failed to load edfapi, because of a paths problem when a Cpp compilation tries to compile edfapi with eyelinkReader in some way:

"Library not loaded: @rpath/edfapi.framework/Versions/A/edfapi"

"tried: '/Library/Frameworks/edfapi.framework/edfapi.framework/Versions/A/edfapi' (no such file)"

"tried: '/Library/Frameworks/edfapi.framework/edfapi.framework/Versions/A/edfapi' (no such file)"

* Likely relevantly, ~/.Renviron includes:

EDFAPI_LIB="/Library/Frameworks"
EDFAPI_INC="/Library/Frameworks/edfapi.framework/Headers"

Not sure if those are added when installing edfapi from SR Forum

Tried commenting those out (adding “#” at the beginning of the line but that didn't work even when we restarted R. Not sure when the offending path is getting created.

* In the source code for eyelinkReader, namely eyelinkReader/R/zzz.R, one can see lines like 
    library_path <-'/Library/Frameworks/edfapi.framework/'

It's probably that line of code that gets concatenated with another edfapi.framework for some reason. But I don't know why it doesn't reset when I quit R and reload, maybe I need to reinstall the package and then we'll see that the EDFAPI_LIB or _INC has an effect.

