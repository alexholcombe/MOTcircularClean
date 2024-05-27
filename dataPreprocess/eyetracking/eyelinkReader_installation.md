eyelinkReader
==============

Eyelink now recommends [Pastukhov's R package](https://cran.r-project.org/package=eyelinkReader) for reading EDF files and also has a [longer list](https://www.sr-research.com/support/thread-7769.html). https://github.com/dahtah/eyelinker 

As-is was working for Tess' Mac, but changes to Macs may have broken the package, but I created a working fork.

On Josh's Windows computer, it kept saying edfapi was not installed or something like that but re-installing never fixed that.

## Fixing problem on Alex’s Mac and Jye's Mac with eyelinkReader

Executing library(eyelinkReader) after installation give the error that failed to load edfapi, because of a paths problem when a Cpp compilation tries to compile edfapi with eyelinkReader in some way:

"Library not loaded: @rpath/edfapi.framework/Versions/A/edfapi"

"tried: '/Library/Frameworks/edfapi.framework/edfapi.framework/Versions/A/edfapi' (no such file)"

"tried: '/Library/Frameworks/edfapi.framework/edfapi.framework/Versions/A/edfapi' (no such file)"

* Likely relevantly, ~/.Renviron includes:

EDFAPI_LIB="/Library/Frameworks"
EDFAPI_INC="/Library/Frameworks/edfapi.framework/Headers"

Not sure if those are added when installing edfapi from SR Forum

Tried commenting those out (adding “#” at the beginning of the line but that didn't work even when we restarted R. I realized later that those aren't used in the offending line or its associates:

* In the source code for eyelinkReader on line 70 of eyelinkReader/R/zzz.R, one can see: 
    library_path <-'/Library/Frameworks/edfapi.framework/'

It's that line of code that gets concatenated with another edfapi.framework somewhere. 

### Fixed it on my fork on Github of eyelinkReader

Created a fork of Pastukhov's github repo, then change the zzz.R file.
Change line 70 of zzz.R  to   library_path <-'/Library/Frameworks/'

Now can install from my fork using devtools: 

devtools::install_github("alexholcombe/eyelinkReader", dependencies=TRUE, build_vignettes=TRUE)

May need to re-install R (but not RStudio) to prevent lazy-loading error within RStudio.

Test it with:

``` gaze <- eyelinkReader::read_edf('dataForTestingOfCode/A20b.EDF') ```

and

``` browseVignettes('eyelinkReader') ```

### 22 May on Alex's Mac

    1. Uninstall R first, both the app and the framework, [using](https://stackoverflow.com/questions/24981581/uninstall-r-from-mac-osx-10-9-2):

'''
sudo rm -Rf /Library/Frameworks/R.framework /Applications/R.app \
   /usr/local/bin/R /usr/local/bin/Rscript
'''

    2. I deleted RStudio
    3. I didn't delete ~/.Renviron but everything in it is commented out from my session with STeve
    3. I downloaded R 4.0 
    3. I created an ~.Renviron with the contents recommended by Pastukhov for MacOS
    3. install.packages("eyelinkReader").
    3. library('eyelinkReader') gives the usual error, and also on Jye's machine, and also with install_github method instead of CRAN

I confirmed that the EDFIAPI_LIB and EDFAPI_INC paths recommended by eyelinkReader github page

I can see where zzz.R, for Darwin systems uses EDFAPI_INC but never uses EDFAPI_LIB, even though it adds that as a possible path in Windows!

Changing line 70 of zzz.R to eliminate the terminal edfapi.framework and will recompile using install.packages(path_to_file, repos = NULL, type="source")

install.packages('/Users/alex/Downloads/eyelinkReader-master', repos = NULL, type="source", build_vignettes = TRUE))

That claimed to work but when loading library gave this error:

Error: package or namespace load failed for ‘eyelinkReader’ in get(Info[i, 1], envir = env):
 lazy-load database '/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/eyelinkReader/R/eyelinkReader.rdb' is corrupt

I found [this example](https://stackoverflow.com/questions/63855025/cant-load-r-packages-after-installation) of when that error occurs, which claims that R and Rstudio were using different versions. And indeed, when I exeuctied 'library('eyelinkReader')' from R.app, it works!  

So then if I do part of what the StackOverflow says and uninstall R from the Terminal and then re-download it and install all the necessary packages, it now works in RStudio!

Only thing is that vignettes are not working

### environment variables can they trump to fix the problem?

EDFAPI_INC needs to be the path to C header files edf.h, edf_data.h, and edftypes.h

EDF_LIB is path to EDF API framework, which might be the problem, that zzz.R has /Library/Frameworks/edfapi.framework/ instead of omitting that final directory, so if we do that it might work