Analysis of Multiple object tracking data
==============

With R,
doAllAnalyses.R analyses all the data, after it's been anonymised by dataPreprocess

This uses some functions copied from the modelfree package.

Old temporal frequency versus speed analyses can be found in https://github.com/alexholcombe/speed-tf-VSS14

I think with brglm, it doesn't support custom link function, so I scale the y's from guessing->(1-lapseRate) to 0 to 1. But then some of the data is actually below guessing and above 1-lapseRate, and because in logistic regression the y's can't be greater than 1 or below 0, I truncate those, which is not good. So need to switch to non-brglm so can use custom link function, and need to then use logit_link_private.

But with brglm2, brglmFit() now takes custom link functions. https://cran.r-project.org/web/packages/brglm2/news/news.html

Example of custom link function https://stats.stackexchange.com/questions/617389/custom-link-function-needed-for-generalised-linear-model-in-r

Turning any link function into a custom link function with upper bound, https://stats.stackexchange.com/questions/617389/custom-link-function-needed-for-generalised-linear-model-in-r

A problem is that it's not constrained binomial regression, but we want to constrain it such that at speed = 0, %correct is 1-lapseRate. Unfortunately brglm doesn't provide constrained fit, not even in brglm2.
I suppose if we took the logarithm of the speed before doing the fit, it would do that, because log(0) = -infinity