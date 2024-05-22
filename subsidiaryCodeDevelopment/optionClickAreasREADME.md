mouseChoiceArea = ballStdDev * 0.2 #debugAH #*0.8  # origin =1.3

mouseToler = mouseChoiceArea + optionSet*mouseChoiceArea/4 #6.  #deg visual angle?

clickableRegion = *mouseToler*
clickedRegion  radius=mouseChoiceArea.  Later set to *mouseToler*
optionChosenCircle radius=mouseChoiceArea

Try to refactor to use radii and maxNumObjects, instead of ballStdDev