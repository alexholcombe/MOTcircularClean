widthPix = 1024
heightPix = 768
centralZoneHeightPix = 300 #10
centralZoneWidthPix = 300 #10
myFile<- file.path("dataForTestingOfCode","1223_25J.EDF")
eachT <- EDFsummarise(myFile, widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix)
