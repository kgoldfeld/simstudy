## Resubmission 2017JUN30

This is a resubmission of version 0.1.3. 

I did get a note from win_build (the same note I received during the last submission for version 0.1.2):

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Keith Goldfeld <Keith.Goldfeld@nyumc.org>'

Possibly mis-spelled words in DESCRIPTION:
  MCAR (16:17)
  Missingness (15:24)
  NMAR (16:28)
  covariates (12:27)
  
These are all spelled correctly.

I am getting note with respect to registering the Rcpp function. I've take all the steps to get rid of the note, and looking at the object, it appears to contradict the note, in that it is registered.

$name
[1] "simstudy_matMultinom"

$address
<pointer: 0x000000000a160c80>
attr(,"class")
[1] "RegisteredNativeSymbol"

$dll
DLL name: simstudy
Filename: C:/R Packages/simstudy/libs/x64/simstudy.dll
Dynamic lookup: FALSE

$numParameters
[1] 1

attr(,"class")
[1] "CallRoutine"      "NativeSymbolInfo"
