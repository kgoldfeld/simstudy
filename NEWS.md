# simstudy 0.1.1

* This is the first submission of simstudy, so there is no news yet!

# simstudy 0.1.2

* Fixed index variable issue related to generating categorical data 
* Fixed index variable issue related to generating longitudinal data
* Fixed issue that arised When creating categorical variable in first field
* Increased speed required to generate categorical data with large sample sizes
* Categorical data can now accomodate probabilities condition on covariates
* Fix: package data.table 1.10.0 broke genMissDataMat. genMissDataMat has been updated.

# simstudy 0.1.3

* Modified "nonrandom" data generation to allow "log"" and "logit"" link options.
* Added function genCorGen - generate a new data.table with correlated data from various distributions.
* Added function addCorData - add correlated data from various distributions to existing data.tables.

# simstudy 0.1.4

* Added error check to verify that specified distributions are valid
* Added function genFactor - converts an existing (non-double) field in a data.table to a factor
* Added function genDummy - creates dummy variables from an integer or factor field in a data.table
* Added function defCondition - define distribution conditional on existing fields
* Added function defReadCond - read in conditional definitions from external csv file
* Added function addCondition - genaration data based on conditional definition

# simstudy 0.1.5

* Added uniform integer distribution (uniformInt)
* Added negative binomial distribution (negBinomial)
* Added exponential distribution (exponential)
* Added function delColumns - deletes one or more columns from data.table

# simstudy 0.1.6

* Fixed function genSurv
* Added spline generating functions

# simstudy 0.1.7

* Added function genOrdCat - creates ordinal categorical data
* Added function genFormula - creates a linear formula in the form of a string
* Added function updateDef - modify existing data definition table (to be used in genData())
* Added function updateDefData - modify existing data def table (to be used in addColumns())

# simstudy 0.1.8

* Fixed function updateDef
* Fixed bug in internal function genbinom
* Added function genCorFlex - generate correlated data from variables that have different marginal distributions
* Added function genCorFlex - generate correlated data from variables that have different marginal distributions, can be dependent on previously defined data

# simstudy 0.1.9

* Added function catProbs - to be used to generate categorical data
* Added binomial distribution
* Added ability to specify formula in variance
* Added function genMultiFac - generates multi-factorial design data
* Added function addMultiFac - adds multi-factorial design data
* Added function iccRE - generates required random effect variance for specified intra-class coefficients (ICCs)
* Fixed bug in function genCorFlex
* Fixed bug in numerous functions related to error checking and scoping
* Fixed bug in function addCondition

