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
