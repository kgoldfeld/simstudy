---
title: 'simstudy: Illuminating research methods through data generation'
tags:
  - R
  - statistics
  - data-simulation
  - statistical-models
  - data-generation
authors:
  - name: Keith Goldfeld
    orcid: 0000-0002-0292-8780
    affiliation: 1 
  - name: Jacob Wujciak-Jens
    orcid: 0000-0002-7281-3989
    affiliation: 2
affiliations:
 - name: NYU Groosman School of Medicine.
   index: 1
 - name: Independent Researcher
   index: 2
date: 18 October 2020
bibliography: simstudy.bib
---

# Summary

The `simstudy` package is collection of functions that allow users to generate
simulated data sets in order to explore modeling techniques or better understand
data generating processes. The user defines the distributions of individual
variables, specifies relationships between covariates and outcomes, and
generates data based on these specifications. The final data sets can represent
randomized control trials, repeated measure designs, cluster randomized trials,
or naturally observed data processes. Other complexities that can be added
include survival data, correlated data, factorial study designs, step wedge
designs, and missing data processes.

Simulation using `simstudy` has two fundamental steps. The user (1) **defines**
the data elements of a data set and (2) **generates** the data based on these
definitions. Additional functionality exists to simulate observed or randomized
**treatment assignment/exposures**, to create **longitudinal/panel** data, to
create **multi-level/hierarchical** data, to create datasets with **correlated
variables** based on a specified covariance structure, to **merge** datasets, to
create data sets with **missing** data, and to create non-linear relationships
with underlying **spline** curves.

The overarching philosophy of `simstudy` is to create data generating processes
that mimic the typical models used to fit those types of data. So, the
parameterization of some of the data generating processes may not follow the
standard parameterizations for the specific distributions. For example, in
`simstudy` *gamma*-distributed data are generated based on the specification of
a mean $\mu$ (or $log(\mu)$) and a dispersion $d$, rather than shape $\alpha$
and rate $\beta$ parameters that more typically characterize the *gamma*
distribution. When we estimate the parameters, we are modeling $\mu$ (or some
function of $(\mu)$), so we should explicitly recover the `simstudy` parameters
used to generate the model - illuminating the relationship between the
underlying data generating processes and the models.

# Statement of need 

Empiricism and statistical analysis are cornerstones of scientific research
but they can crumble if used incorrectly. Choosing the right methodology for the
hypothesis and expected data is crucial for useful, valid results. Data
simulated with `simstudy` under the assumptions derived from the hypothesis
enables researchers to test and refine their analysis methodology without the
need for time-intensive, expensive pre-tests. Additionally data generated with
`simstudy` can be used in generalised, theoretical simulation studies to further
the field of methodology.

There are several `R`-packages that allow for data generation under different
assumptions. Most of these packages have a very narrow scope that focuses on a
certain kind a data to generate, like `ICCbin` [@hossain2017], `BinNonNor`
[@inan2020] and `genSurv`[@meira-machado2014]. Some do not seem to be actively
maintained [@hofert2016;@chan2014;@alfons2010;bien2016:], which can cause
compatibility issues. Some target specific fields and their needs like the
psychology focused `psych` Package [@revelle2020] or the `conjurer` Package
[@macherla2020] that provides methods to generate synthetic customer data for
industry use. `simstudy` is unique with it's philosophy of data generating
processes that mimic the models used in analysis and the possibility to generate
a wide range of complex data through these processes. The `SimDesign` Package
[@chalmers2020] and the related `MonteCarlo` Package [@leschinski2019] follow a
similar line of thought but focus on the analysis and summary of the simulated
data.

`simstudy` has been used in a variety of fields for theoretical exploration of
research methodology
[@anderson2019;@kirasich2018;@krzykalla2020;@liu2019;@nickodem2020;@thoya2018;@wang2020;@elalili2020],
power calculation for trials [@wei2019] and other simulation tasks supporting
researchers in their quest for knowledge
[@forthun2020;@horry2020;@renson2017;chukwu2019].

# Acknowledgements

We acknowledge contributions from James Balamuta, Michael Bradley,  Gertjan
Verhoeven. For the generation of multivariate binary data the algorithm by
@emrich1991 is used.

# References