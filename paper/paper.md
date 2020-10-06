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
date: 13 October 2020
bibliography: paper.bib
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
To Do:
* A clear Statement of Need that illustrates the research purpose of the software.
* A list of key references, including to other software addressing related
  needs.  
* Mention (if applicable) a representative set of past or ongoing research projects using the software and recent scholarly publications enabled by it.
* Acknowledgement of any financial support.

# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$

You can also use plain \LaTeX for equations
\begin{equation}\label{eq:fourier}
\hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
\end{equation}
and refer to \autoref{eq:fourier} from text.

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Fenced code blocks are rendered with syntax highlighting:
```python
for n in range(10):
    yield f(n)
```	

# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References