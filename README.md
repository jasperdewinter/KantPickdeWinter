# Nowcasting GDP using machine learning methods

This repository provides code to reproduce results in the paper "Nowcasting
GDP using machine learning methods" by Dennis Kant, Andreas Pick (EUR) and
Jasper de Winter (DNB) that is forthcoming in AStA Advances in Statistical 
Analysis. The scripts are most easily ran in Rstudio following
these steps: Download or clone the repository and double-click AstA.proj to
open the project in RStudio. Next, open Main and run the models and/or
calculate DM-tests and report RMSFEs.

* The outcome of all Figures and Tables are collected in the directory
  `Results`. The folders `dm` and `rmsfe` contain the outcomes in Table 2, 3
  and 4 of our paper and show the outcome of the Diebold-Mariano test and
  RMSFE, respectively. The folder `fcst` contains the raw backcasts, nowcasts
  and forecasts for all models underlying these results. The folder `graphs`
  contains pdf-files for Figure 3, 4 and 5;
* The setups in the folder `Set-ups_public`can be used to reproduce all
  modeloutcomes and figures in our paper;
* Run R-file `MAIN.R` to reproduce all outcomes in Table 2, 3 and 4; 
* Adjust line 16 of the R-file `MAIN.R` to produce outcomes for one of the models in our
  paper (adjust index in line 16), i.e. AR(1) model, `AR`, Random Walk
  `RW`, Factor-augmented Mixed Data Sampling regression `MIDAS-F`, Least
  Absolute Shrinkage and Selection Operator`LASSO`, Elastic Net `EN`, Random
  Subspace regression `RS`, Random Projections `RP`, Random Forest`RF`. 
* the backcasts, nowcasts and forecasts of the models are written to the
  directory `Results_public\fcst`. Figures are written to
  `Results_public\graphs`. Auxiliary files written to
  `Results_public\other`;
* Run the Matlab file `TABLE_5_7.m` to reproduces Table 5, 6 and 7 of our paper;
* For confidentiality reasons some series had to be removed from the
  data-file (notably all data-series in Table A.2 that only have codes and no
  links. The forecasts, RMSFEs and contributions will therefore deviate from
  the outcomes in the paper);
* The text-file `SessionInfo.txt`contains information on the R-(packages) and
  Matlab versions used to estimate the models.

DISCLAIMER: The code is provided "as is". The authors make no assertions as to
its performance or effects if run, provides no warranties of any kind, and
disclaims any implied warranties, express or implied, including but not
limited to the warranties of merchantability, fitness for a particular
purpose or non infringement.