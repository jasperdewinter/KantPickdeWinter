# Nowcasting GDP using machine learning methods

This repository provides code to reproduce results in the paper "Nowcasting GDP using machine learning methods" by Dennis Kant, Andreas Pick and Jasper de Winter. The code is provided "as is". The authors make no assertions as to its performance or effects if run, provides no warranties of any kind, and disclaims any implied warranties, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose or noninfringement. 

The scripts are most easily ran in Rstudio following these steps: Download or clone the repository and double-click AstA.proj to open the project in RStudio. Nxt, open Main and run the models and/or calculate DM-tests and report RMSFEs.

The folder structure is as follows:
* The R and Matlab code to reproduce all tables and figures are in the folder `Results` 
* The R-code to run all models and functions to create all outcomes and figures are all in the `Set-ups` folder. Support-code in the sub-folder `Set-ups\Support Code`. 
* The data are in the folder `data`. For confidentiality reasons some series had to be removed from the data-file (notably all data-series in Table A.2 that only have codes and not links). The forecasts, RMSFEs and contributions will therefore deviate from the outcomes in the paper;
* Run file `MAIN.R` line 1--16 to reproduce results for all individual models. Forecast of all individual models are written to `Results_public\fcst` folder and the contributions in Figure 3--6 are written to `Results_public\graphs`. Auxilliary files writte to `Results_public\other`;
* ADJUST!! Run line `MAIN.R` line 18--20 to reproduce monthly and quarterly RMSFE's in Table 2,3 and 4 and Figure 2. The resulting RMSFE's are in the `Results\rmsfe` folder. Data for Figure 2 written to Excel-file `FIGURE_2xlsx`;
* ADJUST!! Run line `MAIN.R` line 22--24 to reproduce monthly and quarterly Diebold Mariano tests n Table 2,3 and 4. The resulting DM-tests against the Dynamic Factor model are in the `Results\dm` folder in the Excel-file `DM_TEST.xlsx`;
* ADJUST!! The results in Table 5 and 6 can be reproduced with the Matlab-setup `Table 5&6.m`.
* File SessionInfo.txt contains information on versioning of the used R-software and R-packages.
