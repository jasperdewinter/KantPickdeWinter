# Nowcasting GDP using machine learning methods

This repository provides code to reproduce results in the paper "Nowcasting GDP using machine learning methods" by Dennis Kan, Andreas Pick and Jasper de Winter (2023).

The folder structure is as follows:

* The R-code to run all models and functions to create all outcomes and figures are all in the `Set-ups` folder. Support-code in the sub-folder `Set-ups\Support Code`. 
* The data are in the folder `data`. WARNING: all data are simulated data and will **not** reproduce the results in the Kant, Pick and de Winter (2023) becasue of confidentiaity issues with the data;
* Run file `MAIN.R` line 1--16 to reproduce results for all individual models. Forecast of all individual models are written to `Results\dm` folder and the contributions in Figure 3--6 are written to `Results\graphs`. Auxilliary files writte to `Results\other`;
* Run line `MAIN.R` line 18--20 to reproduce monthly and quarterly RMSFE's in Table 2,3 and 4 and Figure 2. The resulting RMSFE's are in the `Results\rmsfe` folder. Data for Figure 2 written to Excel-file `FIGURE_2xlsx`;
* Run line `MAIN.R` line 22--24 to reproduce monthly and quarterly Diebold Mariano tests n Table 2,3 and 4. The resulting DM-tests against the Dynamic Factor model are in the `Results\dm` folder in the Excel-file `DM_TEST.xlsx`;
* The results in Table 5 and 6 can be reproduced with the Matlab-setup `Table 5&6.m`.