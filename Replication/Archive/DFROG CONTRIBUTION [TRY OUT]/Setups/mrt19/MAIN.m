%__________________________________________________________________________
%%                             MAIN
%
%                         Jasper de Winter
%                      (De Nederlandsche Bank)
%
%                           March 26,2019
%
% 1. The program runs all the models, AFTER the data creation process is
%    complete.
% 2. Check all the estimation options in the file ESTIMATION before
%    running this file
%__________________________________________________________________________

%__________________________________________________________________________
%%                         V. NETHERLANDS

%__________________________________________________________________________
%__________________________________________________________________________
% 1. Process Data
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'DFM_ECB';
% PROCESS_DATA;%  Comment, and use data from ..\Replication\DFROG\Data\DNB\data_mrt_19.mat
Folder = pwd;
cd(Folder);

%__________________________________________________________________________
% 2. Models
clear; clc; Country='NL'; CCode='nl_'; P.Model = 'DFM_ECB'; DFROG;            % Estimate DFM (Banbura and Runstler, 2011)
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'RUN_SFM'; FACT_STAT;      % Estimate SFM (Agostino & Giannone, 2005)
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'RUN_BEQ'; BEQ;            % Estimate unweighted quarterly bridge equations
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'RUN_BEQ'; BEQ_RMSE;       % Estimate RMSFE weighted Bridge Equations
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'RUN_QVR'; QVAR;           % Estimate unweighted quarterly VAR model
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'RUN_QVR'; QVAR_RMSE;      % Estimate RMSFE weighted quarterly VAR model
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'RUN_RNW'; QBM;            % Estimate Random Walk Model
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'RUN_MEN'; QBM;            % Estimate Full Sample Mean
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'RUN_MER'; QBM;            % Estimate Recursive Mean
% clear; clc; Country='NL'; CCode='nl_'; P.Model = 'RUN_AAR'; QBM;            % Estimate Autoregressive Model