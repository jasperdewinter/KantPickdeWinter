%__________________________________________________________________________
%%               OPTIONS FOR ESTIMATION OF VARIOUS MODELS
%
%                         Jasper de Winter
%                      (De Nederlandsche Bank)
%
% March 26, 2019, rerun March 6, 2023, September 25 2023
%
% The program gives all options for the estimation of DFROG. It is
% obligatory to adjust the month of the datafile as you estimate a new
%
%__________________________________________________________________________
% Obligatory options
%   
% MontH                 Set the right month;
% T.Now                 Month on which data were downloaded + three years
%                       (e.g: data downloaded on January 2012, then
%                       T.Now = [2015 1])
% Base_Dir              Adjust Base_Dir for use on laptop, work or home
%__________________________________________________________________________
% Optional Deep parameters Dynamic Factor Model
%
% P.r, P.q, P.p,        Adjust parameters of the dynamic factor model
%
% P.gr                  P.gr=1, Calculate factors per pre-defined group,
%                       P.gr=0, Calculate factors for total
%
% P.wgt                 P.wgt=1, Calculate weights and contributions.
%                       P.wgt=1, Do not calculate
%
% P.fac                 P.fact=1, Save all factors/eigenvectors/eigenvalues
%                       P.fact=0, Do not save
%
% P.yoy                 P.yoy=1, forecast for quarterly y-o-y mutations
%                       P.yoy=0, forecast for quarterly q-o-q mutations
%__________________________________________________________________________

%__________________________________________________________________________
%
%%           I. M O D E L  T Y P E S  A N D  P A R A M E T E R S
%__________________________________________________________________________

%__________________________________________________________________________
% 1. Model parameters Dynamic Factor Model
if P.Model == 'DFM_ECB'                                                     % Run DFM_ECB

% SETTING JANUARY 2016    
    P.crit  = 0;                                                            % use below list P.r, P.q and P.p;
    P.r     = 1;                                                            % Nr of static factors
    P.q     = 1;                                                            % Nr of dynamic factors (q < r)
    P.p     = 2;                                                            % Nr of lags in VAR of ft?s
    P.cutE  = 3;                                                            % Nr of obs to cut at eos for estimation
%     P.crit  = 0;                                                            % use below list P.r, P.q and P.p;
%     P.r     = 2;                                                            % Nr of static factors
%     P.q     = 2;                                                            % Nr of dynamic factors (q < r)
%     P.p     = 2;                                                            % Nr of lags in VAR of ft?s
%     P.cutE  = 3;                                                            % Nr of obs to cut at eos for estimation
% SETTING JANUARY 2016    
%     P.crit  = 0;                                                         % use below list P.r, P.q and P.p;
%     P.r     = 4;                                                         % Nr of static factors
%     P.q     = 3;                                                         % Nr of dynamic factors (q < r)
%     P.p     = 3;                                                         % Nr of lags in VAR of ft?s
%     P.cutE  = 3;                                                         % Nr of obs to cut at eos for estimation
% 
%     P.crit      = 0;                                                       % Method for spec the nr of stat factors r
%     P.r         = 1:6;
%     P.q         = 1:6;
%     P.p         = 1:6;
%     P.cutE      = 3;     
    
    P.gr    = 0;                                                            % P.gr=1, Calculate factors per pre-defined group,
    % P.gr=0, Calculate factors for total
    P.wgt   = 1;                                                            % P.wgt=1,Calculate weights and contributions.
    
    P.fac   = 0;                                                            % Visually check for # of factors and save forecast of
    % X-matrix to compare to new forecast
    %__________________________________________________________________________
    % 2. Model parameters Static Factor Model
elseif P.Model == 'RUN_SFM'                                                 % Run SFM
    Model = P.Model;                                                        % Define Model
    P.crit  = 0;                                                            % Method for specifying the nr of factors r
    % = 0 use below list P.r (no estimation)
    % = 2 estimate from Infocrit (Bai & Ng, 2002)
    % for further options see proc Estim_SPC
    P.r     = 3;                                                            % Nr of static factors
    % (you may enter a column vector)
    % P.crit > 0 => max(P.r) is taken as max
    %               value for selection
    
    P.k     =  2;                                                           % Skip series with publication lag > k
    % (only for P.shift = 1 or 2)
    P.shift =  2;                                                           % Method for shifting (see 'help BalanceX')
    % 1 = Shift series by nr of nan
    % 2 = Forecast nan from univariate AR(p)
    % 3 = EM algorithm
    %__________________________________________________________________________
    % 3. Model parameters for (un)weighted quarterly bridge equations
elseif  P.Model == 'RUN_BEQ'
    Model = P.Model;
    
    P.IC     = 'SIC';                                                       % Lag length sel in BEQ ('AIC'|'SIC'|'FIX')
    P.lags   =     4;                                                       % Nr of lags in bridge equation
    %  Max nr of lags for ('AIC'|'SIC')
    
    P.Mon    =  'AR';                                                       % Fcst for monthly data ('VAR'|'AR' | 'RW')
    P.V.IC   = 'SIC';                                                       % Lag length sel in VAR ('AIC'|'SIC'|'FIX')
    P.V.lags =     4;                                                       % (Max) nr of lags in monthly VAR
    P.cutE   =     3;                                                       % Nr of obs to cut at eos for estimation
    
    %__________________________________________________________________________
    % 4. Model parameters for (un)weighted quarterly VAR
elseif  P.Model == 'RUN_QVR'
    Model = P.Model;
    P.IC    = 'SIC';                                                        % Lag length sel in VAR ('AIC'|'SIC'|'FIX')
    P.lags  =  4;                                                           % Fixed nr of lags in VAR ('FIX')       or
    % Max   nr of lags for    ('AIC'|'SIC')
    % Note: for euro area data, startF = [1998 1]
    % does not leave enough obs for P.lags > 4
    
    %__________________________________________________________________________
    % 5. Model parameters for random walk
elseif P.Model == 'RUN_RNW'
    Model      = P.Model;
    Par        = [];
    fcstH      = 4;                                                         % Forecast horizon (quarters)
    %__________________________________________________________________________
    % 6. Model parameters for full sample mean
elseif P.Model == 'RUN_MEN'                                                 % Full sample mean
    Model      = P.Model;
    Par        = [];
    fcstH      = 4;                                                         % Forecast horizon (quarters)
    
    %__________________________________________________________________________
    % 7. Model parameters for recursive mean
elseif P.Model == 'RUN_MER'                                                 % Recursive Mean
    Model      = P.Model;
    Par        = [];
    fcstH      = 4;                                                         % Forecast horizon (quarters)
    %__________________________________________________________________________
    % 8. Model parameters for autoregressive model
elseif P.Model == 'RUN_AAR'                                                 % Autoregressive Model
    Model      = P.Model;
    lags       = 1;                                                         % Nr of lags in AR
    Par        = [];
    fcstH      = 4;                                                         % Forecast horizon (quarters)
    
else   error('Set P.Model correctly')
end

%__________________________________________________________________________
%
%%          II. G E N E R A L  E S T I M A T I O N  O P T I O N S
%__________________________________________________________________________

%__________________________________________________________________________
% 1. Base Directory
Base_Dir  = 'G:\EBO\ECMO\de Winter\Werk\Onderzoek\PROJECT 11.  KANT PICK DE WINTER\Replication\DFROG CONTRIBUTION\';                      % !!!!!!!!!!!!!!!! ADJUST !!!!!!!!!!!!!!!! Work

%__________________________________________________________________________
% 2. Month of datafile
MontH = 'mrt19';                                                                % !!!!!!!!!!!!!!!! ADJUST !!!!!!!!!!!!!!!! month jan,feb etc.

%__________________________________________________________________________
% 3. Series list
serList = [2 3 6 9 19:22 32:34 36 39 44:50 52 58 60:61 63:67 69:82 ...
            84 87:88 90:93 96:98 105:108 135:142 152:154 156 159:164 ...
            166 168:169 171 177:181];

%__________________________________________________________________________
% 4. Data transformations
P.C3    = 'A';                                                              % Transformation of monthly data: use 3-month growth rate
P.OC    =  0;                                                               
%  2  : Replace outliers with median
%  3  : Replace outliers with largest
%       admissable value (see Outliers)
%__________________________________________________________________________
% 5. Model in y-o-y terms or q-o-q terms
P.yoy   = 0;                                                                % P.yoy=1 if you want model in year on year mutations (y-o-y). P. yoy=0 if you want the model in

% quarter on quarter mutations (q-o-q).
if P.yoy   == 1; YCode='_yoy';
elseif P.yoy == 0; YCode='';
else error('Set P.yoy to 0 (=q-o-q) or 1 (=y-o-y) in ESTIMATION file');
end

%_____________________________
% Timing (in consec order)
T.startE  = [1986  1];                                                     % Start of estimation period              [1,4,7,10]
T.startF  = [1992  1];                                                     % Start of forecasting period             [1,4,7,10]
T.startV  = [1992  3];                                                     % Start of forecast evaluation period     [3,6,9,12]
T.endF    = [2019  9];                                                     % End of forecast evaluation period       [3,6,9,12]
T.Now     = [2022  3]; 

% -> reference point for trimming = month on which data are downloaded + 3 years

%__________________________________________________________________________
% 7. Further parameters
fcstH     = 2;                                                              % Forecast horizon (quarters)
smooth    = 1;                                                              % Smoothed forecasts in observation vector (at|T) ; Filtered forecasts=0 (at|t). Standard is smoothed
Q_a       = [0; 0; 1];                                                      % Pattern of GDP data availability: Q_a(i) = 1 -> prev quarter GDP is available in mnth i of quart
saveflag  = 1;                                                              % save Fcsts to xls
saveFile  = 0;                                                              % save complete file after estimation

%__________________________________________________________________________
% Check dates
if ~ismember(T.startE(2),[1 4 7 10]) || ...
        ~ismember(T.startF(2),[1 4 7 10])
    error('startE & startF must be set at start of quarter')
end
if ~ismember(  T.endF(2),[3 6 9 12])
    error('endF must be set at end of quarter')
end