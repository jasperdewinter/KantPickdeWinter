%__________________________________________________________________________
%%                 DYNAMIC FACTOR MODEL ESTIMATION
%
%                         Jasper de Winter
%                      (De Nederlandsche Bank)
%
%                           August 14, 2018
%
% The program evaluates the forecast performance of the dynamic factor
% model by Doz et al. (2011) as implemented at DNB. The program runs
% recursive forecasts and calculates the RMSEs.It is an adjusted version of
% the original version. Goal is to call only one function (and no functions
% in functions).
%
%__________________________________________________________________________
% Data Functions:
%
% Transform:     Transforms data according to input code (log, first
%                difference, filter.
% q2m:           Transforms quarterly --> monthly.
% mon2qrt:       Creates the quarterly date in which month date lies
% Trimdata:      Trims a data matrix X and the corresponding date vectors
%                to run from date0 to date1.
% TrimPRealTime: Shortens X by nPer while preserving the NaN pattern at
%                the end (shifting the NaN pattern by nPer periods
%                backwards. Used for creating quasi-realtime data.
% TrimQRealTime: For use in recursive forecasting.Shortens the matrix of
%                quarterly series y by nPer periods, deletes data for  the
%                current quarter and applies pattern of data availability
%                to previous.
% Outliers:      Identifies missing values (NaN) and outliers in series x
% reshuffle:     Reorders the forecasts produced in the main program to
%                obtain the forecasts done in the individual months of
%                the quarters.
% z01:           Produces standardised data set x0 with mean 0 and
%                variance 1 per series.
% calc_rmse:     Calculates the root mean squared error from errors in
%                errorQ over time
%__________________________________________________________________________
% Date Functions:
%
% Add2date:     Calculates the new date which is obtained from shifting
%               Date forward by n months.
% Datediff:     Forms the difference between two monthly Dates.
% Date2str:     Creates a string of a Date to print out.
% DateFind:     Finds the indices of a vector date in Date vector Dates.
%
%
%__________________________________________________________________________
% Estimation:
%
% Estim_SPC:    Estimate Principal components of X-matrix. Also contains
%               Several selections criterias. Currently unused (P.Crit=0)
%               (equation 1 Banbura and Runstler)
% Estim_PC:     Estimates VAR of f(t) and f(t-1)...f(t-p). Also contains
%               possibility to selec with AIC criterium. Currently unused
%               (P.crit=0)
%
%               Besides reduces the rank of the covariance matrix by
%               factor analysis on errors of the VAR. Also containg the
%               possibility to select # of lags with Bai and Ng(2005)
%               criterion. It callss Estim_DFP to determine the #
%               of factors. Currently unused (P.crit=0).
%
% OLS:          Estimates an OLS of the factors f(t) on Y
%
%__________________________________________________________________________
% Kalman Equations:
%
% DFM_ECB:      DFM_ECB(Par,S) builds the state space form. Par is a
%               structure containing Parameter values and other arbitrary
%               information. Structure S contains matrices {Z, T, G, H}
%               plus initial conditions A1 and P1 = cov(A1) for the state
%               vector.
%
% KFilter:      Uses the Kalman Filter equations (D&K,2001)
%
% KSmoother:    Uses the Kalman Smoother equations (D&K,2001)
%
% KFignal:      Calculates the Signal from the Kalman equations (D&K,2001)
%__________________________________________________________________________
%
% Data_f:       Monthly dates, lengthened with the forecasts
% fcstj :       Monthly (smoothed) realisations and forecast of monthly
%               growth (t - t(-3)/t(-3)) on a quarterly basis
% fcstX :       Matrix of (smoothed) forecasts and realisations of the X's
%
%__________________________________________________________________________
%
%%                     0. M O D E L  P R E L I M I N A R I E S
%__________________________________________________________________________
tic

%__________________________________________________________________________
% Estimation file
ESTIMATION

%__________________________________________________________________________
% Add path
addpath(genpath(Base_Dir));

%__________________________________________________________________________
% Data
Data         = ['data_', CCode, MontH];
Datafile     = ['Data\dnb\data_',CCode];
Datafile     = [Datafile,MontH];

%__________________________________________________________________________
%
%%                    O. L O A D   D A T A
%__________________________________________________________________________
F               = load([Base_Dir Datafile '.mat']);
X               = F.M.Raw(:,serList);
Date            = F.M.Date;
% F.L.Code(:,3) = 3;                                                        % Shut this of to prevent putting everyything in 3-month growth rates

X               = Transform(X,F.L.Code(serList,:),P.yoy,0);                 % Transform: Transforms the data according to input code
Y               = F.Q.y(:,1);
Y_M             = q2m(Y,F.Q.Date,Date(1,2),Date(end,2));                    % q2m: Converts the quarterly dates and data into monthly format

X               = real(X);                                                  %NEW, you had imaginary parts somehow (d.d.29.01.2013)

%__________________________________________________________________________
%
%%                     0. D A T A  P R E L I M I N A R I E S
%__________________________________________________________________________

%__________________________________________________________________________
% Create Group Code
Gr              = F.L.GCode(serList,:);

%__________________________________________________________________________
% Get Y in quarterly format (delete rows of NaNs)
[YQ, DateQ]     = m2q(Y_M,Date,'3');

%__________________________________________________________________________
% Trim data to startE : Now
% Delete all observations before T.startE and after T.now
% Replace varibales and Date with NaN if variable is non-existent
[Xc, DateX]     = TrimData(X  , Date, T.startE,T.Now,'M');
Yc              = TrimData(Y_M, Date, T.startE,T.Now,'M');

%__________________________________________________________________________
% Extend timing to fill forecasts over the entire sample
startF          = Add2Date(T.startF,-3*fcstH);
endF            = Add2Date(T.endF,3);

%__________________________________________________________________________
% Nr of recursions, preallocation of variable memory
nR              = DateDiff(endF,startF)+1;
fcst            = nan(nR,fcstH+2);
Yref            = nan(nR,fcstH+2);
fDate           = nan(nR,2);

%__________________________________________________________________________
% Trim data to endF while keeping data release pattern against now
diffFN          = DateDiff(T.Now,endF);
Xc              = TrimPRealTime(Xc,diffFN);
Yc              = TrimQRealTime(Yc,diffFN,Date,Q_a);
DateX           = DateX(1:end-diffFN,:);
% Store 'original' data to continue looping over the same data set
Xc_final        = Xc;
Yc_final        = Yc;
DateX_final     = DateX;                                                    % Retain original DateX for loop over parameters

% Iterate over parameter settings to later average them
% H = P;                                                                      % Use for parameter settings
% ParSet_iteration   = combvec(H.r,H.q,H.p);
% ParSet_iteration(:,ParSet_iteration(2,:)>ParSet_iteration(1,:)) = [];       % Remove all cases in which q>r
% Nr_iter_par = size(ParSet_iteration,2);
%forecast_spec = nan(Nr_iter_par,); % CHANGE to have correct dimensions!

% for iter_par = 1:Nr_iter_par
    %%_________________________________________________________________________
    %
    %%  I. P S E U D O  R E A L  T I M E  A N A L Y S I S
    %_________________________________________________________________________

    for i = 1:nR                                                                % counts down from last to first forecast

        % ticker
        disp([Country,' Period ' Date2str(DateX(end,:))])

        %______________________________________________________________________
        %
        %%   S T E P 1:  E S T I M A T E  H Y P E R P A R A M E T E R S
        %______________________________________________________________________

        % _____________________________________________________________________
        % 0. Preliminaries
        Xd    = Xc;                                                             % rename Xc to Xd: X dynamic factor model
        Yd    = Yc;                                                             % rename Yc to Yd: Y dynamic factor model
        DateD = DateX;                                                          % rename DateX to DateD: Date dynamic factor model
        Q     = P;                                                              % Do not change original input P - use Q
%         Q.r   = ParSet_iteration(1,iter_par); ...
%         Q.q   = ParSet_iteration(2,iter_par);
%         Q.p   = ParSet_iteration(3,iter_par);                                   % ADJUST ADJUST ADJUST! TAKE CORRECT COLUMN
        n     = size(Xd,2);                                                     % Determine size of X-vector
        Vd    = zeros(n,4);                                                     % Preallocate Vd

        %______________________________________________________________________
    
        %______________________________________________________________________
        %
        %%    S T E P 5: T I C K E R  T O  C R E A T E  L O O P
        %______________________________________________________________________
         DateSave = [Country, ' Period ' Date2str(DateX(end,:))];
         FileName = [Base_Dir,'MIDASSO\'];
         save([FileName DateSave '.mat'],'Yc_final', 'Xd','DateD', 'Yd');

        %______________________________________________________________________
        % 1. Trim by one while keeping quasi-RT pattern
        Xc          = TrimPRealTime(Xc,1);
        Yc          = TrimQRealTime(Yc,1,DateX,Q_a);
        DateX       = DateX(1:end-1,:);
        
    end
        Xc = Xc_final;
        Yc = Yc_final;
        DateX = DateX_final;


  