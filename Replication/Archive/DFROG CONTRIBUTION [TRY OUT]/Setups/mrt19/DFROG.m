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
    n     = size(Xd,2);                                                     % Determine size of X-vector
    Vd    = zeros(n,4);                                                     % Preallocate Vd
    
    %______________________________________________________________________
    % 1. Extract Static principal components                                % Equation 1 Banbura and Runstler
    if Q.gr == 1;
        test                = z01(Xd(1:end-P.cutE,:));
        [Xe,Cx]             = Outliers(z01(Xd(1:end-P.cutE,:)),2);          % 2013.04.01.2013
        %       [Xe,Cx]             = Outliers(z01(Xd(1:end-P.cutE,:)),3,[]);
        f1                  = sum(Gr(:,1)==1);                              % # of variables in group1 constituting factor 1 (F1)
        f2                  = sum(Gr(:,1)==2)+f1;                           % # of variables in group1 constituting factor 2 (F2)
        f3                  = sum(Gr(:,1)==3)+f2;                           % # of variables in group1 constituting factor 3 (F3)
        f4                  = sum(Gr(:,1)==4)+f3;                           % # of variables in group1 constituting factor 4 (F4)
        
        [F1 V1 D1]          = Estim_SPC(Xe(:,1:f1),max(1),P.crit);          % Calculate Static Principal Components on standardized & cleaned X's, Fd = estimated factors, Vd= eigenvvectors, Dd= eigenvalues
        [F2 V2 D2]          = Estim_SPC(Xe(:,(f1+1):f2),max(1),P.crit);     % Calculate Static Principal Components on standardized & cleaned X's, Fd = estimated factors, Vd= eigenvvectors, Dd= eigenvalues
        [F3 V3 D3]          = Estim_SPC(Xe(:,(f2+1):f3),max(1),P.crit);     % Calculate Static Principal Components on standardized & cleaned X's, Fd = estimated factors, Vd= eigenvvectors, Dd= eigenvalues
        [F4 V4 D4]          = Estim_SPC(Xe(:,(f3+1):f4),max(1),P.crit);     % Calculate Static Principal Components on standardized & cleaned X's, Fd = estimated factors, Vd= eigenvvectors, Dd= eigenvalues
        
        Vd((1:f1),1)        = V1;                                           % Put Eigenvectors in 4*n matrix; first  column contains frst eigenvector first  group
        Vd((f1+1):f2,2)     = V2;                                           % Put Eigenvectors in 4*n matrix; second column contains first eigenvector second group
        Vd((f2+1):f3,3)     = V3;                                           % Put Eigenvectors in 4*n matrix; third  column contains first eigenvector third group
        Vd((f3+1):f4,4)     = V4;                                           % Put Eigenvectors in 4*n matrix; fourth column contains first eigenvector fourth group
        
        Fd                  = Xe * Vd;                                      % Assume orthogonality of factors
        Cd                  = Fd * Vd';
        Errd                = Xe - Cd;
    else
        [Xe,Cx]             = Outliers(z01(Xd(1:end-Q.cutE,:)),2);          % remove outliers from standardized data (function z01), and remove ragged edges. Use x's from 1 to end minus Q.cutE). Defines the dimension of the the static factors.
        %      [Xe,Cx]             = Outliers(z01(Xd(1:end-Q.cutE,:)),3,[]);       % remove outliers from standardized data (function z01), and remove ragged edges. Use x's from 1 to end minus Q.cutE). Defines the dimension of the the static factors.
        [Fd Vd Dd]          = Estim_SPC(Xe,max(Q.r),Q.crit);                % Calculate Static Principal Components on standardized & cleaned X's, Fd = estimated factors, Vd= eigenvvectors, Dd= eigenvalues
        Cd                  = Fd * Vd';
        Errd                = Xe - Cd;
    end
    
    FQ        = filter([1/3 1/3 1/3],1,Fd);                                 % Calculates average of last three months (1/3,1/3,1/3)
    FQ(1:2,:) = nan;                                                        % Set first two rows equal to nan
    
    %______________________________________________________________________
    % 2. Set the Parameterset
    if Q.crit  == 0;                                                        % Als Q.crit=0 than use predefined # of static and dynamic factors
        ParSet   = combvec(Q.r,Q.q,Q.p);                                    % combine the # of static and dynamic factors as well as the # of lags in the VAR in one vector. (1,1)= # static factors, (2,1)= # dynamic factors, (3,1)= maximum nr. of delays in VAR
        lst      = ParSet(1,:) >= ParSet(2,:);                              % check if # static factors > # of dynamic factors
        ParSet   = ParSet(:,lst);                                           % als q>=r than ParSet=[], causing a halt in the setup further on.
    else
        ParSet   = [size(Fd,2); 0; max(Q.p)];                               % Parset matrix, (1,1)= # static factors, (2,1)= # dynamic factors, (3,1)= maximum nr. of delays in VAR
        Q.qflag  = 2;                                                       % In Estim_PC  use Bai & Ngi (2005) criterium for choice # of dynamic factors
    end
    
    %______________________________________________________________________
    % 3. Prepare data for KF: extend to end of forecast
    %    horizon and set outliers to nan
    endFD        = mon2qrt(Add2Date(DateD(end,:),3*fcstH));                 % Add2Date simple function that adds date at the end of the sample
    Xf           = TrimData(Xd, DateD,DateD(1,:),endFD,'M');                % Trim X-Matrix from Date(1,:), endF, using monthly frequency
    [yf Date_f]  = TrimData(Yd, DateD,DateD(1,:),endFD,'M');                % Trim y as well
    Xf           = Outliers(z01(Xf),0);                                     % Remove outliers after Xf is standardized%
    %   Xf           = Outliers(z01(Xf),3,[]);                                  % Remove outliers after Xf is standardized
    Q.Date       = Date_f;                                                  % Quarterly Dates are Date_f
    
    %______________________________________________________________________
    % 4. Main loop
    fcstD  = [];
    sdevD  = [];
    Q.r   = ParSet(1,1);                                                    % number of static factors  (= "old"Q.r)
    Q.q   = ParSet(2,1);                                                    % number of dynamic factors (= 0 (if Q.crit n.e. 0)
    Q.p   = ParSet(3,1);                                                    % number of lags            (= "old" Q.p)
    
    %______________________________________________________________________
    % 5. Create Input Eauation 1, Banbura and Runstler (2011)
    Fj       = Fd(:,1:Q.r);                                                 % F are the factors         (= "old" static factors)
    Vj       = Vd(:,1:Q.r);                                                 % V are the eigenvectors    (= "old" of eigenvectors)
    [nobs,r] = size(Fj);
    p        = Q.p;
    
    Q.C   = [Vj zeros(n,Q.r*(Q.p-1))];                                      % matrix of static eigenvectors horizontally concatenated to zero matrix
    Q.R   = diag(diag(cov(Xe-Fj*Vj')));                                     % diagonal covariance matrix of multivariate white noise term in eq (1) of Banbura and Rünstler)
    % Xe -Fj*Vj' = x(t) - lambda*f(t). first diag command takes the diagnoal of the covariancematrix
    % of the idosynchratic error term cov(Xe -Fj*Vj'). Second diag puts this outcome in a diagonal matrix.
    % the diagonalization of this matrix is "forced". Covariances are ignored in the model.
    %______________________________________________________________________
    % 6. Estimate OLS of factors f(t) on y                                  % Equation 3 Banbura and Runsteler, 2011
    [Q.beta Q.s] = OLS(Yd(3:end-P.cutE,:),FQ(3:end,1:Q.r),1,0);             % Q.beta contains coefficients of beta in eq. 3. Q.s is Cholesky decomposition (a lower triangular matrix R
    % for wich R*R'= covariance matrix.
    
    if min(diag(Q.R)) < 1e-2                                                % Correct for a too small variance of the idiosyncratic component
        LL         = find(diag(Q.R)<1e-2);                                  % Otherwise the KF would break down
        for k = 1:length(LL)
            Q.R(LL(i),LL(i)) = 1e-2;
        end
        disp(('RUN_DFM_ECB: elements of matrix R adjusted to 1e-2'))
        disp(num2str(LL))
    end
    
    %______________________________________________________________________
    % 7. Find optimal lag for the VAR of f(t)s                              % Equation 2 Banbura and Runstler, 2011
    if Q.crit > 0
        VP.lags   = p;                                                      % max # of lags in VAR
        VP.cflag  = 1;                                                      % add constant to VAR
        VP.IC     = 'AIC';                                                  % Use Akaike Selection Criterion for # of lags in VAR
        [V0 V1]   = V_AR(Fj,VP,0);                                          % Serie = Fj, VP criteria, 0 indicates forecast horizon is zero
        % V0 = F series extended with forecasts,
        % V1 = Coefficient matrices
        p         = size(V1,3);                                             % The loop inside V_AR determines the optimal # of lags, and these are saved in p
    end
    
    %______________________________________________________________________
    % 8. Stacked VAR regression F(t) = A*F(t-p) + e(t)
    I  = eye(r*p);                                                          % Identity matrix with dim <(# of static factors * # of lags in VAR), (# of static factors * # of lags in VAR)>
    A  = [ zeros(r,r*p) ; I(1:end-r,1:end) ];                               % This is nog the A in eq.2. Rather is it a preallociation for the SSM matrices.
    
    %______________________________________________________________________
    % 9. AR regression of factors                                           % Form matrix of vars Z (F(t)s in columns, repeated times the # of lags, with deletion of first and last observations
    % Z is defning matrix for AR regression by estimating stacked F(t) = A*F(t-1) + e(t)
    Z = [];
    for k = 1:p
        Z = [Z Fj(p-k+1:end-k,:)];                                          % here you create a matrix containing the lagged factors (horizontally concatenated)
    end
    
    z            = Fj(p+1:end,:);
    A1           = inv(Z'*Z)*Z'*z;                                          % A1 = Beta in normal OLS = inv(X'X)X'y
    E            = z  - Z*A1;                                               % Error in equation B&R equation 2
    A(1:r,1:r*p) = A1';                                                     % A is the matrix that will be input for the Kalman equations
    
    %______________________________________________________________________
    % 10. Find optimal q as from Bai & Ng (2005) when Q.crit > 0
    if Q.crit == 0
        q = min([Q.q r]);
    else
        q = Estim_DFP(E,n,Q.qflag,1);                                       % DFP gives you the equations for the Bai and Ng (2005) criterion for the # of dynamic factors
        disp(['Nr of dynamc factors = ' num2str(q)]);
        disp(['Nr of lags           = ' num2str(p)]);
    end
    
    %______________________________________________________________________
    % 11. Form reduced rank covariances Q = H*H' from resids E
    H           = zeros(p*r,p*r);
    Q1          = zeros(p*r,p*r);
    Q1(1:r,1:r) = cov(E);                                                   % Q1 is the covariance matrix of the error term in B&R equation 2
    
    %   if q > 1
    [D M] = eig(full(cov(E)));                                              % Calculate all eigenvalues/eigenvectors. M = eigenvalues, D = eigenvectors
    D     = D(:,end-q+1:end);                                               % Select q largest eigenvectors = q = the # of dynamic factors
    D     = fliplr(D);                                                      % Put largest eigenvector in column 1 and the second largest eigenvalue in column 2
    D     = D*diag(sign(D(1,:)));                                           % take the sign of the first row of the eigenvectors and put this in a diagonal matrix (in case q=2 a <2 x 2> matrix)
    % and multiply this with the original D matrix
    M     = M(end-q+1:end,end-q+1:end);                                     % retain the q biggest eigenvalues
    M     = rot90(M);                                                       % rotate 90 degrees
    M     = fliplr(M);                                                      % flip from "left" to "right"
    
    %     else
    %         D = 1;
    %         M = q;
    %     end
    
    H(1:r,1:q)  = D*sqrt(M);                                                % eigenvectors * wortel(eigenvalues)
    Q1(1:r,1:r) = D*M*D';                                                   % eigenvector * eigenvalues * eigenvector
    
    %______________________________________________________________________
    % 12. Save output in Q-matrix
    Q.A  = A;
    Q.H  = H;
    Q.Q  = Q1;
    Q.p  = p;
    Q.q  = q;
    
    %______________________________________________________________________
    %                                                                       % Kalman filter & smoother. Model is build from hyperparameters in
    %%  S T E P 2: S T A T E  S P A C E M O D E L                           % step 2. Factors are re-estimated by the Kalmans filter. For this
    %______________________________________________________________________ % reason they need no longer be orthogonal to the disturbance term.
    
    %______________________________________________________________________
    % 1. Build state space model
    SSM = DFM_ECB(Q,[],0);
    
    %______________________________________________________________________
    % 2. Kalman filter
    R_KF     = KFilter([Xf yf],Q.Model,Q,SSM);                              % Calculate outcome Kalman Filter by function SKF and put outcome in R.struct
    
    %______________________________________________________________________
    % 3. Kalman smoother
    R_KF     = KSmoother([Xf yf],Q.Model,Q,R_KF,SSM);                       % Calculate outcome Kalman Smoother by function FIS and put outcome in R.struct
    
    %______________________________________________________________________
    % 4. Signal vector                                                      % !!!!! (1=smoothed estimates; 0=filtered estimates) !!!!!!!!
    [R_KF S] = KSignal([Xf yf],Q.Model,Q,R_KF,1,SSM);                       % Calculate signal from Kalman Filter by function SEC and put outcome in R.struct
    
    %______________________________________________________________________
    % 5. Monthly GDP, X and variance forecasts                              % Date_f contains the Dates belonging to the matrices under the D. heading.
    fcstj    = R_KF.signal(:,end);                                          % Calculate forecasts for GDP by taking the last column of the R_KF.signal matrix
    sdevj    = real(R_KF.sign_P(:,end));                                    % Calculate the standard deviation for the forecasts of GDP. Only take the real part because matrix contains imaginary # cause root (-0,00000) is im. #, and it changes whole matrix to imaginary # (see "Kalman filter-smoother Matrices.xls")
    fcstX    = R_KF.signal(:,(1:(end-1)));                                  % Calculate forecasts for the X-Matrix
    sdevX    = real(R_KF.sign_P(:,(1:(end-1))));                            % Calculate the standard deviation for the forecasts of the X's. Only take the real part because matrix contains imaginary # cause root (-0,00000) is im. #, and it changes whole matrix to imaginary # (see "Kalman filter-smoother Matrices.xls")
    
    %______________________________________________________________________
    % 6. Store forecasts
    fcstD  = [fcstD fcstj(end-(fcstH+1)*3:3:end,:)];                        % Store forecasts
    sdevD  = [sdevD sdevj(end-(fcstH+1)*3:3:end,:)];                        % Store forecasts
    
    %______________________________________________________________________
    %%    S T E P 3: C A L C U L A T E  W E I G H T S
    %
    %______________________________________________________________________
    
    if P.wgt == 1;
        
        %__________________________________________________________________
        % 1. Calculate weights
        weight            = KFWeights(R_KF,Q.Model,Q,'smooth');             % weight = weights from Kalman Filter/Smoother
        
        %__________________________________________________________________
        % 2. Calculate contributions
        now   = size(Date_f,1)-6;
        back  = Contributions([Xf yf],weight{(now-3)},Q.Model,Q,(now-3));   % contributions to the "nowcast" using outcome of KFWeights
        nowc  = Contributions([Xf yf],weight{now},Q.Model,Q,now);           %                      "backcast"
        q1    = Contributions([Xf yf],weight{(now+3)},Q.Model,Q,(now+3));   %                      "1Q ahead forecast"
        q2    = Contributions([Xf yf],weight{(now+6)},Q.Model,Q,(now+6));   %                      "2Q ahead forecast"
      
        %__________________________________________________________________
        % 3. Calculate the contributions to the back, now and forecasts
        C(1,:) = sum(back.CF_p,2);                                          % "backcast" contribution per variable
        C(2,:) = sum(nowc.CF_p,2);                                          % "nowcast"
        C(3,:) = sum(q1.CF_p,2);                                            % "1Q ahead forecast"
        C(4,:) = sum(q2.CF_p,2);                                            % "2Q ahead forecast"
        C(:,(size(Xf,2)+2))= S.c1((size(Xf,2)+1),1);                        % Constant (=average GDPP growth)
        
        %__________________________________________________________________
        % 4. Cumulative weights per variable
        temp               = permute(weight{now-3},[3,1,2]);
        W(1,:) = sum(temp(:,end,:)); clear temp;                            % "backcast" cumulative weights per variable
        temp               = permute(weight{now},  [3,1,2]);
        W(2,:) = sum(temp(:,end,:)); clear temp;                            % "nowcast"
        temp               = permute(weight{now+3},[3,1,2]);
        W(3,:) = sum(temp(:,end,:)); clear temp;                            % "1Q ahead"
        temp               = permute(weight{now+6},[3,1,2]);
        W(4,:) = sum(temp(:,end,:)); clear temp;                            % "2Q ahead"
        
        %__________________________________________________________________
        % 5. Ccumulative weights per group
        gList             = unique(Gr);
        ngroup            = size(gList,1);
        WGr               = zeros(size(W,1),(ngroup+1));                    % Matrix WGr has following dimensions: the # of rows of W and the number of columns equals the # of specified groups
        
        for j=1:size(WGr,1),
            gewicht       = W(j,:);
            for k         = 1:length(gList),
                gN        = gList(k);
                WGr(j,gN) = sum(gewicht(:,ismember(Gr,gN)));
            end
            WGr(:,end)    = W(:,end);                                       % Weight of GDP
        end
        
        %__________________________________________________________________
        % 6. Contributions per group
        CGr               = zeros(size(C,1),(ngroup+1+1));                  % Matrix CGrDeze matrix heeft de afmeting van C (in dit geval 1 tot en met 4, t is S.c1) en als kolommen het aantal groepn dat je onderscheid (momenteel 5: production and sales, surveys, financial, prices, other)
        % De eerste +1 is voor de bijdrage van het bbp en de tweede +1 is voor de constante, die wil je namelijk (mogelijk) ook weergeven
        for j=1:size(CGr,1),
            contr         = C(j,:);
            for k         = 1:length(gList),
                gN        = gList(k);
                CGr(j,gN) = sum(contr(:,ismember(Gr,gN)));
            end
            CGr(:,end-1)  = C(:,end-1);                                     % Contribution of GDP
            CGr(:,end)    = C(:,end);                                       % Constant
        end
        
        %__________________________________________________________________
        % 7. Save results in arrays
        eval(['Weight.W'     num2str(i) ' = W ;'])                          % Cumulative weights per variable
        eval(['WeightGr.WGr' num2str(i) ' = WGr ;'])                        % Cumulative weights per group
        eval(['Signal.S'     num2str(i) ' = S ;'])                          % Signal
        eval(['Contrib.C'    num2str(i) ' = C ;'])                          % Contribution per variable
        eval(['ContGr.CGr'   num2str(i) ' = CGr ;'])                        % Contribution per group
        
        %__________________________________________________________________
        % 8. Clear C and W matrix for next loop
        clear C W
    end
    
    %______________________________________________________________________
    %
    %%   S T E P 4: P O S T  E S T I M A T I O N  O P E R A T I O N S
    %
    %______________________________________________________________________
    
    %______________________________________________________________________
    % 1. Create forecast dates
    Date_fcst = Date_f(end-(fcstH+1)*3:3:end,:);                            % Create Dates forecasts
    
    %______________________________________________________________________
    % 2. Store optimal specification
    P.ParSet  = ParSet;                                                     % Save parameterset
    if P.crit > 0
        if isfield(P,'r_opt')
            P.r_opt = [P.r_opt; size(Fd,2)];
            P.q_opt = [P.q_opt; Q.q];
            P.p_opt = [P.p_opt; Q.p];
        else
            P.r_opt = size(F,2);
            P.q_opt = Q.q;
            P.p_opt = Q.p;
        end
    end
    
    %______________________________________________________________________
    % 3. Store fcst & forecast dates
    if i == 1 && size(fcstD,2) > 1
        fcst = nan(nR,fcstH+2,size(fcstD,2));
    end
    fcst(i,:,:) = fcstD;


    %______________________________________________________________________
    % 4. Store filterd level estimation error standard deviation
    if i == 1 && size(sdevD,2) > 1
        sdev = nan(nR,fcstH+2,size(sdevD,2));
    end
    sdev(i,:,:) = sdevD;
    
    fDate(i,:)  = DateX(end,:);
    idxF        = DateFind(DateQ,mon2qrt(DateX(end,:)));
    Yref(i,:)   = YQ(idxF-1 : idxF+fcstH);
    
    %______________________________________________________________________
    %
    %%    S T E P 5: T I C K E R  T O  C R E A T E  L O O P
    %______________________________________________________________________
    
    %______________________________________________________________________
    % 1. Trim by one while keeping quasi-RT pattern
    Xc          = TrimPRealTime(Xc,1);
    Yc          = TrimQRealTime(Yc,1,DateX,Q_a);
    DateX       = DateX(1:end-1,:);
end

%%_________________________________________________________________________
%
%% II.A. S A V E  F O R E C A S T S, S D E V  A N D  W E I G H T S
%%_________________________________________________________________________

%__________________________________________________________________________
% 1. Flip timing of the forecast and sdev from A-Z to Z-A
fcst   = flipdim(fcst,1);
Yref   = flipdim(Yref,1);
fDate  = flipdim(fDate,1);

[fcstQ YrefQ fDateQ errQ]        = reshuffle(fcst,Yref,fDate);

%__________________________________________________________________________
% 2. Calculate RMSE for both evaluation (V) & pre-sample (s) periods
rmse_V  = calc_rmse(errQ,fDateQ,T.startV,T.endF,'Q');

rmse_s = nan(size(rmse_V));
T.endL = [];
if DateFind(fDateQ,T.startV) > 1
    T.endL  = fDateQ(DateFind(fDateQ,T.startV)-1,:);
    rmse_s  = calc_rmse(errQ,fDateQ,T.startF,T.endL,'Q');
end

%__________________________________________________________________________
% 3. Write Forecasts and RMSEs to Matlab-file
if saveflag == 1
    if P.crit == 0 ; ext = [P.C3 ' loop'];
    else             ext = [P.C3 ' opt'];
    end
    FileName = ['DFM FCST_RMSE ' Country ' ' ext,YCode];
    FileName = [Base_Dir,'Output\',MontH,'\',FileName];
    
    disp('_____________________________________________________________');
    disp('Saving output to Matlab-file ')
    disp(FileName);
    delete([FileName '*.*'])
    save(FileName,'fcstQ','YrefQ','fDateQ','rmse_V','rmse_s','P','T')
end

%__________________________________________________________________________
% 4. Save RMSes
flst = [1:size(rmse_V,1) zeros(1,3)]';
if P.crit > 0
    ParSet = zeros(3,1);
else
    ParSet = P.ParSet;
end
rmse_V = [P.ParSet ; rmse_V];
rmse_s = [P.ParSet ; rmse_s];

%__________________________________________________________________________
% 5. Write Forecasts and RMSEs to Excel-file
if saveflag  == 1
    disp('_____________________________________________________________');
    disp('Saving output to Excel-file ')
    disp([FileName,'.xls'])
    for i = 1:size(fcstQ,3)
        SName = ['r',int2str(P.ParSet(1,i)),...
            'q',int2str(P.ParSet(2,i)),...
            'p',int2str(P.ParSet(3,i))];
        
        fcsti  = squeeze(fcstQ(:,:,i));
        xlswrite(FileName,{'Year','Month','Data','Fcst'},SName,'B2');
        xlswrite(FileName,[fDateQ YrefQ fcsti ]         ,SName,'B3');
    end
end

%%_________________________________________________________________________
%
%% II.B. S A V E  F I L T E R E D  L E V E L  E S T . E R R . V A R.
%%_________________________________________________________________________

%__________________________________________________________________________
% 1. Flip timing of the forecast and sdev from A-Z to Z-A
sdev   = flipdim(sdev,1);
[sdevQ YrefQ fDateQ errQ]        = reshuffle(sdev,Yref,fDate);

%__________________________________________________________________________
% 2. Calculate RMSE for both evaluation (V) & pre-sample (s) periods
rmse_V  = calc_rmse(errQ,fDateQ,T.startV,T.endF,'Q');

rmse_s = nan(size(rmse_V));
T.endL = [];
if DateFind(fDateQ,T.startV) > 1
    T.endL  = fDateQ(DateFind(fDateQ,T.startV)-1,:);
    rmse_s  = calc_rmse(errQ,fDateQ,T.startF,T.endL,'Q');
end

%__________________________________________________________________________
% 3. Write filtered level estimation error variances to Matlab-file
if saveflag ==1
    if P.crit == 0 ; ext = [P.C3 ' loop'];
    else             ext = [P.C3 ' opt'];
    end
    FileName = ['DFM SDEV ' Country ' ' ext,YCode];
    FileName = [Base_Dir,'Output\',MontH,'\',FileName];
    
    disp('_____________________________________________________________');
    disp('Saving output to Matlab-file ')
    disp(FileName);
    delete([FileName '*.*'])
    save(FileName,'sdevQ','YrefQ','fDateQ','rmse_V','rmse_s','P','T')
end

%__________________________________________________________________________
% 4. Save RMSes
flst = [1:size(rmse_V,1) zeros(1,3)]';
if P.crit > 0
    ParSet = zeros(3,1);
else
    ParSet = P.ParSet;
end
rmse_V = [P.ParSet ; rmse_V];
rmse_s = [P.ParSet ; rmse_s];

%__________________________________________________________________________
% 5. Write filtered level estimation error variances to Excel-file
if saveflag  == 1
    disp('_____________________________________________________________');
    disp('Saving output to Excel-file ')
    disp([FileName,'.xls'])
    for i = 1:size(sdevQ,3)
        SName = ['r',int2str(P.ParSet(1,i)),...
            'q',int2str(P.ParSet(2,i)),...
            'p',int2str(P.ParSet(3,i))];
        
        sdevi  = squeeze(sdevQ(:,:,i));
        xlswrite(FileName,{'Year','Month','Data','Stdev'},SName,'B2');
        xlswrite(FileName,[fDateQ YrefQ sdevi ]          ,SName,'B3');
    end
end

%%_________________________________________________________________________
%
%% II.C. S A V E  W E I G H T S  A N D  C O N T R I B U T I O N S
%__________________________________________________________________________

if P.wgt ==1;
    %______________________________________________________________________
    % 1. Calculate weights and contributions per group
    
    [Contribution_Group Weight_Group] = Weight_Grp(Yref,YrefQ,ContGr,WeightGr);
    [Contribution_Var   Weight_Var]   = Weight_Var(Yref,YrefQ,Contrib,Weight);
    
    %______________________________________________________________________
    % 2. Write output for the calculation of weights
    if P.crit == 0 ; ext = [P.C3 ' loop'];
    else             ext = [P.C3 ' opt'];
    end
    FileName = ['DFM WEIGHTS ' Country ' ' ext,YCode];
    FileName = [Base_Dir,'Output\',MontH,'\',FileName];
    
    disp('_____________________________________________________________');
    disp('Saving output to Excel-file ')
    disp([FileName,'.xls'])
    delete([FileName '*.*'])
    
    save(FileName)
    
    SName = 'C_G';
    xlswrite(FileName,[Contribution_Group],SName,'A1');
    SName = 'C_VAR';
    xlswrite(FileName,[Contribution_Var],SName,'A1');

    SName = 'W_G';
    xlswrite(FileName,[Weight_Group],SName,'A1');
    SName = 'W_VAR';
    xlswrite(FileName,[Weight_Var],SName,'A1');
end
%__________________________________________________________________________
%
%%  II.D. S A V E  X's  A N D  R A W  F A C T O R S
%%_________________________________________________________________________

if P.fac ==1;
    %______________________________________________________________________
    % 1. Name File
    
    FileName = ['FACT_CHECK' ' ' Country, YCode,'.xls'];
    FileName = [Base_Dir,'Output\',MontH,'\',FileName];
    
    %______________________________________________________________________
    % 2. Recalculate Raw Factors and all Forecasts
    [Vt Dt] = eig(full(cov(Xe)));                                           % Eigenvector Decomposition; Vt=eigenvector, Dt=eigenvalues
    Ft   = Xe * Vt;                                                         % Factors
    Ct   = Ft * Vt';                                                        % Common component (X-C)=error term
    Errt = Xe -Ct;                                                          % Error term (because of the reduced rank of X)
    forecast = [Date_f, fcstj, yf, fcstX];
    
    %______________________________________________________________________
    % 3. Save to Excel-Workfile
    
    disp('_____________________________________________________________');
    disp('Saving output to Excel-file ')
    disp(FileName)
    delete(FileName);
    d     = {'Ýear','Month','Y_M','Y_Q','Xs'};                              % Sheet with forecasts of Y's
    xlswrite(FileName,d                      ,'forecasts','A1');
    xlswrite(FileName,forecast               ,'forecasts','A2');
    
    d =     {'Ýear','Month','Xs'};                                          % Sheet with forecasts of X's
    xlswrite(FileName,d                      ,'Xs','A1');
    xlswrite(FileName,DateX(1:size(Fd,1),:)  ,'Xs','A2');
    xlswrite(FileName,Xe                     ,'Xs','C2');
    
    d =     {'Ýear','Month','Factors'};                                     % Sheet with factors
    xlswrite(FileName,d                      ,'factors','A1');
    xlswrite(FileName,DateX(1:size(Fd,1),:)  ,'factors','A2');
    xlswrite(FileName,Ft                     ,'factors','C2');
    
    d =     {'Ýear','Month','Common Components'};                           % Sheet with common components
    xlswrite(FileName,d                      ,'common components','A1');
    xlswrite(FileName,DateX(1:size(Fd,1),:)  ,'common components','A2');
    xlswrite(FileName,Ct,'common components' ,'C2');
    xlswrite(FileName,DateX(1:size(Fd,1),:)  ,'error term','A2');
    xlswrite(FileName,Errt,'error term'      ,'C2');
    xlswrite(FileName,Vt,'eigenvectors'      ,'A2');
    xlswrite(FileName,Dt,'eigenvalues'       ,'A2');
    xlswrite(FileName,cov(Xe),'cov. matrix'  ,'A2');
end


%__________________________________________________________________________
% Save complete Datafile
if saveFile ==1;
    FileName = ['DFROG ' Country ' ' ext,YCode];
    FileName = [Base_Dir,'Output\',MontH,'\',FileName];
    
    disp('_____________________________________________________________');
    disp('Saving dynamic factor model output to Matlab-file ')
    disp(FileName)
    delete([FileName '*.*'])
    save(FileName)
end

toc