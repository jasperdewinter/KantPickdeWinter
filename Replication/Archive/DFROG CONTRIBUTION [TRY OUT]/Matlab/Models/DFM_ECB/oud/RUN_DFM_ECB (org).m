function [fcst Date_fcst P R_KF Q W S] = RUN_DFM_ECB(X,y,Date,fcstH,P)

%__________________________________________________________________________
% [fcst Date_fcst P R_KF] = RUN_DFM_ECB(X,y,Date,fcstH,P)
%
% Produces estimates and forecasts for the ECB DFM with 3-mon growth rates
% Takes monthly input data X(1:t,:) and GDP series y(1:t) with possibly
% missing data and produces the forecasts y(t-1:t+fcstH)
%
%
% INPUTS
%  X        Panel of monthly time series possibly unbalanced     (nobs x n)
%  Xe       Standardized X's als input voor factormodel
%  Xf       Standardized and trimmed X's as input for Kalman filter
%  y        Quarterly GDP (ext to monthly freq!)                 (nobs x 1)
%           (months 1 & 2 = NaN | quarterly data in mon 3)
%  yf       Standardized an trimmed y's as input for Kalman filter
%  Date     corresponding to X and y                             (nobs x 2)
%
%  fcstH    Forecast horizon (quarters)
%  P        Structure of Pameters, in this case
%             crit    Method for spec of static factors (help Estim_SPC)
%             qflag   Method for spec of dynamic factors (help Estim_DFP (wordt aangeroep in ESTIM_PC)
%             r       Nr of static factors (# of factors, Barhoumi e.a.,
%                     p. 598 determined by recursive RMSE criterion)
%             q       Nr of dynamic factors (# q innovations in errors,
%                     Barhoumi e.a., p. 599)
%             p       Nr of lags (# of lags in y equation, Barhoumi e.a.,
%                     p.599 formula (8)
%                     If P.crit > 0 then [r p q] define the maximum values
%                     in search
%             cutE    Nr of observations to cut in estimation from eos
%             Model   Type of model. In this case this should (always) be
%                     DFM_ECB
%  (OUTPUT from Estim_SPC.m)
%   F         estimated static factors (monthly frequency)
%   FQ        estimated static factors (quarterly frequency)
%   V         estimated eigenvectors
%   D         estimated eigenvalues
%
% OUTPUTS
%  fcst       Forecast                                        (fcstH+2 x 1)
%  Date_fcst  corresponding to fcst                           (fcstH+2 x 2)
%  P          Pameters extended with estimation results
%  R_KF       Detailed KF output (structure - see help SKF & help FIS)
%
%  (OUTPUt from SKF.m)
%        	R_KF.SF       	v' inv(F) v
%        	R_KF.Q        	Sum of v'F^{-1}v
%        	R_KF.Am       	Predicted state vector  A_t|t-1    (nobs x m)
%        	R_KF.Pm       	Predicted covariance of A_t|t-1    (nobs x m x m)
%        	R_KF.AmU      	Filtered  state vector  A_t|t      (nobs x m)
%        	R_KF.PmU      	Filtered  covariance of A_t|t      (nobs x m x m)
%           R_KF.iF         Inverse of F-matrix (F_t)
%           R_KF.K          Kalman Gain (K_t)                  (nobs x m x (n+1)
%        	R_KF.loglik     Value of likelihood function
%           R.A             last A-matrix (not essential, just for checking, see ReadMe and notes)                           
%           R.P             last P-matrix (not essential, just for checking, see ReadMe and notes)
%           R.Atest         check on how A-matrix changes into R.Ptest
%                           ("column --> row", see ReadMe and notes, page 6)
%           R.Ptest         check on how P-matrix changes into R.Ptest
%                           ("column --> row", see ReadMe and notes, page 6)
%
%  (OUTPUT from FIS.m)
%           R_KF            Smoothed estimates added to above
%          	R_KF.AmT        Estimates     a_t|T                (nobs x m)
%           R_KF.PmT        P_t|T       = Cov(a_t|T)           (nobs x m x m)
%           R_KF.Pstar      C-matrix
%		where m is the dim of state vector and t = 1 ...T is time
%
%  (OUTPUT from SEC.m)
%          	R_KF.signal     signal for data based on (nobs x n)
%          	R_KF.sign_P     standard deviation of signal
%           S               model parameters
%
%           y_t   = S.Z * a_t  + S.c1 + S.G * eps_t  eps_t ~ WN(0,I_q)    
%           a_t+1 = S.T * a_t  + S.c2 + S.H * eps_t       
%
%           Transition/State equation (See Banbura and Runstler, 2010, p. 4)
%           S.T0  first matrix; eq. (7)
%           S.TT  first matrix on the right; eq. (7)
%           S.T   S.T=inv(T0)*TT; Rewriting of eq. (7)
%           S.H   last vector on the right; eq. (7)
%           S.c2  zero-matrix
%
%           Observation equation
%           S.Z   first matrix on the right; eq. (6)
%           S.G   cholesky decomposition of R diagonal covariance matrix for disturbance term in eq(1)
%  
%           Initial conditions
%           S.A1  initial value a (=0)
%           S.P1  initial value P (=taken from Lutkepohl) 
%__________________________________________________________________________

% Do not change original input P - use Q
Q = P;
n = size(X,2);

%_____________________________________________________
% 1. Static principal components
Xe        = Outliers(z01(X(1:end-Q.cutE,:)),2);   			% remove outliers from standardized data (function z01). Use x's from 1 to end minus
% Q.cutE (equaling P.cutE, see above
[F V D]   = Estim_SPC(Xe,max(Q.r),Q.crit);       			% Calculate Static Principal Components on standardized & cleaned X's, F= estimated vectors, V= eigenvvectors
FQ        = filter([1/3 1/3 1/3],1,F);            			% Calculates average of last three months (1/3,1/3,1/3)
FQ(1:2,:) = nan;                                  			% Set first two rows equal to nan

%_____________________________________________________
% 2. Set the Parameterset
if Q.crit  == 0;                                            % Als Q.crit=0 dan geen zoekcriterium hanteren
    ParSet   = combvec(Q.r,Q.q,Q.p);                        % combineer de het aantal statische, dynamische en lag lenth p in een vector
    lst      = ParSet(1,:) >= ParSet(2,:);                  % check op # dynamische en statische factoren
    ParSet   = ParSet(:,lst);                               % als q>=r dan ParSet=[]
else
    ParSet   = [size(F,2); 0; max(Q.p)];                    % Parset matrix, rij 1= aantal statische factoren, rij 2= aantal dynamische factoren =0, rij 3=maximum aantal vertragingen in VAR
    Q.qflag  = 2;                                           % In ESTIM_PC kies je coor Bai & Ngi (2005) criterium vor keuze # dynamische factoren
end

%_____________________________________________________
% 3. Prepare data for KF: extend to end of forecast
%    horizon and set outliers to nan
endF         = mon2qrt(Add2Date(Date(end,:),3*fcstH)); 		% Add2Date simple function that adds date at the end of the sample
Xf           = TrimData(X, Date,Date(1,:),endF,'M');   		% Trim X-Matrix from Date(1,:), endF, using monthly frequency
[yf Date_f]  = TrimData(y, Date,Date(1,:),endF,'M');   		% Trim y as well
Xf           = Outliers(z01(Xf),0);                    		% Remove outliers after Xf is standardized
Q.Date       = Date_f;                                 		% Quarterly Dates are Date_f

%_____________________________________________________
% 4. Main loop
fcst  = [];
for j = 1:size(ParSet,2)                                    % Run the model for j=1 (j is always 1 in our case sinze size (ParSet,2) always equals 1)
    % Set parameters
    Q.r   = ParSet(1,j);                                              % number of static factors  (= "old"Q.r)
    Q.q   = ParSet(2,j);                                              % number of dynamic factors (= 0 (if Q.crit n.e. 0)
    Q.p   = ParSet(3,j);                                              % number of lags            (= "old" Q.p)
    
    % Estimate dynamic model 
    Fj    = F(:,1:Q.r);                                               % F are the factors         (= "old" # of static factors)
    Vj    = V(:,1:Q.r);                                               % V are the eigenvectors    (=  "old" # of eigenvectors)
    Q     = Estim_PC(Fj,Q,n);                                         % Find # of dynamic factors and matrix B and I(q) (Ba?bura and Rünstler, p. 3)
    Q.C   = [Vj zeros(n,Q.r*(Q.p-1))];                                % matrix of eigenvectors horizontally concatenated to zero matrix
    Q.R   = diag(diag(cov(Xe-Fj*Vj')));                               % diagonal covariance matrix of multivariate white noise term in eq (1) of Ba?bura and Rünstler, eq (3), p. 4)
    [Q.beta Q.s] = OLS(y(3:end-P.cutE,:),FQ(3:end,1:Q.r),1,0);        % OLS equation relation y(t) to f(t) creating Q.beta (beta's in eq.) and Q.s (Cholesky decomposition of covariance matrix)
    
    % Correct for a too small variance of the idiosyncratic component
    % Otherwise the KF would break down
    if min(diag(Q.R)) < 1e-2
        LL         = find(diag(Q.R)<1e-2);
        for i = 1:length(LL)
            Q.R(LL(i),LL(i)) = 1e-2;
        end
        disp(['RUN_DFM_ECB: elements of matrix R adjusted to 1e-2'])
        disp(num2str(LL))
    end
    
    % Kalman filter & smoother
    % Remember: you put in Xf as well as tht yf following Ba?bura and Rünstler
    % (2010), which enables weight extraction.
    R_KF     = SKF([Xf yf],Q.Model,Q);                              % Calculate outcome Kalman Filter by function SKF and put outcome in R.struct
    R_KF     = KIF([Xf yf],Q.Model,Q);                              % Calculate full Kalman Gain and F matrices (for later use in weights)
    R_KF     = FIS([Xf yf],Q.Model,Q,R_KF);                         % Calculate outcome Kalman Smoother by function FIS and put outcome in R.struct
    [R_KF S] = SEC([Xf yf],Q.Model,Q,R_KF,1);                       % Calculate signal from Kalman Filter by function SEC and put outcome in R.struct (1=smoothed estimates; 0=filtered estimates)
    fcstj    = R_KF.signal(:,end);                                  % Calculate forecasts for GDP by taking the last column of the R_KF.signal matrix
    W        = KFWeights(R_KF,Q.Model,Q,'smooth');                  % Calculate weights from Kalman Filter    
    
    % Store
    fcst  = [fcst fcstj(end-(fcstH+1)*3:3:end,:)];               % Store forecasts
end
Date_fcst = Date_f(end-(fcstH+1)*3:3:end,:);                     % Create Dates forecasts

% Percentage of explained variance
Par.VF    = sum(diag(D))/sum(diag(cov(Xe)));
P.ParSet  = ParSet;

% Store optimal specification
if P.crit > 0
    if isfield(P,'r_opt')
        P.r_opt = [P.r_opt; size(F,2)];
        P.q_opt = [P.q_opt; Q.q];
        P.p_opt = [P.p_opt; Q.p];
    else
        P.r_opt = size(F,2);
        P.q_opt = Q.q;
        P.p_opt = Q.p;
    end
end