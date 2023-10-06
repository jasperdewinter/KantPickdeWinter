function [fcst Date_fcst P R_KF] = RUN_DFM_ECB(X,y,Date,fcstH,P)

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
%  y        Quarterly GDP (ext to monthly freq!)                 (nobs x 1)
%           (months 1 & 2 = NaN | quarterly data in mon 3)
%  Date     corresponding to X and y                             (nobs x 2)
%
%  fcstH    Forecast horizon (quarters)
%  P        Structure of Pameters, in this case
%             crit    Method for spec of static factors (help Estim_SPC)
%             qflag   Method for spec of dynamc factors (help Estim_DFP)
%             r       Nr of static factors
%             q       Nr of dynamic factors
%             p       Nr of lags
%                     If P.crit > 0 then [r p q] define the maximum values
%                     in search
%             cutE    Nr of observations to cut in estimation from eos
%
% OUTPUTS
%  fcst       Forecast                                        (fcstH+2 x 1)
%  Date_fcst  corresponding to fcst                           (fcstH+2 x 2)
%  P          Pameters extended with estimation results
%  R_KF       Detailed KF output (structure - see help SKF & help FIS)
%__________________________________________________________________________

% Do not change original input P - use Q
  Q = P;
  n = size(X,2);
  
%_____________________________________________________
% Static principal components
   Xe        = Outliers(z01(X(1:end-Q.cutE,:)),2);
  [F V D]    = Estim_SPC(Xe,max(Q.r),Q.crit);
   FQ        = filter([1/3 1/3 1/3],1,F);
   FQ(1:2,:) = nan;
    
%_____________________________________________________
% Find all combinations of (r q p)
  if Q.crit  == 0;
     ParSet   = combvec(Q.r,Q.q,Q.p);
     lst      = find(ParSet(1,:) >= ParSet(2,:));
     ParSet   = ParSet(:,lst);
  else
     ParSet   = [size(F,2); 0; max(Q.p)];
     Q.qflag  = 2; 
  end

%_____________________________________________________
% Prepare data for KF: extend to end of forecast
% horizon and set outliers to nan   
  endF        = mon2qrt(Add2Date(Date(end,:),3*fcstH));

  Xf          = TrimData(X, Date,Date(1,:),endF,'M');
 [yf Date_f]  = TrimData(y, Date,Date(1,:),endF,'M');
  Xf          = Outliers(z01(Xf),0);
  Q.Date      = Date_f;
 
%_____________________________________________________
% Main loop
  fcst  = [];
  for j = 1:size(ParSet,2)  
    % Set parameters
      Q.r   = ParSet(1,j);
      Q.q   = ParSet(2,j);
      Q.p   = ParSet(3,j);
     
    % Estimate dynamic model
      Fj    = F(:,1:Q.r);
      Vj    = V(:,1:Q.r);
      Q     = Estim_PC(Fj,Q,n);
      Q.C   = [Vj zeros(n,Q.r*(Q.p-1))];		
      Q.R   = diag(diag(cov(Xe-Fj*Vj')));
     [Q.beta Q.s] = OLS(y(3:end-P.cutE,:),FQ(3:end,1:Q.r),1,0);
      
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
      R_KF  = SKF([Xf yf],Q.Model,Q);
      R_KF  = FIS([Xf yf],Q.Model,Q,R_KF);
      R_KF  = SEC([Xf yf],Q.Model,Q,R_KF,1);
      fcstj = R_KF.signal(:,end);
 
    % Store
      fcst  = [fcst fcstj(end-(fcstH+1)*3:3:end,:)];
  end
  Date_fcst = Date_f(end-(fcstH+1)*3:3:end,:);

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
