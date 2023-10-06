function [fcst Date_f P] = RUN_GDFM_B(X,y,Date,fcstH,P)
%__________________________________________________________________________
% [y_f,Date_f P] = RUN_GDFM_B(X,y,Code Date,fcstH,P)
%
% Produces estimates and forecasts for the Belgian GDFM
% Takes monthly input data X(1:t,:) and GDP series y(1:t) with possibly
% missing data and produces the forecasts y(t-1:t+fcstH)
% a) Balances the monthly data to the end of the quarter
% b) Aggregates XB to quarterly values XQ 
% c) Balances XQ again to align GDP
% d) Runs the forecasts
%
% Unbalancedness is accounted for by shifting the data (see BalanceX) 
% ir by using the EM algorithm
%
% INPUTS
%  X        Panel of monthly time series possibly unbalanced     (nobs x n)
%  y        Quarterly GDP (ext to monthly freq!)                 (nobs x 1)
%           (months 1 & 2 = NaN | quarterly data in mon 3)
%  Date     corresponding to X and y                             (nobs x 2)
%
%  cutE     Nr of observations to cut in estimation from eos
%  fcstH    Forecast horizon (quarters)
%  P        Structure of parameters (see main program)
%           p       Nr of static factors
%           q       Nr of dynam  factors
%           nn      Size of Bartlett window
%           k       Nr of obs to cut if missing              (see BalanceX)
%           shift   Flag whether to balance data by shifting (see BalanceX)
%
% OUTPUTS
%  y_f        Forecast                                        (fcstH+2 x 1)
%  Date_f     corresponding to y_f                            (fcstH+2 x 2)
%__________________________________________________________________________
  [nobs n] = size(X);
  Q        = P;
  Q.nn     = max(3,ceil(sqrt(nobs)/4));
  Q.qmax   = floor(sqrt(nobs/3));             % Adjust q for short sample
 
%_________________________________________________________
% Extend monthly data to end of current quarter
  [XA DateA] = TrimData(X,Date,Date(1,:),mon2qrt(Date(end,:)),'M');

% Create balanced panel
  if P.shift == 3
     XA           = Outliers(XA,0);
    [XB DateB c]  = Run_EM(XA,DateA,3,3);
  else
     [XB DateB c] = BalanceX(XA,DateA,P.shift,P.k+3-monOfQ(Date(end,2)));
      XB          = Outliers(XB,P.OC);
  end
  
%_____________________________________________________
% Aggregate to quarterly frequency
  [yQ DateQ] = m2q(y,Date,'3');
  [XQ DQ]    = m2q(XB,DateB,Q.C3);
   XQ        = TrimData(XQ,DQ   ,DateQ(3,:),DateQ(end,:),'Q');
  [yQ DateQ] = TrimData(yQ,DateQ,DateQ(3,:),DateQ(end,:),'Q');
  
%_________________________________________________________
% Balance the data incl GDP (ie shift GDP)
   XQ          = [yQ XQ]; 
  [XQ Date2 c] = BalanceX(XQ,DateQ,1,99);

%_________________________________________________________
% Find optimal number of static factors
  if Q.crit  > 0
      FQ      = Estim_SPC(z01(XQ),max(Q.r),Q.crit);
      Q.r     = size(FQ,2);
      Q.qflag = 2;
  end
  
%_____________________________________________________
% Find all combinations of (r q nn)
  if Q.crit  == 0;
     ParSet  = combvec(Q.r,Q.q);
     lst     = find(ParSet(1,:) >= ParSet(2,:));
     ParSet  = ParSet(:,lst);
  else
     ParSet  = [size(FQ,2); max(Q.q)];
  end
  
%_________________________________________________________
% Forecasting
% Determine forecast range
  endF  = mon2qrt(Add2Date(DateQ(end,:),3*fcstH));
  h     = DateDiff(endF,DateQ(end-c(1),:)) / 3;

% Main loop  
  fcst  = [];
  for j = 1:size(ParSet,2)  
    % Set parameters
      Q.r   = ParSet(1,j);
      Q.q   = ParSet(2,j);
      Q.q   = min(Q.q,Q.qmax);                  % !! TAKE CARE !! %
      
    % Find nr of dynamic factors  
      if Q.crit  > 0
         Q.qflag = 2;
         Q.p     = 3;
         Q       = Estim_PC(FQ,Q,n);
      end
      
    % Predict common component
      y_f =  nan(h,1);
      for j = 1:h
          f      = GDFM_BE(XQ,Q.q,max(Q.nn,j),Q.r,j);
          y_f(j) = f(1);
      end

    % Adjust for possibly known previous quarter GDP
      if ~isnan(yQ(end-1))
           y_f     = [XQ(end,1); y_f];
           Date_f  = GenDates(DateQ(end-c(1),:),endF,'M');
      end
      
    % Store  
      fcst = [fcst y_f];
  end
  
  Date_f = GenDates(DateQ(end-c(1)+1,:),endF,'M');
  if ~isnan(yQ(end-1))
      Date_f  = GenDates(DateQ(end-c(1),:),endF,'M');
  end
  
%_________________________________________________________
% Store 
  if P.crit > 0
     P.ParSet = [max(P.r); max(P.q)];
     if isfield(P,'r_opt')
        P.r_opt = [P.r_opt; size(FQ,2)];
        P.q_opt = [P.q_opt; Q.q]; 
        P.p_opt = [P.p_opt; Q.p];
     else
        P.r_opt = size(FQ,2);
        P.q_opt = Q.q;
        P.p_opt = Q.p;
     end
  else
     P.ParSet = ParSet;
 
  end
   