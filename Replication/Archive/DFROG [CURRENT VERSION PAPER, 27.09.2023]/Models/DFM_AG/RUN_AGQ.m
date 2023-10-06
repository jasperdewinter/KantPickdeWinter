function [fcst Date_fcst P] = RUN_AGQ(X,y,Date,fcstH,P)

%__________________________________________________________________________
% [fcst,Date_fcst,P] = RUN_AGQ(X,y,Date,fcstH,P)
%
% Produces estimates and forecasts for the DFM by Agostino & Giannone
% The model works as follows:
% (a) Data X are balanced against end of current quarter 
% (b) Balanced X are aggregated
% (c) Static principal components is run on shifted XQ 
%     The nr of factors may be estimated following Bai & Ng (2002)
% (d) OLS of y(t+h) on factors
% (e) For the backcast Q(-1), X is balanced against the previous quarter
%
% INPUTS
%  X        Panel of monthly time series possibly unbalanced     (nobs x n)
%  y        Quarterly GDP (ext to monthly freq!)                 (nobs x 1)
%           (months 1 & 2 = NaN | quarterly data in mon 3)
%  Date     corresponding to X and y                             (nobs x 2)
%
%  cutE     Nr of observations to cut in estimation from eos
%  fcstH    Forecast horizon (quarters)
%  P        Structure of Pameters, in this case
%             C3     Type of quarterly aggregation  'M' = m-o-m   rates 
%                                                   'A' = 3-month rates
%             OC     Type of outlier correction in data (1,2)
%             crit   Criterion for selecting the number of factors
%                    (see 'help Estim_SPC' for details)
%             r      (Maximum) nr of static factors                  [1 x l]
%             shift  Type of balancing (1 or 2)
%                    (see 'help BalanceX' for details on k & shift)
%             k      Eliminate series with more than k Nan at eos  
%
% OUTPUTS
%  fcst       Forecast                                        (fcstH+2 x m)
%  Date_fcst  corresponding to fcst                           (fcstH+2 x 2)
%  P          Parameters extended with estimation results
%__________________________________________________________________________

%__________________________________________________________________________
%                               FORE- & NOWCAST
%__________________________________________________________________________
% Extend monthly data to end of current quarter 
  [XA DateA]  = TrimData(X,Date,Date(1,:),mon2qrt(Date(end,:)),'M');

% Create balanced panel (up to end of current quarter)
  if P.shift == 3
      XA          = Outliers(XA,0);
     [XB DateB c] = Run_EM(XA,DateA,3,3);
  else
     [XB DateB c] = BalanceX(XA,DateA,P.shift,P.k+3-monOfQ(Date(end,2)));
      XB          = Outliers(XB,P.OC);
  end

% Aggregate to quarterly frequency
  [yQ DateQ] = m2q(y,Date,'3');
  [XQ DQ]    = m2q(XB,DateB,P.C3);
  
% Balance again to end of quarter & get rid of early nan
   XQ        = TrimData(XQ,DQ   ,DateQ(3,:),DateQ(end,:),'Q');
  [yQ DateQ] = TrimData(yQ,DateQ,DateQ(3,:),DateQ(end,:),'Q');
   FQ        = Estim_SPC(z01(XQ),max(P.r),P.crit);

% Set possible values of r
  if P.crit == 0;   r_LIST = P.r;  
  else;             r_LIST = size(FQ,2);
  end

%_________________________________________________________
% Forecast  
  for i = 0:fcstH
  for j = 1:size(r_LIST,2)
      Fj          =  FQ(:,1:r_LIST(j));
      beta        =  OLS(yQ(i+1:end),Fj(1:end-i,:),1,0);
      fcst(2+i,j) = [1 Fj(end,:)] * beta;
  end
  end
  Date_fcst = GenDates(DateQ(end-1,:),Add2Date(DateQ(end,:),3*fcstH),'Q');

% Store  
  P.ParSet  = r_LIST;
  if P.crit > 0
     if isfield(P,'r_opt')
        P.r_opt = [P.r_opt; size(FQ,2)]; 
     else
        P.r_opt = size(FQ,2); 
     end
  end
  
  
%__________________________________________________________________________
%                                BACKCAST  
%           The same but with data cut to end of previous quarter
%__________________________________________________________________________
  cut  = monOfQ(Date(end,2));  
  X    =    X(1:end-cut,:);
  y    =    y(1:end-cut,:);
  Date = Date(1:end-cut,:);

  [XA DateA]  = TrimData(X,Date,Date(1,:),mon2qrt(Date(end,:)),'M');

% Create balanced panel
  if P.shift == 3
      XA          = Outliers(XA,0);
     [XB DateB c] = Run_EM(XA,DateA,3,3);
  else
     [XB DateB c] = BalanceX(XA,DateA,P.shift,P.k+3-cut);
      XB          = Outliers(XB,P.OC);
  end
 
% Aggregate, trim & run PCA  
  [yQ DateQ] = m2q(y,Date,'3');
  [XQ DQ]    = m2q(XB,DateB,P.C3);

   XQ        = TrimData(XQ,DQ   ,DateQ(3,:),DateQ(end,:),'Q');
  [yQ DateQ] = TrimData(yQ,DateQ,DateQ(3,:),DateQ(end,:),'Q');
   FQ        = Estim_SPC(z01(XQ),max(P.r),P.crit);
   
% Possible values of r
  if P.crit == 0;   r_LIST = P.r;  
  else;             r_LIST = size(FQ,2);
  end
 
% Backcast   
  for j = 1:size(r_LIST,2)
      Fj        = FQ(:,1:r_LIST(j));
      P.beta    = OLS(yQ,Fj,1,0);
      fcst(1,j) = [1 Fj(end,:)] * P.beta;
  end  
      
% Adjust for possibly known previous quarter GDP
  if ~isnan(yQ(end))
      fcst(1,:) = yQ(end);
  end   
  
  
%_________________________________________________________
% OLD VERSION WITH PCA DONE ON MONTHLY DATA
% Estimate principal components & aggregate to quarterly f
%!  F        = Estim_SPC(z01(XB),max(P.r),P.crit);
% [yQ DateQ] = m2q(y,Date,'3');
%  [FQ DQ]   = m2q(F,DateB,P.C3);
%! FQ        = TrimData(FQ,DateQ,DateQ(1,:),DateQ(end,:),'Q');

  