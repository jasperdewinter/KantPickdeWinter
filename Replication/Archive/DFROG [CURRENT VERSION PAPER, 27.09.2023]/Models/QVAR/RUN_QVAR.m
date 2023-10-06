function [fcst Date_fcst P] = RUN_QVAR(X,y,Date,fcstH,P);
%__________________________________________________________________________
% function [fcst Date_fcst P] = RUN_QVAR(Y,X,Date,fcstH,P)
%
% Produces estimates and forecasts for a quarterly VAR but under the same
% information sets as used for the mixed frequency data
% a) Monthly data are aggregated over the quarter
%    If any monthly data are missing in the quarter, the same applies to 
%    the quarterly data
% b) The VAR is estimated & forecasts are produced for the missing data
%
%  INPUTS
%  X        Panel of monthly time series                         (nobs x k)
%  y        Quarterly GDP growth (in monthly format)             (nobs x 1)
%           (set months 1 & 2 to NaN & insert the quarterly
%           number at month 3)
%  Date     corresponding to y and X                             (nobs x 2)
%
%  cutE     Nr of observations to cut in estimation (ignored)
%  fcstH    Forecast horizon (quarters)
%  P        Structure of Pameters 
%           C3    Type of quarterly aggregation 'M'|'A'
%           IC    Lag length sel in VAR ('AIC'|'SIC'|'FIX')
%           lags  Fixed nr of lags in VAR ('FIX')       or
%                 Max   nr of lags for    ('AIC'|'SIC')
%
%  OUTPUTS
%  fcst       Forecast                                           (nobs x k)
%  Date_fcst  corresponding to fcst
%  P          Pameters extended with estimation results
%__________________________________________________________________________

% Form the data
  yQ         = m2q(y, Date,'3');
% X          = Outliers(X,P.OC);
 [XQ, DateQ] = m2q(X, Date,P.C3);
  ZQ         = [XQ yQ];
  nobs       = size(ZQ,1);
  
% Find index of missing data at eos & adjust
  i = nobs;
  while sum(isnan(ZQ(i,:))) > 0
        i = i -1;
  end      
  ZQ      = ZQ(1:i,:);
  fcstH2  = fcstH + (nobs-i);
  
% Run VAR
  P.cflag = 1;
  [ZQ P.A P.const P.Sig IC] = V_AR(ZQ,P,fcstH2);
  
% Expand yQ to end of fcst horizon and replace the fcst with
% the existing actual data in yQ
  fcst             =  ZQ(:,end);
   
  endF             =  mon2qrt(Add2Date(Date(end,:),3*fcstH));
  [yQ, DateQ]      =  TrimData(yQ, DateQ,Date(1,:),endF,'Q');
  fcst(~isnan(yQ)) =  yQ(~isnan(yQ));
  
  fcst             =   fcst(end-(fcstH+1):end);
  Date_fcst        =  DateQ(end-(fcstH+1):end,:);
  