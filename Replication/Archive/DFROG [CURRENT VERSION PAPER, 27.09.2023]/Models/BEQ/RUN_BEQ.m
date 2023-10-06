function [fcst Date_fcst P] = RUN_BEQ(X,y,Date,fcstH,P);
%__________________________________________________________________________
% function [fcst Date_fcst P] = RUN_BEQ(Y,X,Date,fcstH,P)
%
% Produces estimates and forecasts for a bridge equation.
% a) Monthly data are forecast from either a RW or a VAR 
% b) Monthly data are AVERAGED over the quarter 
% c) The bridge equation is estimated 
% d) GDP growth is forecast from the bridge equation
%
%  INPUTS
%  y        Quarterly GDP growth (in monthly format)             (nobs x 1)
%           (set months 1 & 2 to NaN & insert the quarterly
%           number at month 3)
%  X        Panel of monthly time series                         (nobs x k)
%  Date     corresponding to y and X                             (nobs x 2)
%
%  fcstH    Forecast horizon (quarters)
%  P        Structure of Pameters 
%     lags  Nr of lags of X in bridge equation
%     Mlag  Nr of lags in monthly VAR
%
%  OUTPUTS
%  fcst       Forecast                                           (nobs x k)
%  Date_fcst  corr to fcst
%  P          Pameters extended with estimation results
%             In this case (beta,s) Ps from regression of GDP on X
%__________________________________________________________________________

% Estimate bridge equation
%  X = Outliers(X,P.OC); 
  [P.beta, P.s lags] = Estim_BEQ(y(1:end-P.cutE,:),X(1:end-P.cutE,:), ...
                                                Date(1:end-P.cutE,:),P);
                            
% Extend data to new end of forecast horizon   
   endF     = mon2qrt(Add2Date(Date(end,:),3*fcstH));
   X        = TrimData(X, Date,Date(1,:),endF,'M');
  [y Date]  = TrimData(y, Date,Date(1,:),endF,'M');
  [nobs k]  = size(X);

% _________________________________________________________________________
%                          Forecast monthly series
% _________________________________________________________________________
  P.V.cflag = 1;
  
  switch P.Mon 
  
  case {'RW'}    
  % Random walk fcst 
  % Find last non-NaN in X(:,i) and extend  
    for j = 1:k
        i = nobs;
        while isnan(X(i,j)); 
              i = i-1; 
        end
        X(i+1:nobs,j) = X(i,j);
    end
    
  case {'VAR'}
  % AR or VAR fcst of X
  % Find last non-NaN in X & cut data missing data at end 
    i   = nobs;
    while sum(isnan(X(i,:))) > 0 
          i = i-1; 
    end
    h   = nobs - i;
    if h > 0
       XF          = V_AR(X(1:i,:),P.V,h);
       X(isnan(X)) = XF(isnan(X));
    end
  
  case {'AR'} 
  % Univariate AR fcsts of X 
  % Perhaps better in case of different publication lags
  % Find last non-NaN in X & cut data missing data at end 
    for j = 1:size(X,2)
        i   = nobs;
        while isnan(X(i,j)) 
          i = i-1; 
        end
        h   = nobs - i;
        if h > 0
           XF                 = V_AR(X(1:i,j),P.V,h);
           X(isnan(X(:,j)),j) = XF(isnan(X(:,j)));
        end  
    end
  end
   
  
% _________________________________________________________________________
%                               Forecast GDP
% _________________________________________________________________________
% Aggregate to quarterly freq
  [XQ, DateQ] = m2q(X, Date,P.C3);
   yQ         = m2q(y, Date,'3');
   
% Form lags & add constant 
  [nobs k]  = size(XQ);
  XQ        = [ones(nobs,1) lagmatrix(XQ,0:lags)];
  fcst      =  XQ * P.beta; 

% Replace with existing data  
  fcst(~isnan(yQ)) =  yQ(~isnan(yQ));
  fcst             =   fcst(end-(fcstH+1):end);
  Date_fcst        =  DateQ(end-(fcstH+1):end,:);
  