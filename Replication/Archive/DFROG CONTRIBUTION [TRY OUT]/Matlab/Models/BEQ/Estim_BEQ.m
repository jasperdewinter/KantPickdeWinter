function [beta s lags] = Estim_BEQ(y,X,Date,P)
%__________________________________________________________________________
% function [beta s lags] = Estim_BEQ(y,X,Date,P)
% Estimates the bridge equation
%
% INPUT
%   y            GDP data (monthly format)                      [nobs x 1]
%   X            Monthly indicators                             [nobs x k]
%   Date         Corresponding Date vector                      [nobs x 2]
%   P            Structure of Pameters
%                P.IC      Info crit to determine lags in X
%                            ('FIX'|'AIC'|'SIC')
%                P.lags    Lags in X                          [integer]
%
% OUTPUT
%   beta          Cofficients
%   s             Residual standard deviation
%   lags         (Possibly) optimised nr of lags
%__________________________________________________________________________

% Aggregate to quart freq
  XQ = m2q(X, Date,P.C3);
  yQ = m2q(y, Date,'3');

  [nobs n] = size(XQ);

  switch P.IC
    case{'FIX'}    
      [beta s] = OLS(yQ,lagmatrix(XQ,0:P.lags),1,0);
      lags     = P.lags;
    otherwise
    % IC_min = inf;
      ii     = 1:P.lags;
      if     strcmp(upper(P.IC),'AIC')
                 NC  = ((ii+1)*n+1)*(2/nobs); 
      elseif strcmp(upper(P.IC),'SIC')    
                 NC  = ((ii+1)*n+1)*(log(nobs)/nobs); 
      end    
 
      for i  = 1:P.lags 
          [beta s] = OLS(yQ,lagmatrix(XQ,0:i),1,0);
           IC(i)   = log(s^2) + NC(i);
      end
      [IC_m lags] = min(IC);
      [beta s]    = OLS(yQ,lagmatrix(XQ,0:lags),1,0);
  end

