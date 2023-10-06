function [Y A const Sig IC] = V_AR(Y,P,h)
%__________________________________________________________________________
% [Y A const Sig AIC] = V_AR(Y,lags,h)
% Estimates an vector autoregressive model Y = c + A(L)*Y(-1) + u from OLS
% and produces forecasts over h steps
%
% INPUTS
%  Y             Series                                        (nobs x n)
%  P.cflag       = 1 -> Add constant to VAR 
%
%  P.IC         Info crit for lag length selection
%                  'AIC'  Akaike
%                  'SIC'  Schwartz
%                  'FIX'  fixed lag length
%
%  P.lags          if IC = 'AIC', 'SIC' Maximum nr of lags in VAR                             (integer)
%                  if IC = 'FIX'        Nr of lags in VAR
%
%  h           Nr of steps in forecasting                    (integer)
%
% OUTPUT
%  Y          Series extended with dynamic forecast h steps  (nobs+h x n)
%  A          Coefficient matrices                           (n x n x lags)
%  const      Estimated constant                             (n x 1)
%  Sig        Covmat of residuals                            (n x n)
%  IC         Akaike / Schwartz information criteria         (P.lags x 1)
%__________________________________________________________________________
 [nobs n] = size(Y);

% Checks
  if sum(strcmp(P.IC,upper({'AIC','SIC','FIX'}))) == 0 
     error('Input P.IC is invalid - use AIC, SIC or FIX')
  end
  if P.lags <= 0
     error('Lags must be positive integer')
  end
  if n*P.lags + P.cflag > nobs/2
     P.lags = floor((nobs/2-P.cflag)/n);
     P.lags = max(1,P.lags);
     disp(['V_AR: P.lags is cut to ' num2str(P.lags) ': too few obs'])
  end    
  
% Estimate
  if strcmp(P.IC,upper('FIX'))
     lags  = P.lags;
     [b IC A const Sig] = Estimate_VAR(Y,P.lags,lags,P.cflag,'');
  else
     for i = 1:P.lags
         [bi ICi] = Estimate_VAR(Y,P.lags,i,P.cflag,P.IC);
         IC(i)    = ICi;
     end
     [IC_min lags]       = min(IC);
     [b ICi A const Sig] = Estimate_VAR(Y,P.lags,lags,P.cflag,P.IC);
  end
  

% Dynamic Forecasts
  for s = 1:h
      Z = lagmatrix(Y,1:lags);
      
      Z_nxt = [Y(end,:) Z(end,1:n*(lags-1))];
      if  P.cflag
          Z_nxt = [Z_nxt 1]; 
      end
      
      Y_nxt  = Z_nxt * b;
      Y = cat(1,Y,Y_nxt);
  end    
  
  
 function [b IC A const Sig] = Estimate_VAR(Y,cut,lags,cflag,Iflag)
%______________________________________________________________________________
% Estimates the above VAR for a fixed lag number
% INPUTS
%  Y               Series                                        (nobs x n)
%  cut             cut first cut obs in data
%  lags            Nr of lags in VAR
%  cflag           = 1 -> Add constant to VAR 
%  Iflag           Info crit for lag length selection
%______________________________________________________________________________

% Create lags & add constant
  Z = lagmatrix(Y,1:lags);
  if  cflag
      Z = [Z ones(size(Z,1),1)];
  end
  Y = Y(cut+1:end,:);
  Z = Z(cut+1:end,:);
  
% Clear all obs with missing data
  ix    = ~isnan(sum([Y Z],2));
  Yc    =  Y(ix,:);
  Zc    =  Z(ix,:);  
 
% Estimate beta & resids
  iZc   = inv(Zc'*Zc);
  b     = iZc * Zc' * Yc;
  U     = Y - Z*b;
  Sig   = cov(U(ix,:));
  
% Calculate AIC / SIC
  [nobs n] = size(Yc);
   if     strcmp(upper(Iflag),'AIC')
          IC  = log(det(Sig)) + (n^2*lags + n*cflag) * (2/nobs); 
   elseif strcmp(upper(Iflag),'SIC')    
          IC  = log(det(Sig)) + (n^2*lags + n*cflag) * (log(nobs)/nobs); 
   else
          IC  = nan; 
   end    
          
% Coefficent matrices  
  A     = nan(n,n,lags);
  for j = 1:lags
      A(:,:,j) = b((j-1)*n+1:j*n,:);
   end
 
  if cflag 
     const = b(lags*n+1,:)';
  else
     const = [];
  end    
  
     
     
         