function [X,Date,s_NaN] = BalanceX(X,Date,shift,k);
%__________________________________________________________________________
% [X,Date,s_NaN] = BalanceX(X,Date,shift,k);
%
% Balances T x n data matrix X bei either of two methods. 
% Data X is unbalanced if there are (different) numbers of NaN at the end
% of sample (eos). The data are assumed to contain no other NaNs
%
% The adjustment is done in 2 steps
% 1)  Eliminate all series with > k missing observations at end-of-sample.
% 2)  shift = 0: Shift all remaining series by k observations
%     shift = 1: The series are shifted individually -> if data are missing 
%                from x(t-m+1) onwards, then replace x(t) with x(t-m)
%     shift = 2: Forecast missing observations from a univariate AR(p) 
%         
% In all cases the length of the date vector is adjusted such that the 
% final date remains unchanged
%
% OUTPUT
% X       Balanced X
% Date    Corresponding date (Final date remans unchanged) 
%                            (- 1st obs are trimmed)
% s_NaN   n x 1 vector:  size of shift for each series (if shift = 1) 
%__________________________________________________________________________
 if shift < 0
    s_NaN = zeros(size(X,2));
    return
 end
        
% Spot the nr of consecutive NaN's at eos 
  [T,n]  = size(X);
 
  X_NaN  =  isnan(X);
  s_NaN  = (cumsum(flipud(X_NaN)) == repmat((1:T)',1,n));
  s_NaN  =  sum(s_NaN,1);
    
% Eliminate series with more then k consec Nans
  X     =    X(:,[s_NaN <= k]); 
  s_NaN =  s_NaN([s_NaN <= k]);

% Check for data left
  [T,n]  = size(X);
  if n == 0
     error('No series left after balancing - increase k')
     return
  end   
  
% Shift the remainder by either of 3 methods
  if     shift == 0
         X     = X(1:end-k,:); 
         Date  = Date(k+1:end,:);
         s_NaN = k*ones(size(X,2));
         
  elseif shift == 1
         for   i = 1:n
               X(s_NaN(i)+1:T,i) = X(1:T-s_NaN(i),i);
         end
         X     =   X(max(s_NaN)+1:end,:);
         Date  =   Date(max(s_NaN)+1:end,:);

  elseif shift == 2
         C.IC    = 'SIC';
         C.lags  = 6;
         C.cflag = 1;
         for i = 1:n
             h = s_NaN(i);
             X(:,i) = V_AR(X(1:T-h,i),C,h);
         end    
  else  
         error('Parameter shift must be set to 1,2, or 3')
  end
    
