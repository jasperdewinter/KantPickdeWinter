function X_lag = lagx(X,lags)
%__________________________________________________________________________
% function X_lag = lagx(X,lags)
% Takes the T x n data matrix 'X' and the matrix 'lags ', which that
% contains the information about all included lags.
%
% Each column of 'data' matrix (we assume that columns contain 
% the original data series) is lagged according to the info conveyed by 
% the 'lags' matrix. The initial length of each of the series is 
% preserved as missing observations are substituted with 'NaN's.
% Then all the lags are concatenated. The output is 'laggeddata'
% matrix which consists of all the variables and all their lags.
%__________________________________________________________________________

  if nargin ~= 2
    error('2 input matrices are required')
    return
  end  
  if size(X,2)==0||size(lags,2)==0||size(X,1)==0||size(lags,1)==0
     error('Both data matrix and lags matrix have to be non-empty')
     return
  end      
  if size(X,2)~=size(lags,2)
     error('The number of columns in both matrices must be equal')
     return        
  end
  
  flag = 0;
  if size(X,1) == 1 && size(X,2) ~= 1
     X    = X';
     flag = 1;
  end
            
  X_lag  = []; 
  [m n]  = size(lags); 
   o     = size(X,1);
            
   for i = 1:n
       for j = 1:m
           if lags(j,i) ~= -1
              z   = lag(X(:,i),lags(j,i));
              add = nan*zeros(o,1);
                        
              add(lags(j,i)+1:o,:) = z;
              X_lag                = [X_lag add];
           end
       end
   end

   if flag == 1
      X_lag = X_lag';
   end    
  