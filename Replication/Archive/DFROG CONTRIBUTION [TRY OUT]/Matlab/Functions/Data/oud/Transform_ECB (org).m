function Xc = Transform_ECB(X,Code,Year)
%________________________________________________________________________________
% function Xc = Transform_ECB(X,Code)
% Transforms the euro area data according to input code 
% (version for STFC project)
% INPUT
%       X        : Panel of raw data                      (nobs x N)
%       Code     : Vector containing transformation codes (N x 3)
%                  - col 1: take logs (Yes = 1)
%                  - col 2: Degree of differening (1 or 2) 
%                  - col 3: degree of one-sided moving average filter
%                           x(t) + x(t-1) + ... + x(t-col3)
% OUTPUT    
%       Xc       : Panel of transformed series            (nobs x N)
%________________________________________________________________________________
  Xc  = nan*zeros(size(X));
  n   = size(X,1);
  
  for j = 1:size(X,2)
      k = 0;
      z = X(:,j);
          
    % log           
      if Code(j,1) == 1                     
         z = log(z);
      end       
     
    % Differencing
      for i = 1:Code(j,2)
          ni = size(z,1);
          z  = (z(2:ni)-z(1:ni-1)); 
          k  = k + 1;
      end
      
    % Filter
      if Code(j,3) > 0
         m   = Code(j,3); 
         z2  = filter(ones(1,m)/m,1,z);
         z   = z2(m:end); 
         k   = k + m - 1;
      end
           
   % Add leading NaNs  
     z      = [nan*ones(k,1);z];
     Xc(:,j) = z;
 
  end
