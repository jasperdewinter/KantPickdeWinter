function [q_opt stat critval s] = Estim_DFP(E,n,method,C_flag);
%__________________________________________________________________________
% function [q_opt stat critval s] = Estim_DFP(E,n,method,C_flag);
% Estimates the number of dynamic factors from Bai & Ng (2005), 
% 'Determining the number of primitive shocks in factor models'
%
% INPUT
% E            Residuals from VAR in estimated static factors F    [T x r]
% n            Nr of series, from which F have been derived
% method       Type of statistics to be calculated                 (1/2)
% C_flag  = 1  Use correlation matrix 
%         = 0  Use covariance matrix
%
% OUTPUT
% q_opt        Estimated value of q
% stat         Test statistics for q = 1:Nr of static factors-1  [r-1 x 1]
% critval      Critical value of test
% s            Eigenvalues                                         [r x 1]
% 
% m            Threshold for critical values (standard values depend 
%                                             on method & C_flag)
% delta        Exponent in test statistics                           [0.1]
%__________________________________________________________________________
  [nobs r] = size(E);
  
%___________________________________________________
% Covariance / correlation matrix of E  
  if C_flag
     E = z01(E);        % standardize error covariance matrix
  end
  C = cov(E);           % calculate covariance 
        
% Obtain eigenvalues 
  [u s v] = svd(C);     % singular value decomposition of C
   s      = diag(s);    
   EV     = s.*s;
   s_EV   = sum(EV);

%___________________________________________________
% Test statistics & critical values
  stat   = zeros(r-1,1);
  switch method
      case 1
        for q = 1:r-1
            stat(q) = sqrt(EV(q+1))        / sqrt(s_EV);
        end
      case 2  
        for q = 1:r-1
            stat(q) = sqrt(sum(EV(q+1:r))) / sqrt(s_EV);
        end
  end
  
%___________________________________________________
% Critical value
  delta  = 0.1;
  m      = 1.0;
  if C_flag 
     if method == 1
       m = 1.25; 
     else  
       m = 2.25; 
     end
  end
  critval = m/min([n^(.5-delta);nobs^(.5-delta)]);
    
%___________________________________________________
% Obtain nr of factors (q)  
  q_opt   = size(find(stat >= critval),1) + 1;

