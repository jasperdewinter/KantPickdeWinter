function y = EM_step(data,f,nbpcode)
%__________________________________________________________________________
% function y = EM_step(data,f,nbpcode)
%
% One step of the Em algortihm 
% Imputes the missing values into non-balanced-panel of vectors. 
% The 'NaN's are also forecasted backwards and forewards with the use
% of principal components. The method used here takes into account the fact
% that input data might be a stock or a flow with degree of integration 
% of either one or two. For the reference see Chow & An-loh Lin, 
% "Best linear unbiased interpolation, distribution and extrapolation...".
% The input data consists of a column vactor with missing values, denoted 
% with, 'data', a matrix 'f' of pre-selected principal components and a 
% vector of transformation code for the non-balanced-panel data, called
% 'nbpcode'. The values of 'nbpcode' denote as follows:
% 0 - there are no missing values (& hence the serie was not included;
%     if the series doesn't contain NaNs this value is irrelevant).
% 1 - incomplete monthly data; simple ols imputation for all observations
% 2 - quarterly data; underlying monthly variable is an I(0) stock 
% 3 - quarterly data; underlying monthly variable is an I(1) stock 
% 4 - quarterly data; underlying monthly variable is an I(0) flow 
% 5 - quarterly data; underlying monthly variable is an I(1) flow 
% 
% INPUT 
% data      T x n matrix, non-balanced panel
% f         T x facn matrix, balanced panel of principal components 
% nbpcode   Transformation code (see above)
%
% OUTPUT
% y          T x n matrix, balanced panel
%
% Warning: there should be no missing quarterly data unless at the end of
% the sample
%__________________________________________________________________________

  if nargin ~= 3
      error('3 Inputs are required')
      return
  end    
  if (size(f,2)<1) || size(f,1)==1
      error('Input F must be a column vector or a matrix of column vectors')
      return
  end
  if size(data,2)~=1||size(data,1)<2
     error('Input Data must be column vector') 
     return
  end
  if (size(nbpcode,2)~=1)||(size(nbpcode,1)~=1)
     error('Input nbpcode must be a scalar')
     return
  end    
 
  d1 = data(~isnan(data),1);
  n1 = size(data,1); 
  n2 = size(d1,1);
 
  if     nbpcode == 1
       % Simple OLS imputation for all observations.
         f1  = f(~isnan(data),:);
         lam = flamcal1(d1,f1);
         e   = d1-(f1*lam); 
         ssr = e'*e;
         y   = GR_replace(data,f*lam,1);            
        
  elseif nbpcode==2
       % Underlying monthly variable is an I(0) stock.
         A     = A1(n2);
         lam   = flamcal2(d1,f(1:3*n2,:),A);
         uhat  = d1-A*f(1:3*n2,:)*lam;
         y     = f(1:3*n2,:)*lam+A'*inv(A*A')*uhat;
         if n1 > 3*n2
            y = [y; f(3*n2+1:n1,:)*lam];
         end

  elseif nbpcode==3
       % Underlying monthly variable is an I(1) stock.
         A     = A3(n2);
         lam   = flamcal2(d1,f(1:3*n2,:),A);
         uhat  = d1-A*f(1:3*n2,:)*lam;
         y     = f(1:3*n2,:)*lam+A'*inv(A*A')*uhat;
         if n1 > 3*n2
            y = [y; f(3*n2+1:n1,:)*lam];
         end

  elseif nbpcode==4
       % Underlying monthly variable is an I(0) flow.
         A     = (1/3)*A3(n2);
         lam   = flamcal2(d1,f(1:3*n2,:),A);
         uhat  = d1-A*f(1:3*n2,:)*lam;
         y     = f(1:3*n2,:)*lam+A'*inv(A*A')*uhat;
         if n1 > 3*n2
            y = [y; f(3*n2+1:n1,:)*lam];
         end

  elseif nbpcode==5
       % Underlying monthly variable is an I(1)flow.
         A              = (1/9)*A9(n2);
         A              = A(:,1:size(A,2)-2);
         A(:,size(A,2)) = (3/2)*A(:,size(A,2));
         
         lam   = flamcal2(d1,f(1:3*n2,:),A);
         uhat  = d1-A*f(1:3*n2,:)*lam;
         y     = f(1:3*n2,:)*lam+A'*inv(A*A')*uhat;
         if n1 > 3*n2
            y = [y; f(3*n2+1:n1,:)*lam];
         end

  end
       
  