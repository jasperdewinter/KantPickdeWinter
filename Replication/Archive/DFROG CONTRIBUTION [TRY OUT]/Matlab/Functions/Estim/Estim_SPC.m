function [F,V,D,IC1] = Estim_SPC(X,r_max,jj);
%__________________________________________________________________________
% function [F,V,D,IC1] = Estim_SPC(X,r_max,jj);
% Estimate static principal components
% Different methods for estimating the number of static factors
%
% X      Data matrix  (standardised & outlier corrected!)        [T x N]
% r_max  Max nr of factors 
% jj     Choice of method to estimate nr of factors
%        The most important:
%        jj = 0   Use r_max for nr of factors (no estimation)
%        jj = 2   Use information criterion proposed by Bai & Ng (2002)
%                        r*log(min(N,T)) * (N+T)/(N*T)
%        jj = 8   Find r such that the sum of eigenvals from 1 to r > 0.5
%        Other values of jj (1,3:7) utilise different information criteria 
%        (see below)
%
% OUTPUT
% F                Estimated factors                             [T x r]
% V                Eigenvectors                                  [N x r]
% D                Eigenvalues                                   [r x r]
% IC1              Value of criterion
%__________________________________________________________________________
% Check for nan
  if sum(sum(isnan(X))) > 0
     error('X contains missing values')
     return
  end   

% Correction factor CT for # factors  
  [T, N]  = size(X);
  NT      = N*T;
  NT1     = N+T;
  mNT     = min([N;T]);
  
  ii      = 1:r_max;
  if jj  == 1;  CT = ii*log(NT/NT1)* NT1/NT;     end; %  PC_p1
  if jj  == 2;  CT = ii*log(mNT)   * NT1/NT;     end; %  PC_p2
  if jj  == 3;  CT = ii*log(mNT)   / mNT;        end; %  PC_p3
  if jj  == 4;  CT = ii*2/T;                     end; %  AIC_1
  if jj  == 5;  CT = ii*log(T)/T;                end; %  BIC_1
  if jj  == 6;  CT = ii*2*NT1/NT;                end; %  AIC_3
  if jj  == 7;  CT = ii*log(NT)*NT1/NT;          end; %  BIC_3
    
  
%__________________________________________________________________________
% Eigenval decomp of cov(X) = VDV', only r_max largest eigenvalues
if jj == 8
    [V D] = eig(cov(X));
else
%       d.disp = 0;
%       [V D]  = eigs(cov(X),r_max,'lm',d);
    [V D] = eig(full(cov(X)));
    V     = V(:,end-r_max+1:end);
    V     = fliplr(V);
    D     = D(end-r_max+1:end,end-r_max+1:end);
    D     = rot90(D);
    D     = fliplr(D);
end

    
F  = X * V;                                 % Factors or Principal Components
C  = F * V';                                % Common component (X-C)=error term
  
%__________________________________________________________________________
% Select the number of factors
  IC1   = [];
 
% No selection
  if jj == 0
     r_opt = r_max; 

% Information criteria   
  elseif jj < 8
     IC1   = zeros(1,r_max+1);
     Sigma = zeros(1,r_max+1); 
 
     for i = r_max:-1:1
         Ci       = F(:,1:i)*(V(:,1:i))';
         Xi       = X - Ci;
         Sigma(i) = mean(sum(Xi.*Xi/T));
         IC1(i)   = log(Sigma(i)) + CT(i);
     end
     Sigma(r_max+1) = mean(sum(X.*X/T));
     IC1(r_max+1)   = log(Sigma(r_max+1));
     
     [mv r_opt]     = min(IC1(1:r_max)');
     
% Breakpoint in eigenvalues
  elseif jj == 8
      sumEval = flipud(cumsum(diag(D))/sum(diag(D)));
      r_opt   = size(find(sumEval >= 0.5),1);
  end

  if jj ~= 0
     disp(['Nr of static factors = ' num2str(r_opt)]);
  end
  
%__________________________________________________________________________
% Trim output 
  F   =  F(:,1:r_opt);
  V   =  V(:,1:r_opt);
  D   =  D(1:r_opt,1:r_opt);
  
%__________________________________________________________________________
% To check: call the original code
% [ic_check, chat,Fhat,eigval]=ICP(X,r_max,jj,0);
%
% This uses a singular value decomposition of XX = X*X' (sic!), i.e.
% XX             = X*X';
% [F0a,EVal,F1a] = svd(X*X');
% Fi             = F0a(:,1:i);
% Xi             = X - Fi * Fi' * X;

