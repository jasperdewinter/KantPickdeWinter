function Par = Estim_PC(F,Par,n)
%__________________________________________________________________________
% function Par = Estim_PC(F,Par,n)
% Estimates the factor model
%
%         X_t = C F_t            +   xi_t,       xi_t ~ WN(0,R)
%         F_t = Sum_p(A_i F_{t-i}) +  u_t,        u_t ~ WN(0,Q)
%
% where the 2nd eq is a VAR of order p and with rank(Q) = r
%
% INPUT
%  F               Estimates of factors                          [nobs x r]
%  Par.r           (Max) Nr of static factors
%  Par.qflag       Method for finding the nr of dyn factors
%                    = 0 use P.q (no estimation)
%                    = 1 estimate from Test 1 (Bai & Ng, 2005)
%                    = 2 estimate from Test 2 (Bai & Ng, 2005) 
%  Par.q           Nr of dyn factors   (only with P.qflag = 0)                           
%  Par.p           Nr of lags in factor VAR   
%  n               Nr of series in original data (used for static PC)
%
% OUTPUT
%   Par            Extended with parameter matrices A Q H &
%                  possibly adjusted values of q & p (qflag > 0)                  
%__________________________________________________________________________
% Explanation
%
% Most of the time (if you use the Bai an Ngi criteria for selection purposes)
% The input to this function consists of the matrix Q, that dates back to
% the matrix P. This matrix contains P.r, P.qflag, P.q and P.p as is stated
% in the setup RUN_DFM_ECB
%__________________________________________________________________________

[nobs,r]   = size(F);               % # obs and r: the # of static factors
   p       = Par.p;                 % p: max # of lags in factor VAR

%_____________________________________________________
% Find optimal lag using function V_AR
  if Par.crit > 0                   % goes back to RUN_DFM_ECB: criterion for selection crit>0 implies use cri. Bai and Ngi (2002)          
     VP.lags   = 3;                 % max. # of lags in VAR
     VP.cflag  = 1;                 % adds constant to VAR
     VP.IC     = 'AIC';             % selection criterion for VAR    
     [V0 V1]   = V_AR(F,VP,0);      % F(actors) are the input series, VP contains specifics, 0 means no forecasting
     p         = size(V1,3);        % p= het optimaal aantal lags dat volgt uit V_AR
  end
  

%_______________________________________________________________________
%Using the optimal lag_structure from V_AR do
%the whole thing again and put it in the matrix A (see notes for what A
%looks like)
%_______________________________________________________________________

% Do a stacked AR regression F(t) = A*F(t-1) + e(t)
  I  = eye(r*p);    
  A  = [   zeros(r,r*p)  ; ... 
        I(1:end-r,1:end) ];    
      
% Form expl vars Z for AR regression
% Estimate stacked F(t) = A*F(t-1) + e(t)  
  Z = [];
  for i = 1:p
      Z = [Z F(p-i+1:end-i,:)];
  end
  z            = F(p+1:end,:);             
  A1           = inv(Z'*Z)*Z'*z;          
  E            = z  - Z*A1;             % matrix containing e(t)s in above equation
  A(1:r,1:r*p) = A1';                   % creating matrix A equal to A1'
  
%_____________________________________________________
% Find optimal q as from Bai & Ng (2005)
  if Par.crit == 0
      q = min([Par.q r]);
  else    
      q = Estim_DFP(E,n,Par.qflag,1);                   % input errorvariance of VAR in DFP
      disp(['Nr of dynamc factors = ' num2str(q)]);     % output of DFM, number of dynamic factors
      disp(['Nr of lags           = ' num2str(p)]);     % output of DFM, number of lags 
  end   
  
%_____________________________________________________
% Form reduced rank covariances Q = H*H' from resids E
  H          = zeros(p*r,p*r);
  Q          = zeros(p*r,p*r);
  Q(1:r,1:r) = cov(E);

  if q > 1 
     d.disp  = 0;
     [D M]   = eigs(cov(E),q,'lm',d);   % see help funtion eigs, q= q eigenalues, 'lm'=largest eigenvalues,  d="disp:0"; e.i.: return the q largest eigenvalues and display nothing, D=eigenvectors, M= eigenvalues
     D       = D*diag(sign(D(1,:)));    % change signs (not really clear)
  else
     D = 1;
     M = q;
  end    
  H(1:r,1:q) = D*sqrt(M);               % eigenvectors * sqaured root of the eigenvalues (the errors; see DFM_ECB)
  Q(1:r,1:r) = D*M*D';                  % eigenvectors * eigenvalues * eigenvectors

%_______________________________________________________
  Par.A  = A; 
  Par.H  = H;
  Par.Q  = Q;
  Par.p  = p;
  Par.q  = q;
  