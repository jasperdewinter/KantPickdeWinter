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
  [nobs,r] = size(F);
   p       = Par.p;

%_____________________________________________________
% Find optimal lag using function V_AR
  if Par.crit > 0
     VP.lags   = p;
     VP.cflag  = 1;             % add constant to VAR
     VP.IC     = 'AIC';         % Use Akaike Selection Criterion for # of lags in VAR
     [V0 V1]   = V_AR(F,VP,0);  % Serie=F,VP contains lags, cflag=1 indicates to add a constant term to the AR, IC indicates to use AIC criterium, 0 indicates forecast horizon is zero
                                % V0=F series extended with forecasts,
                                % V1=Coefficient matrices. 
     p         = size(V1,3);    % The loop inside V_AR determines the optimal # of lags, and these are saved in p
  end
  
%_____________________________________________________
% Stacked AR regression F(t) = A*F(t-1) + e(t)
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
  A1           = inv(Z'*Z)*Z'*z;            % Coefficient Matrix    
  E            = z  - Z*A1;                 % Error
  A(1:r,1:r*p) = A1';
  
%_____________________________________________________
% Find optimal q as from Bai & Ng (2005)
  if Par.crit == 0
      q = min([Par.q r]);
  else    
      q = Estim_DFP(E,n,Par.qflag,1);
      disp(['Nr of dynamc factors = ' num2str(q)]);
      disp(['Nr of lags           = ' num2str(p)]);
  end   
  
%_____________________________________________________
% Form reduced rank covariances Q = H*H' from resids E
  H          = zeros(p*r,p*r);
  Q          = zeros(p*r,p*r);
  Q(1:r,1:r) = cov(E);
  
  if q > 1
      [D M] = eig(full(cov(E)));            % M-eigenvalues, D= eigenvectors
      D     = D(:,end-q+1:end);
      D     = fliplr(D);
      D     = D*diag(sign(D(1,:)));
      M     = M(end-q+1:end,end-q+1:end);
      M     = rot90(M);
      M     = fliplr(M);

  else
      D = 1;
      M = q;
  end
  H(1:r,1:q) = D*sqrt(M);
  Q(1:r,1:r) = D*M*D';

%_______________________________________________________
  Par.A  = A; 
  Par.H  = H;
  Par.Q  = Q;
  Par.p  = p;
  Par.q  = q;
  