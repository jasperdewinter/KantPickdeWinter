function S = DFM_ECB(P,S,t)
%__________________________________________________________________________
% function S = DFM_ECB(P,S,t)
% Creates state space form from parameter structure P. The model is
%
%             y_t   = S.Z * a_t  + S.c1 + S.G * eps_t  eps_t ~ WN(0,I_q)    
%             a_t+1 = S.T * a_t  + S.c2 + S.H * eps_t       
%
% with S.H'S.G = 0 and initial conditions A1 and cov(A1) = P1.
%
% INPUT
% P    Structure that contains all parameters & other info 
% S    Structure that contains the state space form
%                use S = [] for the 1st call of this function in SKF
% OUTPUT
% S    Structure that contains the SSF for time t
%      S must contain {Z(t), G(t), T(t), H(t), c1(t), c2(t),P1, A1}
%__________________________________________________________________________
% This here ...
%
% The obs   vector is [monthly data - GDP ] 
% The state vector is [factors - monthly GDP - aggregator]
%__________________________________________________________________________
  [n m]    = size(P.C);

% Update matrices in repeated call
% Set the aggregator  
  if (~isempty(S)) ; 
    TT          = zeros(m+2,m+2);
    TT(1:m,1:m) = P.A;
      
    if monOfQ(P.Date(t,2)) ~= 3
       TT(end,end) = 1; 
    end

    T0            =  eye(m+2);
    T0(m+2,m+1)   = -1/3 ;
    T0(m+1,1:P.r) = -P.beta(2:end)';
    S.T           =  inv(T0) * TT;

     return
   end    

%__________________________________________________________________________
% Obs eq
  S.Z              = zeros(n+1,m+2);
  S.Z(1:n,1:m)     = P.C;
  S.Z(n+1,end)     = 1;
 
  S.c1         = zeros(n+1,1);
  S.c1(n+1)    = P.beta(1);

  S.G          = zeros(n+1,n+m+1);
  S.G(1:n,1:n) = chol(P.R);
% S.G(n+1,n+m+1) = P.s;
  
%__________________________________________________________________________
% Trans eq 
  TT          = zeros(m+2,m+2);
  TT(1:m,1:m) = P.A;
      
  if (t > 1) & monofQ(P.Date(t,2)) ~= 3
      TT(end,end) = 1; 
  end
  
  T0            =  eye(m+2);
  T0(m+2,m+1)   = -1/3;
  T0(m+1,1:P.r) = -P.beta(2:end)';
  
  S.c2          = zeros(m+2,1);
  
  S.H           = zeros(m+2,m+1);
  S.H(1:m,1:m)  = P.H;
  S.H(m+1,m+1)  = P.s * sqrt(3);
  S.H           = [zeros(m+2,n) S.H];
    
  S.T   =  inv(T0) * TT;
  S.H   =  inv(T0) * S.H;
  S.c2  =  inv(T0) * S.c2;  
  
%__________________________________________________________________________
% Initial condition
  S.A1  = zeros(size(S.T,1),1);
  S.P1  = InitCov(S.T,S.H*S.H');  
