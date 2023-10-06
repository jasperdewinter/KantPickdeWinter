function S = DFM_ECBm(P,S,t)
%__________________________________________________________________________
% function S = DFM_ECBm(P,S,t)
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
%      S must contain {Z(t), G(t), T(t), H(t), c1(t), c2(t)}
%__________________________________________________________________________
%      !!!     This model version handles m-o-m growth rates     !!!
%__________________________________________________________________________
%
% The obs   vector is [monthly data - GDP ] 
% The state vector is [factors  mom-GDP mom-GDP(1) 3m-GDP - aggregator]
%__________________________________________________________________________
  [n m]    = size(P.C);

% Update matrices in repeated call
% Set the aggregator  
  if ~isempty(S) 
      
    TT            = zeros(m+4,m+4);
    TT(1:m,1:m)   = P.A;
    TT(m+2,m+1)   = 1; 
    TT(m+3,m+2)   = 1; 

    if monOfQ(P.Date(t,2)) ~= 3
      TT(m+4,m+4) = 1; 
    end  
   
    T0            =  eye(m+4);
    T0(m+1,1:P.r) = -P.beta(2:end)';
    T0(m+3,m+1)   = -1;
    T0(m+3,m+2)   = -1;
    T0(m+4,m+3)   = -1/3;
    S.T           =  inv(T0) * TT;

    return
  end    
  
%__________________________________________________________________________
% Obs eq
  S.Z           = zeros(n+1,m+4);
  S.Z(1:n,1:m)  = P.C;
  S.Z(n+1,m+4)  = 1;
 
  S.c1          = zeros(n+1,1);
  S.c1(n+1)     = P.beta(1);

  S.G           = zeros(n+1,n+m+1);
  S.G(1:n,1:n)  = chol(P.R);

%__________________________________________________________________________
% Trans eq 
  TT            = zeros(m+4,m+4);
  TT(1:m,1:m)   = P.A;
  TT(m+2,m+1)   = 1; 
  TT(m+3,m+2)   = 1; 

  if (t > 1) & monOfQ(P.Date(t,2)) ~= 3
      TT(m+4,m+4) = 1; 
  end
  
  T0            =  eye(m+4);
  T0(m+1,1:P.r) = -P.beta(2:end)';
  T0(m+3,m+1)   = -1;
  T0(m+3,m+2)   = -1;
  T0(m+4,m+3)   = -1/3;
    
  S.H           =  zeros(m+4,m+1);
  S.H(1:m,1:m)  =  P.H;
  S.H(m+1,m+1)  =  sqrt(9/19)*P.s;
  S.H           =  [zeros(m+4,n) S.H];
  S.c2          =  zeros(m+4,1);
 
  S.T           =  inv(T0) * TT;
  S.H           =  inv(T0) * S.H;
  S.c2          =  inv(T0) * S.c2;  
  
%__________________________________________________________________________
% Initial condition
  S.A1  = zeros(size(S.T,1),1);
  S.P1  = InitCov(S.T,S.H*S.H');  

  