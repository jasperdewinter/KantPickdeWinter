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
%
%
% S    Structure that contains the state space form
%      use S = [] for the 1st call of this function in SKF
% 
% Transition/State equation (See Banbura and Runstler, 2010, p. 4)
%  S.T0  first matrix;                  eq. (7)
%  S.TT  first matrix on the right;     eq. (7)
%  S.T   S.T=inv(T0)*TT; Rewriting of   eq. (7)
%  S.H   last vector on the right;      eq. (7)
%  S.c2  zero-matrix (empty vector in current setup)
%  S.c1  zero-matrix (vector containing the constant term)
%
% Observation equation
%  S.Z   first matrix on the right;     eq. (6)
%  S.G   cholesky decomposition of R 
%        diagonal covariance matrix
%        for disturbance term in        eq. (1) 
%  
% Initial conditions
%  S.A1  initial value a (=0)
%  S.P1  initial value P (=taken from Lutkepohl)
%
%
% OUTPUT
% S    Structure that contains the SSF for time t
%      S must contain {Z(t), G(t), T(t), H(t), c1(t), c2(t),P1, A1}
%__________________________________________________________________________
% This here ...
%
% The obs   vector is [monthly data - GDP ] 
% The state vector is [factors - monthly GDP - aggregator]
%__________________________________________________________________________
  [n m]    = size(P.C);                         % size matrix C

% Update matrices in repeated call
% Building matrix S.t
% Set the aggregator  
if (~isempty(S)) ;
    TT          = zeros(m+2,m+2);               % create matrix TT 
    TT(1:m,1:m) = P.A;                          % put matrix A into TT. This is the matrix A(i) of eq. 2 in B&R (2011)
    if monOfQ(P.Date(t,2)) ~= 3                 % monOfQ gives the month in the quarter (creating the cumulator variable of eq. 7 in 
        TT(end,end) = 1;                        % If the month does not equal three the cumulator variable equals 1
    end
    
    T0            =  eye(m+2);                  % identity matrix
    T0(m+2,m+1)   = -1/3 ;                      % Put 1/3 in the matrix
    T0(m+1,1:P.r) = -P.beta(2:end)';            % Put Beta (matrix) inside matrix T0 (starting on position 2; the coefficients of the factors)
    S.T           =  inv(T0) * TT;              % Bring T0 to the right side of the equation, which defines S.T
    
    return
end

%__________________________________________________________________________
% Obs eq
  S.Z              = zeros(n+1,m+2);            % S.Z
  S.Z(1:n,1:m)     = P.C;                       % P.C matrix of eigenvectors (see RUN_DFM_ECB)
  S.Z(n+1,end)     = 1;                         % add a 1
 
  S.c1             = zeros(n+1,1);              % create S.c1
  S.c1(n+1)        = P.beta(1);                 % first beta = constant (zie S.c1 in intro)

  S.G              = zeros(n+1,n+m+1);          % S.G
  S.G(1:n,1:n)     = chol(P.R);                 % Choleski decompositie van P.R =Q.R =m, diagonal covariancematrix of the errors in eq. 1 B&R (2011)
% S.G(n+1,n+m+1) = P.s;
  
%__________________________________________________________________________
% Trans eq 
  TT          = zeros(m+2,m+2);                 % idem to S.T         
  TT(1:m,1:m) = P.A;                            % idem to S.T
      
  if (t > 1) & monOfQ(P.Date(t,2)) ~= 3         % (t>1) toegevoegd
      TT(end,end) = 1; 
  end
  
  T0            =  eye(m+2);                    % idem aan S.T
  T0(m+2,m+1)   = -1/3;                         % idem aan S.T
  T0(m+1,1:P.r) = -P.beta(2:end)';              % idem aan S.T
  
  S.c2          = zeros(m+2,1);                 % create zero matrix for S.C2. In current setup this vector is always empty
  
% Create error-term in eq(7) B&R (2010)
  S.H           = zeros(m+2,m+1);               % 
  S.H(1:m,1:m)  = P.H;                          % P.H: the errors (see Estim_PC; error-term in eq(7) B&R, first row)
  S.H(m+1,m+1)  = P.s * sqrt(3);                % P.s: Cholesky decomposition of covariance matrix; see error-term in eq(7) B&R, second row);
  S.H           = [zeros(m+2,n) S.H];           % zeros plus a piece of errors
    
  S.T   =  inv(T0) * TT;                        % identical to the above with slide change in TT
  S.H   =  inv(T0) * S.H;                       % S.H. need to be premultilpied with inv(T0), because you take it to the right hand side of eq. 7 B&R (2011) [only premultiplied and not premultiplied (squared) because you took the Choleski Decomposition of the covariance matrix = normal (co)variance rules]
  S.c2  =  inv(T0) * S.c2;                      % S.c2 (for the same reason) need to be premultiplied by T0; this is still zero, because S.c2 is a vector of zeros.
  S.T0  =  T0;
  S.TT  =  TT;
%__________________________________________________________________________
% Initial condition
  S.A1  = zeros(size(S.T,1),1);                 % intitiële waarde A1=0
  S.P1  = InitCov(S.T,S.H*S.H');                % inititalisatie strategie voor het Kalman filter (afkomstig van Lutkepohl, D&K hanteren andere initialisatie)
