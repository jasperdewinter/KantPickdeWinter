function R= SKF(Ym,Model,Par)
%______________________________________________________________________
% USAGE R = SKF(Ym,Model,Par)
% Kalman filter for stationary systems with time-varying system matrices
% and missing data.
% Version for block correlations in matrix G*G'
%
% The model is        y_t   = Z(t) * a_t  + c1(t) + G(t) * eps_t
%                     a_t+1 = T(t) * a_t  + c2(t) + H(t) * eps_t
%
% with H'G = 0  & diagonal matrix G G'
%
%                            I M P O R T A N T
%  A special expression is used to calculate the inverse of F_t, as
%  the dimension of the obs eq is high. This requires S.G*S.G' to be
%  of a certain structure. The first S.nd rows/cols are assumed to
%  be diagonal, whereas now assumptions are made on the second part.

%  If this is not the case, change the code below & compute
%  inv(F_t) as iF = inv(Z*P*Z' + G*G')
%______________________________________________________________________
% INPUT
%        Ym         Data                                 (nobs x n)
%        Model      Name of function to build SSF        (string)
%        Par        Parameter structure to build SSF
% OUTPUT
%        R.Am       Predicted state vector  A_t|t-1      (nobs x m)
%        R.AmU      Filtered state vector  A_t|t         (nobs x m)
%        R.Pm       Predicted covariance of A_t|t-1      (nobs x m x m)
%        R.PmU      Filtered covariance of A_t|t         (nobs x m x m)
%        R.Q        Sum of v'F^{-1}v
%        R.SF       v' inv(F) v
%        R.loglik   Value of likelihood function
%______________________________________________________________________
% a) SKF uses a function S = Model(Par,S), which builds the state space
%    form. Par is a structure containing Parameter values and other
%    arbitrary information. Structure S contains matrices {Z, T, G, H}
%    plus initial conditions A1 and P1 = cov(A1) for the state vector.
%    Use S=[] for the 1st call of Model and update S thereafter.
%    Model is passed to SKF as a text string
% b) Function MissData checks for missing series in obs i (Ym(i,:))
%    it reduces the observation equation accordingly. If observation i
%    is empty, a pure forecasting step is done. Otherwise the KF
%    recursion is applied to the reduced obs eq.
%______________________________________________________________________
% Initalise
eval(['S = ' Model '(Par,[],0);'])                             % this part calls DFM_ECB(Par,[],0 (See DFM_ECB for details; it build the SSF)
if size(S.Z,1) ~= size(Ym,2)
    disp(['Data vector : ' num2str(size(Ym,2))]);
    disp(['Z           : ' num2str(size(S.Z,1))]);
    error('Data vector does not fit Z');
end

% Output structure & dimensions
[n m] = size(S.Z);                                             % size of S.Z (first mat in eq (6)
nobs  = size(Ym,1);                                            % # of rows in Ym

% Preallocatie memory
R.sF    = 0;
R.Q     = 0;
R.Am    = nan*zeros(nobs,m);   R.Pm  = nan*zeros(nobs,m,m);      % size of Am  # of variables, size of Pm  # of variables, # of variables
R.AmU   = nan*zeros(nobs,m);   R.PmU = nan*zeros(nobs,m,m);      % size of AmU # of variables, size of PmU # of variables, # of variables     
R.Ptest = nan*zeros(nobs,m,m);                                   % check-array, not necessary for filter
R.Atest = nan*zeros(nobs,m);                                     % check-array, not necessary for filter

% Check whether the first Par.Ds block of S.G*S.G' is diagonal
%  G_chk = S.G(1:S.nd,1:S.nd)*S.G(1:S.nd,1:S.nd)';
%  G_chk = sum(abs((sum(abs(G_chk))' - diag(G_chk))));
%  if G_chk > 1e-5
%      R = '1st block of S.G*S.G'' is not diagonal: type help SKF'
%      return
%  end


%________________________________________________________________________________________________
A  = S.A1;                                                          % Assigning initial values to A
P  = S.P1;                                                          % Assigning initial values to P
Au = zeros(size(A));

for t = 1:nobs                                                      % Starting the loop
    % Important to understand what is exactly in the matrices R.Am en R.Pm
    R.Am(t,:)   = A;                                                % A_t|t-1 = A
    R.Pm(t,:,:) = P;                                                % P_t|t-1 = P
    
    %_________________________________________________________________________________________________
    % Obtain SSF and D= inv(GG')
    % Rebuild DFM using S and time t. Diff with initial model call is you now
    % put in values for S and a value for the cumulator
    %_________________________________________________________________________________________________
    %
    eval(['S = ' Model '(Par,S,' num2str(t) ');']);
    [y,Z,G,c1,L] = MissData(Ym(t,:)',S);                            % Missdata is a special command
    
    % Prediction equations
    %_____________________
    if isempty(y)
        Au = A;                                                     % Au = At-1   
        Pu = P;                                                     % Pu = Pt-1
        A  = S.T*A      + S.c2;                                     % A  = A_t|t-1 
        P  = S.T*P*S.T' + S.H*S.H';                                 % P  = P_t|t-1
        iF = zeros(n,n);                                            % Set iF-Matrix to zero when no new information in y(i).
        K  = zeros(m,n);                                            % Set K-Matrix to zero when no new information in y(i).
    else
        
        % Compute inv(F_t)
        % Necessary for caluclation of Kalman Gain: K_t=T_t*P_t*Z_t*F_t(-1)
        PZ = P*Z';                                                  % create first part of F
        GG = G*G';                                                  % create second part of F
        
        if sum(sum(GG-diag(diag(GG)))) > 0 | sum(find(diag(GG)==0)) > 0
            iF  = inv(Z*PZ + GG);                                   % p. 67 D&K, see notes 
        else
            D   = diag(1./diag(GG));                                % okay; trick
            iF  = D - D*Z*inv(eye(m)+PZ*D*Z)*PZ*D;                  % okay; trick
        end
        if sum(sum(abs(iF*(Z*P*Z'+G*G') - eye(size(iF))))) > n^2*1e-6 % okay; trick
            error('Inversion of matrix F in did not work')          % okay; trick
            return
        end
        
        % Kalman gain K_t
        PZF = PZ*iF;                                                % create first part of Kalman Gain  
        K   = S.T*PZF;                                              % create Kalman gain (see notes)              
        
        % Au = A_t|t   & Pu = P_t|t                           
        V   = y - Z*A  -  c1;                                       % y arrives, calculate forecast error
        
        % Contemporaneous filtering equations
        Au  = A  + (PZF * V);                                       % Au = A_t|t
        Pu  = P  - (PZF * PZ');                                     % Pu = P_t|t  
        
        % Updating equations
        % A  = A_t+1|t & P  = P_t+1|t
        A   =  S.T*A + K*V + S.c2;                                  % update A
        P   = (S.T-K*Z)*P*S.T' + S.H*S.H';                          % update P
        P   =  0.5 * (P+P');                                        % okay; trick; check for symmetry (?)
        
        % Likelihood
        R.Q  = R.Q  + V'*iF*V;                                 
        R.sF = R.sF - log(det(iF));                                 % R.sf was zero in first call
        
        % Restore structure of iF and K
        iF   = L*iF*L';                                             % uses the L-matrix from  MissData                      
        K    = K*L';                                                % uses the L-matrix from  MissData
    end
    
    R.AmU(t,:)     = Au;                                                               
    R.PmU(t,:,:)   = Pu;
    R.Ptest(t,:,:) = P;   
    R.Atest(t,:,:) = A;   
    R.iF(t,:,:)    = iF;
    R.K(t,:,:)     = K;
     
end % t

% Likelihood
R.loglik = 0.5 * (R.sF + R.Q);