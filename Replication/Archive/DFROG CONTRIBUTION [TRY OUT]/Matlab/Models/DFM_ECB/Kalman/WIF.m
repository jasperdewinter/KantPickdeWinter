function R= WIF(Ym,Model,Par)

% Initalise
eval(['S = ' Model '(Par,[],0);'])                              % this part calls DFM_ECB(Par,[],0 (See DFM_ECB for details; it build the SSF)
if size(S.Z,1) ~= size(Ym,2)
    disp(['Data vector : ' num2str(size(Ym,2))]);
    disp(['Z           : ' num2str(size(S.Z,1))]);
    error('Data vector does not fit Z');
end

% Dimensions 
[n m] = size(S.Z);                                              % size of S.Z (first mat in eq (6)
nobs  = size(Ym,1);                          
R.iFf = nan*zeros(nobs,n,n);                                    % Pre-allocate memory
R.Kf  = nan*zeros(nobs,m,n);                                    % Pre-allocate memory

P  = S.P1;                                                      % Assigning initial values to P2

for t = 1:nobs                                                  % Starting the loop
    %_________________________________________________________________________________________________
    % Obtain SSF and D= inv(GG')
    % Rebuild DFM using S and time t. Diff with initial model call is you now
    % put in values for S and a value for the cumulator
    %_________________________________________________________________________________________________
    %
    eval(['S = ' Model '(Par,S,' num2str(t) ');']);
    
    % create system matrices to calculate full K and iF matrices (Kf and
    % Iff)
    Z=S.Z;
    G=S.G;
    T=S.T;
    
    % Updates the P-matrix for nobs>1
    P  = S.T*P*S.T' + S.H*S.H';                            % P  = P_t|t-1
    
    % Compute inv(F_t)
    % Necessary for caluclation of Kalman Gain: K_t=T_t*P_t*Z_t*F_t(-1)
    PZ = P*Z';                                                    % create first part of F
    GG = G*G';                                                    % create second part of F
    
    if sum(sum(GG-diag(diag(GG)))) > 0 | sum(find(diag(GG)==0)) > 0
        iF  = inv(Z*PZ + GG);                                     % p. 67 D&K, see notes
    else
        D   = diag(1./diag(GG));                                  % okay; trick
        iF  = D - D*Z*inv(eye(m)+PZ*D*Z)*PZ*D;                    % okay; trick
    end
    if sum(sum(abs(iF*(Z*P*Z'+G*G') - eye(size(iF))))) > n^2*1e-6 % okay; trick
        error('Inversion of matrix F in did not work')            % okay; trick
        return
    end
    
    % Kalman gain K_t
    PZF = PZ*iF;                                                  % create first part of Kalman Gain
    K   = T*PZF;                                                  % create Kalman gain (see notes)
    
    % Updating equations
    P   = (S.T-K*Z)*P*S.T' + S.H*S.H';                    % update P
    P   =  0.5 * (P+P');                                  % okay; trick; check for symmetry (?)
    
    R.iFf(t,:,:)    = iF;
    R.Kf(t,:,:)     = K;
 end
 