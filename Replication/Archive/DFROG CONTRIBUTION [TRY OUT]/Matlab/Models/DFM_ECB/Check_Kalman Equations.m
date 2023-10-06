%-------------------------------------------------------------------
% Run Setup test_DFM and then check how Kalman equations are defined
%-------------------------------------------------------------------

%-------------
% Preliminary
%-------------
endF         = mon2qrt(Add2Date(Date(end,:),3*fcstH)); 		% Add2Date simple function that adds date at the end of the sample
Xf           = TrimData(X, Date,Date(1,:),endF,'M');   		% Trim X-Matrix from Date(1,:), endF, using monthly frequency
[yf Date_f]  = TrimData(Y_M, Date,Date(1,:),endF,'M');   	% Trim y as well
Xf           = Outliers(z01(Xf),0);                    		% Remove outliers after Xf is standardized
Q.Date       = Date_f;                                 		% Quarterly Da

Ym   = [Xf yf]

[n m] = size(S.Z);                                          % size of S.Z (first mat in eq (6)
nobs  = size(Ym,1);                                         % # of rows in Ym

%-------
% Set t
%-------
t     = 572
Model = Q.Model
Par   = Q

eval(['S = ' Model '(Par,S,' num2str(t) ');']);

%--------------------
% Reproduce Missdata
%--------------------
y  =Ym(t,:)'
ix = ~isnan(y)
e  = eye(size(y,1))
L  = e(:,ix)
y  =    y(ix)
Z  =  S.Z(ix,:) 
G  =  S.G(ix,:)
c1 = S.c1(ix,:)

%--------------------------------------------------------------------------
% If serlist = [8,34], dan voor observation 3 and smaller line 48-53. If
% obsveration >3 then line 55-99. You can just put everything to matlab
% (doesn't matter, it picks the best way). From 573 onwards use line 48-53
%--------------------------------------------------------------------------

Pt   = permute(R.Pm,[3 2 1]);                               % extra line to create P-matrix at different points in time
% Ktest   = permute(R.K,[2 3 1]);                           % extra line to create P-matrix at different points in time
% Ptest   = permute(R.Pm,[3 2 1]);                          % extra line to create P-matrix at different points in time
% Ktest(:,:,4)
% Ptest(:,:,4)

% Kalman equations
%-------------------
if isempty(y)
    A  = R.Am(t,:)'                                         % WATCH OUT! Extra line to create A-matrix"
    P  = Pt(:,:,t)                                          % WATCH OUT! Extra line to create P-matrix     
    A  = S.T*A      + S.c2;                                 % A  = A_t|t-1
    P  = S.T*P*S.T' + S.H*S.H';                             % P  = P_t|t-1
    iF = zeros(n,n);                                        % Preallocate for inverse of F-matrix.
    K  = zeros(m,n);                                        % Preallocate for Kalman Gain; K-matrix.

else
    A  = R.Am(t,:)'                                         % WATCH OUT! Extra line to create A-matrix"
    P  = Pt(:,:,t)                                          % WATCH OUT! Extra line to create P-matrix     
    
    % Compute inv(F_t)
    % Necessary for caluclation of Kalman Gain: K_t=T_t*P_t*Z_t*F_t(-1)
    PZ = P*Z';                                             % create first part of F
    GG = G*G';                                             % create second part of F
    
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
    PZF = PZ*iF;                                          % create first part of Kalman Gain
    K   = S.T*PZF;                                        % create Kalman gain (see notes)
    
    % Au = A_t|t   & Pu = P_t|t
    V   = y - Z*A  -  c1;                                 % y arrives, calculate forecast error
    
    % Contemporaneous filtering equations
    Au  = A  + (PZF * V);                                 % Au = A_t|t (gecheck  met R.AmU(t,:,:))
    Pu  = P  - (PZF * PZ');                               % Pu = P_t|t (gecheck  met R.PmU(t,:,:))
    
    % Updating equations
    % A  = A_t+1|t & P  = P_t+1|t
    A   =  S.T*A + K*V + S.c2;                            % update A  (gechekt met R.Atest(t,:,:)
    P   = (S.T-K*Z)*P*S.T' + S.H*S.H';                    % update P  (gechekt met R.Ptest(t,:,:)
    P   =  0.5 * (P+P');                                  % okay; trick; check for symmetry (?)
    
    % Likelihood
    R.Q  = R.Q  + V'*iF*V;                                % ...
    R.sF = R.sF - log(det(iF));                           % R.sf was zero in first call
    
    % Restore structure of iF and K
    iF   = L*iF*L';                                       % Re-create matrix structure of iF
    K    = K*L';                                          % Re-create matrix structure of K
end