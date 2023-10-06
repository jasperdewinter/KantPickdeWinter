% Stop gezet in SKF in line 80 (dus precies op de loop)
% nu regel voor regel kijken wat er gebeurt.


for t = 1:nobs                                                  % Starting the loop
    % Important to understand what is exactly in the matrices R.Am en R.Pm
    R.Am(t,:)   = A;                                            % A_t|t-1 = A
    R.Pm(t,:,:) = P;                                            % P_t|t-1 = P
    
    %_________________________________________________________________________________________________
    % Obtain SSF and D= inv(GG')
    % Rebuild DFM using S and time t. Diff with initial model call is you now
    % put in values for S and a value for the cumulator
    %_________________________________________________________________________________________________
    %
    eval(['S = ' Model '(Par,S,' num2str(t) ');']);
%     [y,Z,G,c1,L] = MissData(Ym(t,:)',S);                       % Missdata is a special command
%     
%     % Prediction equations
%     %_____________________
%     if isempty(y)
%         Au = A;                                                % Au = At-1   
%         Pu = P;                                                % Pu = Pt-1
%         A  = S.T*A      + S.c2;                                % A  = A_t|t-1 
%         P  = S.T*P*S.T' + S.H*S.H';                            % P  = P_t|t-1
%         iF = zeros(n,n);                                       % Set iF-Matrix to zero when no new information in y(i).
%         K  = zeros(m,n);                                       % Set K-Matrix to zero when no new information in y(i).
%     else
        
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
        Au  = A  + (PZF * V);                                 % Au = A_t|t
        Pu  = P  - (PZF * PZ');                               % Pu = P_t|t  
        
        % Updating equations
        % A  = A_t+1|t & P  = P_t+1|t
        A   =  S.T*A + K*V + S.c2;                            % update A
        P   = (S.T-K*Z)*P*S.T' + S.H*S.H';                    % update P
        P   =  0.5 * (P+P');                                  % okay; trick; check for symmetry (?)
        
        % Likelihood
        R.Q  = R.Q  + V'*iF*V;                                 
        R.sF = R.sF - log(det(iF));                           % R.sf was zero in first call
        
        % Restore structure of iF and K
        iF   = L*iF*L';                                       % uses the L-matrix from  MissData                      
        K    = K*L';                                          % uses the L-matrix from  MissData
%     end
    
    R.AmU(t,:)     = Au;                                                               
    R.PmU(t,:,:)   = Pu;
    R.Ptest(t,:,:) = P;   
    R.Atest(t,:,:) = A;   
    R.iF(t,:,:)    = iF;
    R.K(t,:,:)     = K;
     
end 