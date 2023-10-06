function W = KFWeights(R,Model,Par,flag)
%_____________________________________________________________________________________
% Calculates the weights W assigned to observations y when estimating unobserved
% components a by Kalman filter and smoother 
%                            a_t|t=sum_j=1^t wF_j y_j
%                            a_t|T=sum_j=1^T wS_j y_j.
%
% INPUT  
%        R Estimates from Kalman filter FIS, see there for explanation                                                          
%        Model      Name of function to build SSF        (string)
%        Par        Parameter structure to build SSF
%        flag       'filter' or 'smooth' to obtain the respective weights
% OUTPUT 
%        W       : Weights from either filter or smoother (see flag)  
%        Filter  :   1 x nobs cell array of p x n x j     matrices, j = 1,...,nobs 
%        Smoother:   1 x nobs cell array of p x n x nobs  matrices
%_____________________________________________________________________________________

  eval(['S(1) = ' Model '(Par,[],1);']);
  [T,p] = size(R.Am);
  N     = size(S(1).Z,1);

% Extracting estimates
  Pm    = permute(R.Pm   ,[2 3 1]);
  Pstar = permute(R.Pstar,[2 3 1]);
  iF    = permute(R.iF   ,[2 3 1]);
  K     = permute(R.K    ,[2 3 1]);
  
  clear R
%_____________________________________________________________________________________
%                           Weights for filtering
%_____________________________________________________________________________________
% WF is a cell array of length T
% cell t contains t weights for a_t|t
% they are stored in 3-dimensional matrix where j runs along 3rd dimension 
% see Koopman, Harvey (2003) for the formulas

for t = 1:T

    wF = zeros(p,N,t);
    if t > 1
       eval(['S(t) = ' Model '(Par,S(t-1),' num2str(t) ');']);
    end

    Z = S(t).Z;
    wF(:,:,t) = Pm(:,:,t)*Z'*iF(:,:,t);
    B         = eye(p) - Pm(:,:,t)*Z'*iF(:,:,t)*Z;
    for j=t-1:-1:1
        wF(:,:,j)= B*K(:,:,j);
        B        = B*S(j).T-wF(:,:,j)*S(j).Z;
    end
    WF{t} = wF;
    
end


%_____________________________________________________________________________________
%                             Weights for smoothing
%_____________________________________________________________________________________
% To obtain the weights we use the fact that
%                    a_t|T=(I-P_tStar*T_t+1)a_t|t+P_tstar*a_t+1|T                 
% (Harvey 1989, pp.154)
% The weights are additive and consequently for the first term we can use the weights 
% for filtering and for the second term we can calculate them recursively
% WS is a cell array of length T
% Cell t contains T weights for a_t|T
% They are stored in 3-dimensional matrix where j runs along 3rd dimension 

  WS{T} = WF{T};
  
  for t=T-1:-1:1
     wS    = zeros(p,N,T);
     wSold = WS{t+1};
     for j = 1:t
         wF        = WF{t}(:,:,j);
         wS(:,:,j) = (eye(p)-Pstar(:,:,t) *S(t+1).T) * wF + Pstar(:,:,t)*wSold(:,:,j);
     end
     for j=t+1:T
         wS(:,:,j) = Pstar(:,:,t) * wSold(:,:,j);
     end
        
     WS{t} = wS;
  end

% Output the structure
  if     strcmp(flag,'filter')
         W = WF;
  elseif strcmp(flag,'smooth')
         W = WS;
  else   error('Flag for weights must be either filter or smooth')
  end
        

