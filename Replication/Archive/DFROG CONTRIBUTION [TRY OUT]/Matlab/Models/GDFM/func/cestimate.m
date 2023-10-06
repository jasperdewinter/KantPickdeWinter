function S = cestimate(x,nfactors,w)
%__________________________________________________________________________
% S = cestimate(x,nfactors,w)
% Estimates the common covariance structure
%__________________________________________________________________________
  opts.disp = 0;

% Define some useful quantities
  [T,N] = size(x);
  W     = 2*w+1;
  B     = triang(W);

% Compute covariances
  S = zeros(N,N,W);
  for k = 1:w+1,
      S(:,:,w+k)   = B(w+k)*center(x(k:T,:))'*center(x(1:T+1-k,:))/(T-k);
      S(:,:,w-k+2) = S(:,:,w+k)';
  end

% Compute the spectral matrix in W points (S)
  Factor = exp(-sqrt(-1)*(-w:w)'*(0:2*pi/W:4*pi*w/W));
  for j  = 1:N
      S(j,:,:) = squeeze(S(j,:,:))*Factor;
  end
 
% Compute the eigenvectors  for all points (E)
  [A,D]    = eigs(S(:,:,1),nfactors,'LM',opts);
  S(:,:,1) = A*D*A'; 

  for j = 2:w+1,
      [A,D]        = eigs(S(:,:,j),nfactors,'LM',opts);
      S(:,:,j)     = A*D*A'; 
      S(:,:,W+2-j) = conj(S(:,:,j));
  end
  for j = 1:N
      S(:,j,:)     = real(squeeze(S(:,j,:))*conj(Factor).'/W);
  end
