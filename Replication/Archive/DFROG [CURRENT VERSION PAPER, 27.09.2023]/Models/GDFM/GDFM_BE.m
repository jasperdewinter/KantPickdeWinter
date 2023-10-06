function fcst = GDFM_BE(X,n_dyn,nn,n_stat,h)
%__________________________________________________________________________
%   fcst  = GDFM_BE(X,n_dyn,nn,n_stat,h) 
%   
%   Produces the h-step ahead forecast of the common components 
%   of the Generalized Dynamic Factor Model.
%   INPUTS
%   X              Data matrix having on the columns the series 
%   n_dyn          Nr of dynamic factors                           
%   nn             Bartlett lag-window size input in cestimate     
%   n_stat         Nr of static factors                             
%   h              Forecast horizon                                
%__________________________________________________________________________
 [X M W] = z01(X);
  W      = diag(W);
  T      = size(X,1);
  
  covc   = cestimate(X,n_dyn,nn);
  n      = size(covc,3);

  G      = squeeze(covc(:,:,(n + 1)/2));
  Gh     = squeeze(covc(:,:,(n + 1)/2+h));
  clear covc
  
  S      = cov(X) - G;
  [R D]  = eigs(G,diag(diag(S)),n_stat);
  
  H      = (R'*(G+S)*R)^(-1);
  M_x    = kron(ones(size(X(T,:),1),1),M);
  

  K      = Gh*R*H*R';
  temp   = X(T,:)*K';
  fcst   = temp*W + M_x;
