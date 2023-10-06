function y = normx(X)
%__________________________________________________________________________
% function y = normx(X)
% Normalizes matrix X to mean 0 and standard deviation 1.
% We assume that the variables are organized into cols.
% Does the same as z01(X), but the latter also takes acount of missing
% values
%__________________________________________________________________________
  if nargin ~=1
      error('Normalizing X: one input variable is required.')
      return
  end    
  if (size(X,1)==1)&&(size(X,2)==1)
      error('Normalizing X: input variable has to be either a matrix or a scalar.')
      return
  end    
        
  m  = size(X,1); 
  me = mean(X); 
  s  = std(X); 
        
  mea = []; 
  st  = [];
        
  for i = 1:m
      mea = [mea; me];
      st  = [st; s];
  end
  y = (X-mea) ./ st;
   
    