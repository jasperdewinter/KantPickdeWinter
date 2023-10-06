function XC = center(X)
%__________________________________________________________________________
%   XC = center(X)
%	Centers each column of X.
%__________________________________________________________________________

  [T n] = size(X);
  XC    = X - ones(T,1)*(sum(X)/T);  % Much faster than MEAN with FOR loop
