function l = lag(x,n)
%__________________________________________________________________________
% function l = lag(x,n)
% Lags vector x by n periods
% The n leading elemnts of x are cut
%__________________________________________________________________________

  if nargin == 1
     n = 1;
  end
  if max(size(n)) > 1
     error('Second input variable has to be a scalar' )
     return
  end

  if     size(x,1) == 1
         l = x(1:size(x,2)-n);
  
  elseif size(x,2) == 1
         l = x(1:size(x,1)-n);

  else
         error('First input variable has to be a vector')
         return
  end