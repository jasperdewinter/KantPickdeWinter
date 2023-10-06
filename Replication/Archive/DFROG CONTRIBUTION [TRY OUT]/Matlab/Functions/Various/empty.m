function s = empty(n)
% Produces empty horizontal space for display
  s = kron(char(0),ones(1,n));
