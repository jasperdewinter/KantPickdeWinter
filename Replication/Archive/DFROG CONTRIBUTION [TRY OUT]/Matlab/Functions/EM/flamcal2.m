function y = flamcal2(x,f,A)
%__________________________________________________________________________
%
%__________________________________________________________________________

  y = inv(f'*A'*inv(A*A')*A*f) * f'*A'*inv(A*A')*x;