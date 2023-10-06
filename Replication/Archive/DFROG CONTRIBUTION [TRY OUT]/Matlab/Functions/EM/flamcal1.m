function beta = flamcal1(x,f)
%__________________________________________________________________________
% beta = flamcal1(x,f)
% OLS estimator beta = inv(f'*f)*f'*x   (see also function OLS)
%__________________________________________________________________________
 beta = inv(f'*f)*f'*x;