function Xr = GR_replace(X,Z,s);
%__________________________________________________________________________
% function Xr = GR_replace(X,Z,s);
%
% Replaces missing values of matrix X with values of Z
%__________________________________________________________________________

 Xr           = X; 
 Xr(isnan(X)) = Z(isnan(X));