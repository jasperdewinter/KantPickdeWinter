function A=A1(k)
%__________________________________________________________________________
% function A=A1(k)
% This auxilary proc is intended to compute C matrix that 
% converts 3n monthly observations into n quarterly observations.
% We assume that the underlying variable is an I(0) stock.
% For the reference see Chow & An-loh Lin, "Best linear unbiased 
% interpolation, distribution and extrapolation...".  
%__________________________________________________________________________
 A = zeros(k,3*k);
 for i = 1:k
     A(i,3*i) = 1; 
 end    