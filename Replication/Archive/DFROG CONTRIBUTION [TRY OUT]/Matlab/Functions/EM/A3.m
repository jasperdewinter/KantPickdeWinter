function A = A3(k)
%__________________________________________________________________________
% function A = A3(k)
% This auxilary proc is intended to compute C matrix that 
% converts 3n monthly observations into n quarterly observations.
% We assume that the underlying variable is either an I(1) stock or a I(0) flow.
% For the reference see Chow & An-loh Lin, "Best linear unbiased 
% interpolation, distribution and extrapolation...". 
%__________________________________________________________________________
 A = zeros(k,3*k);
 
 for i = 1:k
   A(i,3*i-2:3*i) = ones(1,3); 
 end  
