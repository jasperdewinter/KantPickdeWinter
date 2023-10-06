%______________________________________________________________________
function D  = CalcGinv(y,G,k);
%______________________________________________________________________
% PROC CalcGinv                                                      
% PURPOSE: Calculates the inverse of S.G*S.G' under the assumption that 
%          G is block-diagonal, with the 1st block being diagonal and 
%          the 2nd block being general
%          Takes account of missing data in doing so
%
% INPUT    y             vector of observations at time t      (n x 1 )    
%          S             KF system matrices                 (structure)
%          k             length of the first (diagonal) block
% OUTPUT   
%          D             inv(G*G')          (reduced for avaiable data)       
%______________________________________________________________________
  List = 1:size(G,1);

% Get rid of rows with NaN data   
  ix   = ~isnan(y);
  G    =  G(ix,:);
  List =  List(ix);
 
% Find last row of 1st block 
  L1 = find(List <= k);
  L2 = find(List >  k);
  
% Make D from the two blocks  
  n = size(G,1);
  D = zeros(n,n);
  
  D(L1,L1) = diag(1./diag(G(L1,:)*G(L1,:)'));
  D(L2,L2) =         pinv(G(L2,:)*G(L2,:)');

%  'Check'
%  D*(G*G')