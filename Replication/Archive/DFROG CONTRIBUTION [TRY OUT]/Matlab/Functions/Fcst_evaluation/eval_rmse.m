function [Rs,Ps,Rm,rk,sx] = eval_rmse(R,P,h)
%__________________________________________________________________________
%  function [Rs,Ps,Rm,rk,sx] = eval_rmse(R,P,h)
% 
% Takes a matrix of root mean squared errors over a horizon of 1:fcstH 
% together with K model specifications and sorts both according to the 
% average RMSE over a certain range of horizons h
%
% INPUT
%   R       Root mean squared errors                          [fcstH x K]
%   P       Parameter specifications                              [M x K]
%   h       Index row vector: horizons for calculating mean RMSE
%           ([] = 1:fcstH)
%
% OUTPUT
%   Rs      Sorted RMSE
%   Ps      Sorted specifications
%   Rm      Sorted average RMSE (over horizons h)
%   rk      Rank of models      i.e. r_s(:,rk) = r
%   sx      Index of sorting    i.e.   r(:,sx) = r_s
%__________________________________________________________________________
% Check whether h is feasible
  if isempty(h)
     h = 1:size(R,1);
  end   
  if size(h,2) == 1
     h = h'; 
  end
  if max(h) >  size(R,1)
     error('Index is too large')
  end  
  
% Calculate mean RMSE for desired horizon
  if size(h,2) > 1
     Rm = mean(R(h,:)); 
  else
     Rm = R(h,:);
  end   
  
% Sort
 [Rm sx] = sort(Rm',1,'ascend');
  Rm     = Rm';
  Rs     = R(:,sx);
  Ps     = P(:,sx);
  
% Rank = position of i in ix
  for i = 1:size(sx)
      rk(sx(i)) = i; 
  end
  sx = sx';