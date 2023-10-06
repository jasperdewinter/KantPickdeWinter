function rmse = calc_rmse(E,Date,T1,T2,F)
%__________________________________________________________________________
% function rmse = calc_rmse(Error,Date,T1,T2,F)
%
% Calculates the root mean squared error from errors in errorQ over time
% periods T1 to T2.
%
% INPUT
% E            Errors                                        [T x h x k]
% Date(t,:)    Date at period t                              [T x 2]
% T1           Start date for calculation                    [1 x 2]
% T2           End date for calculation                      [1 x 2]
% F            Data frequency - 'Q' or 'M'
%
% OUTPUT
% rmse         Root mean squared errors                     [h x k]
%__________________________________________________________________________
  
% TrimData works only for 2-dim matrices
  if ndims(E) == 2
     Er = TrimData(E,Date,T1,T2,F);
  else
     Er = [];
     for i = 1:size(E,3)
         Er = cat(3,Er,TrimData(squeeze(E(:,:,i)),Date,T1,T2,F));
     end
  end   
    
  rmse = squeeze(sqrt(nanmean(Er.^2)));
  
% Adjust output  
  if ndims(E) == 2
     rmse = rmse';
  end  
  rmse  = flipud(rmse);
