function date_ix = Datefind(Dates,date);
%__________________________________________________________________________
% function i = Datefind(Dates,date);
% Finds the indices of a vector date in Date vector Dates
% 
% If date(i,:) is not found in Dates then NaN is returned
%
% INPUT         Dates (nobs x 2)
%               date  (k    x 2)
% OUTPUT         i index
%__________________________________________________________________________
  k       = size(date,1);
  date_ix = nan * zeros(k,1);
  
  for i = 1:k
      j = find(Dates(:,1) == date(i,1) & Dates(:,2) == date(i,2));
     
     if ~isempty(j)
         date_ix(i) = j;
     end    
  end