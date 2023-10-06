 function [X, Date_m] = q2m(X_q, Date_q, mon1,monL,type)
%__________________________________________________________________________
%  function [X, Date_m] = q2m(X_q, Date_q, mon1,monL,type)
%  Converts the quarterly dates and data into monthly format.   
%  INPUT
%  X_q     Quarterly data                                       [nobs x k]
%  Date_q  Quarterly dates                                      [nobs x 2]
%          (quarters 1, 2, 3, 4 are indicated by  3, 6, 9, 12) 
%  mon1    1st month of the monthly data                    (1 < mon1 < 12)
%  monL    Last month of the monthly data                 (1 < monEnd < 12)
%  type    How to fill the data 
%                          3 ... fill only 3rd month
%                          0 ... fill all      months 
%
%  OUTPUT
%  Date_m  Monthly dates                                      [3*nobs x 2]
%  X       Monthly data                                       [3*nobs x k]
%__________________________________________________________________________
% Checks
  if size(Date_q,1) ~= size(X_q,1)
     error('Length of Data & Dates are not consistent');
  end   
  
  if  (mon1 < 1) | (mon1 > 12)
     error('Month 1   must be in [1,12]'); 
  end
  if  (monL < 1) | (monL > 12)
     error('Month End must be in [1,12]'); 
  end

  if nargin < 5
      type = '3';
  end
  
% Expand from M to Q
  switch type
     case '3'    
         X = kron(X_q,[NaN NaN 1]');
     case '0'
         X = kron(X_q,[1   1   1]'); 
  end
  Date = GenDates(Date_q(1,:)-[0 2],Date_q(end,:),'M');
     
% Trim to desired date 
  [X, Date] = TrimData(X,Date,[Date_q(1,1),mon1],[Date_q(end,1),monL],'M');
  
  