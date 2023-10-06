 function [X_q,date_q] = m2q(X,date,aggtype)
%__________________________________________________________________________
%  function [X_q, date_q] = m2q(X, date,aggtype)
%  Converts the monthly dates and data into quarterly format 
%  Monthly data need not start in month 1
%
%  INPUT
%  X_q     Monthly data                                       [nobs x k]
%  date_q  Monthly dates                                      [nobs x 2]
%          (quarters 1, 2, 3, 4 are indicated by  3, 6, 9, 12) 
%  aggtypes = 'S'  Sum       of monthly data
%           = 'A'  Average   of monthly data
%           = 'M'  Aggregation of month-on-month rates
%           = '3'  3rd month of each quarter
%
%  OUTPUT
%  date  Quarterly dates                                      [nobs/3 x 2]
%  X     Quarterly data                                       [nobs/3 x k]
%__________________________________________________________________________

  if size(X,1) ~= size(date,1)
     error('Data and date vectors must be of equal length')
  end   

% Expand date to start in 1st and end in 3rd mon of the quarter 
  d1 = Add2Date(date(1,:)  ,1-monOfQ(date(1,2)));  
  d2 = Add2Date(date(end,:),3-monOfQ(date(end,2))); 

% Trim to expanded dates 
  [X,date] = TrimData(X,date,d1,d2,'M');
  
% Generate date vector %& data
  [nobs, k] = size(X);
  date_q    = GenDates(mon2qrt(date(1,:)),mon2qrt(date(end,:)),'Q');
 
  if       aggtype == 'S' ;  X = filter([1 1 1]       ,1,X);
  elseif   aggtype == 'A' ;  X = filter([1 1 1]    ./3,1,X);
  elseif   aggtype == 'M' ;  X = filter([1 2 3 2 1]./3,1,X);   
  end
  X_q = X(3:3:end,:);
  
  if aggtype == 'M'
     X_q(1,:) = nan;
  end   
 
