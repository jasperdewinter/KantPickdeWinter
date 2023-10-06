 function [X_q,date_q] = m2qRealTime(X,date)
%__________________________________________________________________________
%  function [X_q, date_q] = m2qRealTime(X, date)
%  Converts the monthly dates and data into quarterly format
%  Averages only those over monthly data that are available at eos
%  Monthly data need not start in month 1
%
%  For each series the function determines the month within the quarter 
%  at which the last observation is available (month = 1/2/3). 
%  For the entire series the quarterly aggregate is then calulated only 
%  from the available months. 
%  Last observation at month 1  Form q agg as x(t-2)
%                            2                (x(t-1)+x(t-2))/2
%                            3                (x(t)+x(t-1)+x(t-2))/3
%
%  INPUT
%  X_q     Monthly data                                       [nobs x k]
%  date_q  Monthly dates                                      [nobs x 2]
%          (quarters 1, 2, 3, 4 are indicated by  3, 6, 9, 12) 
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

% Trim X to the expanded dates
  [X,date] = TrimData(X,date,d1,d2,'M');
  [T, k]   = size(X);

% Spot the nr of consecutive NaN's at eos 
% Vector month gives the month of the last obs of each series
  X_NaN    =  isnan(X);
  s_NaN    = (cumsum(flipud(X_NaN)) == repmat((1:T)',1,k));
  s_NaN    =  sum(s_NaN,1);
  month    =  monofQ(date(T-s_NaN,2));      

% Generate date vector & data
  date_q   = GenDates(mon2qrt(date(1,:)),mon2qrt(date(end,:)),'Q');
  X_q      = nan(T/3,k);
  
% Loop across series to do averaging over available month
  for i = 1:k
      movag    = ones(1,month(i))./month(i);
      x        = filter(movag,1,X(:,i));
      X_q(:,i) = x(month(i):3:end-3+month(i));
  end    
  