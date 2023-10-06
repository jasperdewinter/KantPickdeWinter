function yp = QLinPol(y,Date)
%__________________________________________________________________________
% yp = QLinPol(y,Date)
% Produces monthly interpolates of quarterly data y
% INPUT
%  y    Quarterly data in monthly format with nan in months 1 & 2 (n x 1)
%  Date corresp date
% OUTPUT
%  yp Interpolated data
%
% The formula is as follows: consider to data points (0,y0) & (3,y3)
% One finds the equation      y = y0 + (y3-y0)/3 * x
%__________________________________________________________________________
  yp = nan(size(y));
  
% Below: stop at last available data point
  k  = mod(monOfQ(Date(end,2)),3);
  
  for i = 4:(size(y,1) - k);
      if     monOfQ(Date(i,2)) == 1
             y0    = y(i-1);
             y3    = y(i+2);
             yp(i) = (y3+2*y0)/3;
             
      elseif monOfQ(Date(i,2)) == 2     
             y0    = y(i-2);
             y3    = y(i+1);
             yp(i) = (2*y3+y0)/3;
      else
             yp(i) = y(i);
      end
  end    
