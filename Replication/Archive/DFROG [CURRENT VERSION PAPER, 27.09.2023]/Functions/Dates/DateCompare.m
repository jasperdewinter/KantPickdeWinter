 function h = DateCompare(d1,d2)
%__________________________________________________________________________
%  function h = Datecompare(d1,d2)
%  Compares two dates
%  h =  1 if d1 is later than   d2
%  h = -1 if d1 is earlier than d2
%  h =  0 if d1 equals          d2
%__________________________________________________________________________
% Check dimensions
  if      sum(size(d1) ~= [1 2] | size(d2) ~= [1 2]) > 0
          h = NaN;
          return
  end

% Compare 
      if  d1(1) == d2(1) & d1(2) == d2(2)
          h =  0;
  elseif  d1(1) < d2(1) | (d1(1) == d2(1) & d1(2) < d2(2))
          h = -1;
  elseif  d1(1) > d2(1) | (d1(1) == d2(1) & d1(2) > d2(2))
          h = +1;
  end      
