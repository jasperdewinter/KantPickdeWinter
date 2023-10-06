function y = rearrange(x)
%__________________________________________________________________________
% function y = rearrange(x)
% Sorts (in ascending order) the values of vector x and then 
% cuts out the values that appear more than once
%__________________________________________________________________________

  y = unique(sort(x));
  
%  x = sort(x); 
%  flag = 0;
%  if size(x,2)>1
%     x = x';
%     flag = 1;
%  end

%  rowsx = size(x,1);
%  if rowsx > 1
%    s = x(1,1);
%    y = s;
%    for i = 2:rowsx
%        t = x(i,1);
%        if t > s
%            s = t;
%            y = [y;s];
%        end
%    end
%  end

%  if flag == 1
%     y = y';
%  end
    
