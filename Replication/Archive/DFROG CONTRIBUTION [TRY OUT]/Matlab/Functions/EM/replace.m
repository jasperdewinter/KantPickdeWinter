function w = replace(u,v,flag)

% This proc replaces missing/non-missing values in u vector with 
% the corresponding values taken from v vector. If 'flag' equals to 1,
% NaN's are being replaced, if 'flag' amounts to 0 NaN's are preserved 
% and all the other values are being replaced.

  if nargin ~= 3
     error('Replacing values: 2 input vectors plus 1 scalar are required.')
     return
  end    
  if size(u,2)~=1||size(v,2)~=1
     error('Replacing values: procedure accepts only row vectors.')
     return
  end
  if size(u,1)~=size(v,1)
     error('Replacing values: the length of v has to match the length of u.')
     return
  end
  if (size(flag,1)~=1)||(size(flag,2)~=1)
     error('Replacing values: flag has to be a scalar.')
     return
  end
      
  for i=1:size(u,1)
      if flag == 1
         if isnan(u(i,1))
            u(i,1) = v(i,1);
         end
      else
         if flag = =0
            if ~isnan(u(i,1))
                u(i,1) = v(i,1);
            end
         end
      end
   end    
   w = u;

