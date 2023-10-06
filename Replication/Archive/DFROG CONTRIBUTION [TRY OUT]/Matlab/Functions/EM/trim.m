function v = trim(u)
%__________________________________________________________________________
% function v = trim(u)
% Cuts zeros out of a given vector u
%__________________________________________________________________________

  v = u(u~=0);
  
% Possibly transpose
%  flag = 0; 
%  r = size(u,1);
%  if r == 1
%    u = u';
%    flag = 1;
%    r = size(u,1);
%  end
  
% Find Nr of zeros in u
%  ind = size(find(u ~= 0),1);
%   ind = 0;
%   for i = 1:r
%       if u(i,1) ~= 0
%          ind = ind+1;
%       end
%   end

%  v = zeros(ind,1); 
%  ind = 1;
%  for i = 1:r
%    if u(i,1) ~= 0
%        v(ind,1) = u(i,1);
%        ind = ind+1;
%    end
%  end

%  if flag==1
%     v = v';
%  end

