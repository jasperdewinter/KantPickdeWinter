function v = trimnanx(u)
%__________________________________________________________________________
% function v = trimnanx(u)
% cuts NaN's (with entire corresponding rows) out of a given raw matrix.
%__________________________________________________________________________

  v = u(all(~isnan(u),2),:);