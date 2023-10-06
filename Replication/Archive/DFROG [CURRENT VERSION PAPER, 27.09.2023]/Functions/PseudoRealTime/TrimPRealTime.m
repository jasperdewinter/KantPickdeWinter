function X = trimPRealTime(X,nPer)
%_____________________________________________________________________________________
% X = trimPRealTime(X,nPer)
%
% Shortens X by nPer while preserving the NaN pattern at the end (shifting the
% NaN pattern by nPer periods backwards. Used for creating quasi-realtime data.
%
% INPUT       X      Data matrix                                           (T x n)
%             nPer   Nr of periods by which X should be shortened
% OUTPUT      X      Shortened data                                   (T-nPer x n)
%_____________________________________________________________________________________
  [T,n]  = size(X);

% Spot the consecutive NaN's at the end of data
% cNaN(i,j) = 1 iff X(k,j) = NaN for all k >= i 
  X_NaN = isnan(X);
  
  cNaN  = (cumsum(flipud(X_NaN)) == repmat((1:T)',1,n));
  cNaN  = flipud(cNaN);
  
% Shorten X by nPer while preserving nan pattern at end
  cNaN    = cNaN(nPer+1:end,:);
  
  X       = X(1:end-nPer,:);
  X(cNaN) = nan;

