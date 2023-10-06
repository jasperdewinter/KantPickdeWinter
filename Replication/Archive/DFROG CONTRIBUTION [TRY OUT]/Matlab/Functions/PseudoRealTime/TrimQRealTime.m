function y = trimQRealTime(y,nPer,DateC,Q_a)
%_____________________________________________________________________________________
% function y = trimQRealTime(y,DateC,nPer,Q_a)
%
% For use in recursive forecasting: 
% Shortens the matrix of quarterly series y by nPer periods, deletes data
% for  the current quarter and applies pattern of data availability to previous
%
% INPUT
% y      Quarterly series in monthly format                              (T x n)
% DateC  Corresponding date vector                                       (T x 2)
% nper   Length of cut                                                   (integer)
% Q_a    Pettern of prev quarter data availability                       (3 x n)
%_____________________________________________________________________________________

% Cut data
  y     = y(1:end-nPer,:);
  
% Obtain current month  
  month = monOfQ(DateC(end-nPer,2));
  
% Current quarter  
  y(end-month+1:end,:) = nan;
  
% Apply pattern
  if     month == 3 
             y(end-5:end-3,~Q_a(3,:)) = nan;
  elseif month == 2 
             y(end-4:end-2,~Q_a(2,:)) = nan;
  elseif month == 1 
             y(end-3:end-1,~Q_a(1,:)) = nan;
  end
