function diffD = DateDiff(Date1,Date2);
%____________________________________________________
% diffD = DateDiff(Date1,Date2);
%
% Forms the difference between two monthly Dates
% The difference is an index (i.e. nr of data points)
%____________________________________________________

  diffD = Date1(1)*12+Date1(2)-Date2(1)*12-Date2(2);