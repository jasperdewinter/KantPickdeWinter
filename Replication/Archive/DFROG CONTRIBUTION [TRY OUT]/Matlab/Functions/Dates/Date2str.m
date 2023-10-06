 function str = date2str(date)
%__________________________________________________________________________
%  function str = Date2str(date1)
%  Creates a string of a Date to print out
%__________________________________________________________________________

 str = [int2str(date(1,1)) '-' int2str(date(1,2))];