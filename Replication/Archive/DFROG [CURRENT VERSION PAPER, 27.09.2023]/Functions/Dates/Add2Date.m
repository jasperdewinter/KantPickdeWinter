function Date = Add2Date(Date,n)
%_____________________________________________________________________________________
% function Date = Add2Date(Date,n)
% This function calculates the new date which is obtained from shifting Date
% forward by n months. n must be an integer number and migth be negative.
%_____________________________________________________________________________________

  years  = floor(n/12);
  mons  =    mod(n,12);
        
  if Date(2) + mons > 12
     years  = years  +  1;
     mons   = mons  - 12;
  end

  Date = Date + [years, mons];
      
