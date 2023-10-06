 function datq = mon2qrt(datm)
%__________________________________________________________________________
%  function datq = mon2qrt(datm)
%  Creates the quarterly date in which month datm lies
%  This used when combining monthly and quarterly data in ReadPopulator
%__________________________________________________________________________
  if sum(size(datm) ~= [1 2]) > 0
      datq = NaN*zeros(1,2);
      return
  end 
  
  datq(1,1) = datm(1,1);
  datq(1,2) = ceil(datm(1,2)/3)*3;
