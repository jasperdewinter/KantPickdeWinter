function Dates = GenDates(date1,date2,F)
%__________________________________________________________________________
% function Dates = GenDates(date1,date2,F)
% Generates a Dates vector from date1 to date2 with frequency F
% INPUT
%           trim0        Start date  (1    x 2)
%           trim1        End   date  (1    x 2)
%           F            frequency (either 'Q' or 'M')
% OUTPUT    Dates        Date vector (nobs x 2)
%__________________________________________________________________________
% Checks
  if ~((F == 'Q') | (F == 'M')) 
     error('Frequency must be either "Q" or "M"');
  end
 
 
 
% Years  
  Dates = (date1(1,1):date2(1,1))';

% Quarterly  
  if F == 'Q'
      Dates = [kron(Dates,ones(4,1)) , kron(ones(size(Dates,1),1),[3;6;9;12])];
      
      a     = date1(1,2) / 3;
      b     = date2(1,2) / 3;
      Dates = Dates(a:(end-(4-b)),:);
  end
  
% Monthly  
  if F == 'M'
      Dates = [kron(Dates,ones(12,1)) , kron(ones(size(Dates,1),1),(1:12)')];
      
      a = date1(1,2);
      b = date2(1,2);
      Dates = Dates(a:(end-(12-b)),:);
  end
