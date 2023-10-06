function [xc, dc] = Linkser(x1,d1,x2,d2,dLK,type)
%__________________________________________________________________________
% function [xc, dc] = Linkser(x1,d1,x2,d2,date)
%
% Links two series x1 and x2 that range over different dates into one series xc. 
% The corresponding date vectors d1 and d2 are combined into dc appropriately. 
% The series are linked at date dLK, which must be contained in both d1 and d2!
% 
% If type is 'A', series are linked additively 
%                       xc = x1 + C  if date <  dLK
%                       xc = x2      if date >= dLK              
%  where C = x2(dLK) - x1(dLK)
%
% If type is 'M', series are linked multiplicatively
%                       xc = x1 * C  if date <  dLK
%                       xc = x2      if date >= dLK              
%  where C = x2(dLK) / x1(dLK)
%_______________________________________________________________________________
% Indices of dLK
  ix1 = Datefind(d1,dLK);
  ix2 = Datefind(d2,dLK);
  
  if isnan(ix1)
     error(['Date ' date2str(dLK) ' not found in series 1'])
  end
  if isnan(ix2)
     error(['Date ' date2str(dLK) ' not found in series 2'])
  end
  
% Adjust x2
  if type == 'A';    
     C  = x2(ix2) - x1(ix1);
     x1 = x1 + C * ones(size(x1));
  end
  if type == 'M'
     C  = x2(ix2) / x1(ix1);
     x1 = C * x1;
  end    
  
% Link  
  xc  = [ x1(1:ix1-1,:) ; x2(ix2:end,:)];
  dc  = [ d1(1:ix1-1,:) ; d2(ix2:end,:)];
