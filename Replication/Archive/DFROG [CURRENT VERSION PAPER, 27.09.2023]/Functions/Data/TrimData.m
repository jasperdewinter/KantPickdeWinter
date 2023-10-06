function [X_trim, D_trim] = TrimData(X,Dates,trim0,trim1,F)
%__________________________________________________________________________
% function [X_trim, Dates_trim] = TrimData(X,Dates,trim0,trim1,F)
%
% Trims a data matrix X and the corresponding date vector Dates to run
% from date0 to date1. If date0 or date1 are beyond the dates covered by 
% the series then X and Dates are expanded with NaN
%
% INPUT
%           X            Data matrix                           (nobs x k)       
%           Dates        Dates                                 (nobs x 2)
%           trim0        Start date                            (1    x 2)
%           trim1        End   date                            (1    x 2)
%           F            Frequency                           ('Q' or 'M')
%________________________________________________________________________________
  [nobs,k] = size(X);
  
% Convert to monthly if desired frequency is quarterly  
  if F == 'Q';
    trim0 = mon2qrt(trim0);
    trim1 = mon2qrt(trim1);
  end  
  
% Find indices of start/end
  i0 = DateFind(Dates,trim0);
  i1 = DateFind(Dates,trim1);

% If Dates not found: set to start / end & expand later on 
  if isnan(i0) ; i0 = 1        ; end
  if isnan(i1) ; i1 = size(X,1); end

% Check whether start later than end  
  if i0 > i1
     X_trim = [];
     D_trim = [];
     error('Start date is later than end date')
  end

% Trim data & dates  
  X_trim  =      X(i0:i1,:); 
  D_trim  =  Dates(i0:i1,:);   
  
% Now possibly expand at the beginning
  i0 = DateFind(Dates,trim0);
  if isnan(i0)
     D_app = GenDates(trim0,Dates(1,:),F);
     
     X_trim = [nan*ones(size(D_app,1)-1,k) ; X_trim];
     D_trim = [           D_app(1:end-1,:) ; D_trim];
  end    
      
% Expand at the end  
  i1 = DateFind(Dates,trim1);
  if isnan(i1)
     D_app = GenDates(Dates(end,:),trim1,F);
     
     X_trim = [X_trim; nan*ones(size(D_app,1)-1,k)];
     D_trim = [D_trim;            D_app(2:end,:)];
  end 