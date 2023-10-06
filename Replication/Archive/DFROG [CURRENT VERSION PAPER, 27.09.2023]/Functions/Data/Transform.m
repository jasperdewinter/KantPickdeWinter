function Xc = Transform(X,Code,YearonYear,Quarterly)
%__________________________________________________________________________
% function Xc = Transform_ECB(X,Code)
% Transforms the euro area data according to input code
% (version for STFC project)
% INPUT
%       X        : Panel of raw data                      (nobs x N)
%       Code     : Vector containing transformation codes (N x 3)
%                  - col 1: take logs (Yes = 1)
%                  - col 2: Degree of differening (1 or 2)
%                  - col 3: degree of one-sided moving average filter
%                           x(t) + x(t-1) + ... + x(t-col3)
% OUTPUT
%       Xc       : Panel of transformed series            (nobs x N)
%__________________________________________________________________________
Xc  = nan*zeros(size(X));
n   = size(X,1);

for j = 1:size(X,2)
    k = 0;
    z = X(:,j);
    
    % log
    if Code(j,1) == 1
        z = log(z);
    end
    
    % Differencing for y-o-y and m-o-m mutations
    % for Monthly Data
    if Quarterly ==0;
        if YearonYear ==1;                                                  % for y-o-y growth rate calculation of quarterly series
            for i = 1:Code(j,2)
                ni = size(z,1);
                z  = (z(13:ni)-z(1:ni-12));
                k  = k + 1;
            end
        elseif YearonYear ==0;                                              % for q-o-q growth rate calculation of monthly series
            for i = 1:Code(j,2)
                ni = size(z,1);
%               z  = (z(5:ni)-z(1:ni-4)); % fout ontdekt d.d.25.01.2013
                z  = (z(4:ni)-z(1:ni-3));                                  
                k  = k + 1;
            end
        end
        
    % for Quarterly Data
    elseif Quarterly==1;
        if YearonYear ==1;                                                  % for y-o-y growth rate calculation of quarterly series
            for i = 1:Code(j,2)
                ni = size(z,1);
                z  = (z(5:ni)-z(1:ni-4));
                k  = k + 1;
            end
        elseif YearonYear ==0;                                              % for q-o-q growth rate calculation of monthly series
            for i = 1:Code(j,2)
                ni = size(z,1);
                z  = (z(2:ni)-z(1:ni-1));
                k  = k + 1;
            end
        end
    else   error('Set Quarterly (=1) or Monthly (=0) Frequencey in Transform function')
    end
    
    % Filter                                                                % Calculates the 3-month average of the monthly mutations (the
    % quarterly year-on-year growth rate)
    if Code(j,3) > 0
        m   = Code(j,3);
        z2  = filter(ones(1,m)/m,1,z);
        z   = z2(m:end);
        k   = k + m - 1;
    end
    
    % Add leading NaNs
    l1     = size(Xc,1) - size(z,1);
    z      = [nan*ones(l1,1);z];
    Xc(:,j) = z;
    
end
