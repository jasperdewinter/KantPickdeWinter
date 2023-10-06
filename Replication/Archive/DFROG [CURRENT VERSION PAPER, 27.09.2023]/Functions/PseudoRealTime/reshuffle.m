function [fcstQ,YrefQ,fDateQ,errorQ] = reshuffle(fcstR,Y_ref,fDate)
%__________________________________________________________________________
% function [fcstQ,YrefQ,fDateQ,errorQ] = reshuffle(fcstR,Y_ref,fDate)
%
% Reorders the forecasts produced in the main programs Recfcst to obtain 
% the forecasts done in the individual months of the quarters. 
% Input fcstR just obtains the sequence of forecasts that is obtained when 
% moving through the months. f(123,1) would contain the backcast done in 
% 132 of the sample for the previous quarter.
%
% Output fcstQ puts into a single column the forecasts, which have been 
% done in a certain month of a quarter for a certain quarterly horizon.
%    fcstQ(S,i) i = 1:3 contains the fcst for quarter S   done in month i 
%                       of quarter S+1 (backcast). 
%    fcstQ(S,i) i = 4:6 contains the fcst for quarter S   done in month i 
%                       of quarter S (nowcast)
%    fcstQ(S,i) i = 7:9 contains the fcst for quarter S+1 done in month i 
%                       of quarter S 
%
% INPUT
% fcstR(t,j,k) Forecast  Y(t+(j-2)|t)                        [T,fcstH+2,K]
%              of Y for period t+j-2 done at time t
%              The +2 occurs because the setup includes now- & backcasts
%              t = 1:T (months) is the period at which the fcst is done 
%              j = 1:fcsth+2    is the fcst horizon 
%              k = 1:K          is the specification (K=1 is feasible)
%
% Y_ref(t,j)   Original data: error = fsctR - Y_ref          [T,fcstH+2,1]
% fDate(t,:)   Date at period t                              [T,2]
%
% OUTPUT
% fcstQ(s,j,k) Forecast Y(s|s-(j-2)) for a certain quarter  
%              (see above)                             [T/3,3*(fcstH+2),K]
% 
% YrefQ        Original data
% fDateQ       Date S
% rmse         Root mean squqared errors [3*(fcstH+2),K]
%__________________________________________________________________________
  
  nR      = size(fcstR,1);
  fcstQ   = nan(size(fcstR,1)/3,size(fcstR,2)*3,size(fcstR,3));
  mnthQ   = nan(size(fcstQ));
  YrefQ   = nan(size(fcstQ));
    
  fDateQ  = fDate(3:3:nR,:);
  mnth    = monOfQ(fDate(:,2));
  if mnth(1) ~= 1
     error('Data must start in 1st month of quarter')
  end  
  
  for i = 1:size(fcstR,1)
  for j = 1:size(fcstR,2)
  if  ceil(i/3)+(j-2) > 0 & ceil(i/3)+(j-2) <= size(fcstQ,1)
        fcstQ(ceil(i/3)+(j-2),3*(j-1)+4-mnth(i),:,:) = fcstR(i,j,:);
        YrefQ(ceil(i/3)+(j-2),3*(j-1)+4-mnth(i),:,:) = Y_ref(i,j,:);
        mnthQ(ceil(i/3)+(j-2),3*(j-1)+4-mnth(i),:,:) = mnth(i);
  end
  end
  end
      
% Adjust for precise timing (startF:endF)
  fcstH   =  size(fcstR,2)-2;
  fcstQ   =   fcstQ(fcstH+1:end-1,:,:);
  YrefQ   =   YrefQ(fcstH+1:end-1,:,:);
  mnthQ   =   mnthQ(fcstH+1:end-1,:,:);
  fDateQ  =  fDateQ(fcstH+1:end-1,:,:);
  
% Errors  
  errorQ  = (fcstQ - YrefQ);
  YrefQ   = squeeze(YrefQ(:,1,1));
