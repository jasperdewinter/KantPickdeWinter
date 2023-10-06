function [fcst Yref fDate P] = PseudoRealTime(Model,P,X,Y,DateX,T,h,Q_a)
%______________________________________________________________________
% function [fcst Yref fDate P] = PseudoRealTime(Model,P,X,Y,DateX,T,c,Q_a)
%
% Performs pseudo-real time forecasts. The model is passed via string
% INPUTS
%  Model    (string) is the name of a function that runs a the model 
%           & delivers a forecast
% P         Modelspecific structure that may contain parameters.
%           While P might be modified in function 'Model', the latter 
%           should not change the original input as P is used recursively
% X
% Y
% DateX
% T
% c
% h
% Q_a
%
% OUTPUTS
%
%______________________________________________________________________

% Get Y in quarterly format 
  [YQ, DateQ] = m2q(Y,DateX,'3');

% Trim data to startE : Now
  [Xc, Date] = TrimData(X, DateX, T.startE,T.Now,'M');
   Yc        = TrimData(Y, DateX, T.startE,T.Now,'M');
   
% Extend timing to fill forecasts over the entire sample
  startF = Add2Date(T.startF,-3*h);
  endF   = Add2Date(T.endF,3);

% Nr of recursions  
  nR     = DateDiff(endF,startF)+1;
  fcst   = nan(nR,h+2);
  Yref   = nan(nR,h+2);
  fDate  = nan(nR,2);

% Trim data to endF while keeping data release pattern against now
  diffFN = DateDiff(T.Now,endF); 
  Xc     = TrimPRealTime(Xc,diffFN);
  Yc     = TrimQRealTime(Yc,diffFN,Date,Q_a);
  Date   = Date(1:end-diffFN,:);
    
%_________________________________________________
% LOOP 
  for i = 1:nR
    if ~(strcmp(Model,'RUN_QVAR') | strcmp(Model,'RUN_BEQ'))
        disp(['Period ' Date2str(Date(end,:))])
    end
    
  % Run estimation & forecast
    eval(['[f Date_f P] = ' Model '(Xc,Yc,Date,h,P);']);
   
  % Store fcst & current date 
    if i == 1 & size(f,2) > 1 
       fcst = nan(nR,h+2,size(f,2));
    end   
    fcst(i,:,:) = f;
  
    fDate(i,:)  = Date(end,:);                    
    idxF        = DateFind(DateQ,mon2qrt(Date(end,:)));
    Yref(i,:)   = YQ(idxF-1 : idxF+h);
    
  % Trim by one while keeping quasi-RT pattern
    Xc          = TrimPRealTime(Xc,1);
    Yc          = TrimQRealTime(Yc,1,Date,Q_a);
    Date        = Date(1:end-1,:);
  end

% Flip timing of the forecast
  fcst   = flipdim(fcst,1);
  Yref   = flipdim(Yref,1);
  fDate  = flipdim(fDate,1);
  