function [fcst Yref fDate P W S C CGr WGr] = PseudoRealTime_ps(Model,P,X,Y,DateX,T,h,Q_a,Gr)
%______________________________________________________________________
% function [fcst Yref fDate P W S C] = PseudoRealTime(Model,P,X,Y,DateX,T,c,Q_a)
%
% Performs pseudo-real time forecasts. The model is passed via string
% INPUTS
%  Model    (string) is the name of a function that runs a the model 
%           & delivers a forecast
% P         Modelspecific structure that may contain parameters.
%           While P might be modified in function 'Model', the latter 
%           should not change the original input as P is used recursively
% X         Contains quarterly growth rate/level in every month of all the
%           X's
% Y         Contains quarterly growht rate of GDP on the third month of
%           each quarter
% DateX     Contains <# x 2> Date vector
% T         Struct with startE [1991 1], startF [1998 1], startV [2000 3],
%           endF [2010 12], Now [2012 9]
% c         Forecast horizon
% Q_a       Quarterly pattern of the data [0 0 1]
%
%
% OUTPUTS
% fcst      Forecasts
% Yref      
% fDate     Forecast Dates
% P          
% W         Struct of Weight matrices for every time point, i=1 is last
%           database
% S         Struct of Signal matrices for evey time point, i=1 is last
%           database
%______________________________________________________________________

% Get Y in quarterly format (delete rows of NaNs) 
  [YQ, DateQ] = m2q(Y,DateX,'3');

% Trim data to startE : Now
% Delete all observations before T.startE and after T.now
% Replace varibales and Date with NaN if variable is non-existent
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
    % eval(['[f Date_f P] = ' Model '(Xc,Yc,Date,h,P);']);
    eval(['[f Date_f P R_KF Q W S C CGr WGr] = ' Model '(Xc,Yc,Date,h,P,Gr);']); 
    
    % Create seperate weights and signal matrices for every database
    eval(['Weight.W' num2str(i) ' = W ;'])
    eval(['WeightGr.WGr' num2str(i) ' = WGr ;'])
    eval(['Signal.S' num2str(i) ' = S ;'])
    eval(['Contrib.C' num2str(i) ' = C ;'])
    eval(['ContGr.CGr' num2str(i) ' = CGr ;'])
    W   = Weight;
    WGr = WeightGr;
    S   = Signal;
    C   = Contrib;
    CGr = ContGr;
    
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

% Flip timing of the forecast from A-Z to Z-A
  fcst   = flipdim(fcst,1);
  Yref   = flipdim(Yref,1);
  fDate  = flipdim(fDate,1);
  