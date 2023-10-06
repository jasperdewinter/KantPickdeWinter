%__________________________________________________________________________
%%                      STATIC FACTOR MODEL ESTIMATION
%
%                         Jasper de Winter
%                      (De Nederlandsche Bank)
%
%                          February 7 2012
%
%      The program evaluates the forecast performance of the model by
%      Agostino & Giannone (2005); this combines static principal
%      components with a Stock&Watson(1999) h-step forecasting equation
%      for GDP. Monthly data are balanced to the end of the quarter
%      The program runs recursive forecasts and calculates the RMSEs
%
%__________________________________________________________________________
%
%%                     0. M O D E L  P R E L I M I N A R I E S
%__________________________________________________________________________
tic

%__________________________________________________________________________
% Estimation file
ESTIMATION

%__________________________________________________________________________
% Add path
addpath(genpath(Base_Dir));

%__________________________________________________________________________
% Data
Data         = ['data_', CCode, MontH];
Datafile     = ['Data\dnb\data_',CCode];
Datafile     = [Datafile,MontH];

%__________________________________________________________________________
%                              L O A D   D A T A
%__________________________________________________________________________
F      = load([Base_Dir Datafile '.mat']);
X      = F.M.Raw(:,serList);
Date   = F.M.Date;

if     P.C3 == 'A'  ; F.L.Code(:,3) = 3;
elseif P.C3 == 'M'  ; F.L.Code(:,3) = 1;
else   error('Set P.C3 correctly')
end

X      = Transform(X,F.L.Code(serList,:),P.yoy,0);
Y      = F.Q.y(:,1);
Y_M    = q2m(Y,F.Q.Date,Date(1,2),Date(end,2));

%__________________________________________________________________________
%                     O B T A I N   F O R E C A S T S
%__________________________________________________________________________
% Run Pseudo-realtime forecasts
[fcstR Yref fDate P] = PseudoRealTime(Model,P,X,Y_M,Date,T,fcstH,Q_a,Country);
[fcstQ YrefQ fDateQ errQ] = reshuffle(fcstR,Yref,fDate);

% Calculate RMSE for both evaluation (V) & pre-sample (s) periods
rmse_V  = calc_rmse(errQ,fDateQ,T.startV,T.endF,'Q');

rmse_s = nan(size(rmse_V));
T.endL = [];
if DateFind(fDateQ,T.startV) > 1
    T.endL  = fDateQ(DateFind(fDateQ,T.startV)-1,:);
    rmse_s  = calc_rmse(errQ,fDateQ,T.startF,T.endL,'Q');
end


%__________________________________________________________________________
%                              S A V E
%__________________________________________________________________________

%__________________________________________________________________________
% 1. Write Forecasts and RMSEs to Matlab-file
if saveflag == 1
    if P.crit == 0 ; ext = [P.C3 ' loop s' num2str(P.shift)];
    else           ; ext = [P.C3 ' opt s'  num2str(P.shift)];
    end
    FileName = [Model(5:end) ' ' Country ' ' ext,YCode ];
    FileName = [Base_Dir,'Output\',MontH,'\',FileName];
    
    disp('_____________________________________________________________');
    disp('Saving output to Matlab-file ')
    disp(FileName);
    delete([FileName '*.*'])
    save(FileName,'fcstQ','YrefQ','fDateQ','rmse_V','rmse_s','P','T')
end

%__________________________________________________________________________
% 2. Save RMSes
flst = [1:size(rmse_V,1) 0]';
if P.crit == 0
    rmse_V = [P.r ; rmse_V];
    rmse_s = [P.r ; rmse_s];
else
    rmse_V = [99  ; rmse_V];
    rmse_s = [99  ; rmse_s];
end

%__________________________________________________________________________
% 3. Write Forecasts and RMSEs to Excel-file
if saveflag == 1
    disp('_____________________________________________________________');
    disp('Saving output to Excel-file ')
    disp([FileName,'.xls'])
    for i = 1:size(fcstQ,3)
        SName = ['r',int2str(P.r(i))];
        fcsti =  squeeze(fcstQ(:,:,i));
        xlswrite(FileName,{'Year','Month','Data','Fcst'},SName,'B2');
        xlswrite(FileName,[fDateQ YrefQ fcsti ]         ,SName,'B3');
    end
end