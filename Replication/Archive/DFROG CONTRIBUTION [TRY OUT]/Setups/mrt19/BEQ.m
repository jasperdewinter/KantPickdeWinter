%__________________________________________________________________________
%%              UNWEIGHTED AVERAGE BRIDGE EQUATION ESTIMATION
%
%                         Jasper de Winter
%                      (De Nederlandsche Bank)
%
%                         February 7 2012
%
%   The program evaluates the forecast performance of a bridge equation
%          to forecast quarterly GDP from monthly series.
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
%                             L O A D   D A T A
%__________________________________________________________________________
F     =  load([Base_Dir Datafile '.mat']);
X      = F.M.Raw(:,serList);
Date  =  F.M.Date;

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
fcstR = [];
for j = 1:size(X,2);
    disp(['Series ' num2str(serList(j))])
    
    [fcstj,Yref,fDate] = PseudoRealTime(Model,P,X(:,j),Y_M,Date,T,...,
        fcstH,Q_a,Country);
    fcstR = cat(3,fcstR,fcstj);
end

% Average the forecasts
fcstR = fcst21(fcstR);
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
    FileName = [Model(5:end) ' ' Country ' ' P.C3,YCode];
    FileName = [Base_Dir,'Output\',MontH,'\',FileName];
    disp('_____________________________________________________________');
    disp('Saving output to Matlab-file ')
    disp(FileName);
    delete([FileName '*.*'])
    save(FileName,'fcstQ','YrefQ','fDateQ','rmse_V','rmse_s',...
        'P','T','serList')
end

flst   = [1:size(rmse_V,1)]';
if saveflag == 2
    wk1write([FileName '_V'],[flipud(flst) rmse_V] ,2,1)
    wk1write([FileName '_s'],[flipud(flst) rmse_s] ,2,1)
end

%__________________________________________________________________________
% 2. Write Forecasts and RMSEs to Excel-file
if saveflag  == 1
    disp('_____________________________________________________________');
    disp('Saving output to Excel-file ')
    disp([FileName,'.xls'])
    warning off MATLAB:xlswrite:AddSheet
    xlswrite(FileName,{'RMSE Evaluation' }             , 'RMSE_V','B2')
    xlswrite(FileName,{ num2str(T.startV)}             , 'RMSE_V','B3')
    xlswrite(FileName,{ num2str(T.endF)  }             , 'RMSE_V','B4')
    xlswrite(FileName,{['MaxLag = ' num2str(P.lags)]}  , 'RMSE_V','B5')
    xlswrite(FileName,{['Mon ser fcst from ' P.Mon]}   , 'RMSE_V','B6')
    xlswrite(FileName,{['Series = ' num2str(serList)]} , 'RMSE_V','B7')
    xlswrite(FileName, [flipud(flst) rmse_V]           , 'RMSE_V','B10')
    
    xlswrite(FileName,{'RMSE pre-sample' }             , 'RMSE_s','B2')
    xlswrite(FileName,{ num2str(T.startF)}             , 'RMSE_s','B3')
    xlswrite(FileName,{ num2str(T.endL)  }             , 'RMSE_s','B4')
    xlswrite(FileName, [flipud(flst) rmse_s]           , 'RMSE_s','B10')
    
    for i = 1:size(fcstQ,3)
        SName = ['EQ_' int2str(i)];
        fcsti =  squeeze(fcstQ(:,:,i));
        xlswrite(FileName,{'Year','Month','Data','Fcst'},SName,'B2');
        xlswrite(FileName,[fDateQ YrefQ fcsti ]         ,SName,'B3');
    end
end
