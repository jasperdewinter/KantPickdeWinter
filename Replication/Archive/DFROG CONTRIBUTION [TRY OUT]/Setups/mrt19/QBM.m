%__________________________________________________________________________
%%                    QUARTERLY BENCHMARK MODELS
%
%                         Jasper de Winter
%                      (De Nederlandsche Bank)
%
%                         February 7 2012
%
%           This program evaluates forecasts of purely quarterly
%                       univariate benchmark models
%__________________________________________________________________________
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
%                       L O A D   D A T A
%__________________________________________________________________________
F      = load([Base_Dir Datafile '.mat']);
Y      = F.Q.y(:,1);
DateY  = F.Q.Date;

%__________________________________________________________________________
%                R E C U R S I V E     F O R E C A S T S
%__________________________________________________________________________
% Trim data to end of fcst horizon
[Y_C, DateC] = TrimData(Y,DateY,T.startE,Add2Date(T.endF,3*fcstH),'Q');

% Number of fcst steps
% Adjust startF to have complete fcsts in the desired sample
T.startF = Add2Date(T.startF,-3*fcstH);
nR       = DateDiff(T.endF,T.startF)/3 + 1;
fcstR    = nan(nR,fcstH);
Y_ref    = nan(nR,fcstH);
fDate    = nan(nR,2);

disp(['Recursive forecasts for quarterly data ',Country])
disp('Evaluation period:')
disp([Date2str(T.startF) ' - ' Date2str(T.endF)])
disp('  ')

%__________
% LOOP
for s = 1:nR
    
    t = size(Y_C,1) + (1 - fcstH - s);
    disp(['Period ' Date2str(DateC(t,:))])
    
    % Produce forecasts
    switch Model
        case {'RUN_MEN'}                                                    % Full sample mean
            fcst  = nanmean(Y_C(1:end));
            fcst  = kron(ones(fcstH,1),fcst);
        case {'RUN_MER'}                                                    % Recursive mean
            fcst  = nanmean(Y_C(1:t));
            fcst  = kron(ones(fcstH,1),fcst);
        case {'RUN_RNW'}                                                    % Random walk
            fcst  = Y_C(t,:);
            fcst  = kron(ones(fcstH,1),fcst);
        case {'RUN_AAR'}                                                    % AR
            Par.lags   = lags;
            Par.IC     = 'AIC';
            Par.cflag  = 1;
            [fcst b c] = V_AR(Y_C(1:t),Par,fcstH);
            fcst       = fcst(end-fcstH+1:end);
    end
    
    % Store forecasts & reference values (i,j) -> x_(t+j|t)
    fcstR(end-s+1,:,:) = fcst;
    Y_ref(end-s+1,:,:) = Y_C(t+1:t+fcstH,:);
    fDate(end-s+1,:)   = DateC(t,:);
    
end

%__________________________________________________________________________
%                          E V A L U A T I O N
%__________________________________________________________________________
% Rearrange: fcstR(t,j) contains fcsts t+j|t
%            fcstQ(t,j) contains fcsts t|t-j

fcstQ = nan(size(fcstR,1)-fcstH,size(fcstR,2));
YrefQ = nan(size(fcstR,1)-fcstH,size(fcstR,2));

for i = 1:size(fcstQ,2)
    fcstQ(:,i) = fcstR(fcstH-i+1:end-i,i);
    YrefQ(:,i) = Y_ref(fcstH-i+1:end-i,i);
end
fDateQ = fDate(fcstH+1:end,:);

% Calculate RMSE for both evaluation (V) & pre-sample (s) periods
errQ    = fcstQ - YrefQ;
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
    FileName = ['QBM '  Country ' ' Model YCode];
    FileName = [Base_Dir,'Output\',MontH,'\',FileName];
    disp('_____________________________________________________________');
    disp('Saving output to Matlab-file ')
    disp(FileName);
    delete([FileName '*.*'])
    save(FileName,'fcstQ','YrefQ','fDateQ','rmse_V','rmse_s', ...
        'Model','Par','T')
    
    flst = (1:size(rmse_V,1))';
end

%__________________________________________________________________________
% 2. Write Forecasts and RMSEs to Excel-file
if saveflag == 1
    disp('_____________________________________________________________');
    disp('Saving output to Excel-file ')
    disp([FileName,'.xls'])
    warning off MATLAB:xlswrite:AddSheet
    xlswrite(FileName,{'RMSE Evaluation'}       , 'RMSE_V','B2')
    xlswrite(FileName,{ num2str(T.startV)}      , 'RMSE_V','B3')
    xlswrite(FileName,{ num2str(T.endF)  }      , 'RMSE_V','B4')
    xlswrite(FileName,{['Model = ' Model]}      , 'RMSE_V','B5')
    xlswrite(FileName, [flipud(flst) rmse_V]    , 'RMSE_V','B8')
    
    xlswrite(FileName,{'RMSE Pre-sample'}       , 'RMSE_s','B2')
    xlswrite(FileName,{ num2str(T.startF)}      , 'RMSE_s','B3')
    xlswrite(FileName,{ num2str(T.endL)  }      , 'RMSE_s','B4')
    xlswrite(FileName, [flipud(flst) rmse_s]    , 'RMSE_s','B8')
    
    for i = 1:size(fcstQ,3)
        SName =  Model;
        fcsti =  fcstQ(:,:);
        xlswrite(FileName,{'Year','Month','Data','Fcst'},SName,'B2');
        xlswrite(FileName,[fDateQ YrefQ fcsti ]         ,SName,'B3');
    end
end

