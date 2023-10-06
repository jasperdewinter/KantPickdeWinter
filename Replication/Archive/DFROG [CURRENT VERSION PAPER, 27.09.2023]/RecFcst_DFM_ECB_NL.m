tic
%__________________________________________________________________________
%              RECURSIVE EVALUATION OF FORECAST PERFORMANCE
%              Written by Marta Banbura & Gerhard Ruenstler
%                       (European Central Bank)
%
%      The program evaluates the forecast performance of the dynamic
%      factor model by Doz et al. (2005) as implemented at the ECB
%      The program runs recursive forecasts and calculates the RMSEs
%__________________________________________________________________________
clear
clc
data_now = clock;
date_now = clock;
date_now = strcat(num2str(date_now(1)),'_',num2str(date_now(2)),'_', num2str(date_now(3)),'_', num2str(date_now(4)), num2str(date_now(5)));
diary(strcat('NL_', 'log', date_now,'.log'));
Base_Dir  = 'G:\EBO\ECMO\de Winter\Werk\Onderzoek\PROJECT 11.  KANT PICK DE WINTER\Replication\DFROG\';
addpath(genpath(Base_Dir));

%____________________________
% Data
% serList is the list of monthly series included in analysis
Country  = 'NL';
Datafile = 'Data\dnb\data_nl_mrt19';

%__________________________________________________________________________
% 3. Series list
serList = [2 3 6 9 19:22 32:34 36 39 44:50 52 58 60:61 63:67 69:82 ...
            84 87:88 90:93 96:98 105:108 135:142 152:154 156 159:164 ...
            166 168:169 171 177:181];

%______________________________
% Data transformations
% Outlier correction is currently not used!!
P.C3    = 'A';                                                              % Transformation of monthly data
% 'A' : Use 3-month growth rates
% 'M' : Use monthly growth rates
P.OC    =  0;                                                               % 2  : Replace outliers with median
                                                                            % 3  : Replace outliers with largest
                                                                            % admissable value (see Outliers)
%_____________________________
% Reference to the model function
if     P.C3 == 'A'; P.Model = 'DFM_ECB';
elseif P.C3 == 'M'; P.Model = 'DFM_ECBm';
else               error('Get P.C3 right')
end

%_____________________________
% Model parameters
% P.r P.p & P.q might be column vectors. If P.crit = P.qflag = 0 then all
% possible combinations from [P.r P.q P.p] with q <=r are used.
% Otherwise specification search is done up to the maximum values of the
% specified lists
%                                                                           %  = 0 use below lists P.r P.q and P.p
%                                                                           %  > 0 use information criteria to determine
%                                                                           %      [r p q] - preferred test = crit = 2
%                                                                           % This uses:
%                                                                           % - Bai & Ng (2002) crit min(N,T) for r
%                                                                           % - AIC                           for p
%                                                                           % - Bai & Ng (2005) test 2        for q
%                                                                           % see also 'help Estim_SPC & Estim_DFP'
%   P.r     =  8;                                                           % Nr of static  factors
%   P.q     =  5;                                                           % Nr of dynamic factors (q < r)
%   P.p     =  3;                                                           % Nr of lags in factor dynamics
%   P.cutE  =  3;                                                           % Nr of obs to cut at eos for estimation

% Use this for the q-o-q model All Combinations
P.crit      = 0;                                                            % Method for spec the nr of stat factors r
P.r         = 1:6;
P.q         = 1:6;
P.p         = 1:6;
P.cutE      = 3;   

%_____________________________
% Timing (in consec order)
T.startE  = [1986  1];                                                     % Start of estimation period              [1,4,7,10]
T.startF  = [1992  1];                                                     % Start of forecasting period             [1,4,7,10]
T.startV  = [1992  3];                                                     % Start of forecast evaluation period     [3,6,9,12]
T.endF    = [2019  9];                                                     % End of forecast evaluation period       [3,6,9,12]
T.Now     = [2022  3]; 

%% Further parameters
fcstH     = 2;                                                              % Forecast horizon (quarters)
Q_a       = [0; 0; 1];                                                      % Pattern of GDP data availability:
                                                                            %   Q_a(i) = 1 -> prev quarter GDP
                                                                            %   is available in mnth i of quart

%_____________________________
% Output flags
plotflag  = 0;                                                              % = i -> plot forecasts done in month i
                                                                            % = 0 -> don't plot
saveflag  = 4;                                                              % > 0 -> save all   to Matlab data file
                                                                            % = 2 -> save RMSE  to wk1
                                                                            % > 2 -> save RMSE  to xls (only  7)
                                                                            % > 3 -> save Fcsts to xls (only Matlab 7)

%_____________________________
% Check dates
if ~ismember(T.startE(2),[1 4 7 10]) || ...
        ~ismember(T.startF(2),[1 4 7 10])
    error('startE & startF must be set at start of quarter')
end
if ~ismember(  T.endF(2),[3 6 9 12])
    error('endF must be set at end of quarter')
end

%__________________________________________________________________________
%                              L O A D   D A T A
%__________________________________________________________________________
F     = load([Base_Dir Datafile '.mat']);
X     = F.M.Raw(:,serList);
Date  = F.M.Date;

if     P.C3 == 'A'  ; F.L.Code(:,3) = 3;
elseif P.C3 == 'M'  ; F.L.Code(:,3) = 1;
else   error('Get P.C3 right')
end
% X     = Transform_ECB(X,F.L.Code(serList,:));
X     = Transform(X,F.L.Code(serList,:),0,0);
Y     = F.Q.y(:,1);
Y_M   = q2m(Y,F.Q.Date,Date(1,2),Date(end,2));

%__________________________________________________________________________
%                     O B T A I N   F O R E C A S T S
%__________________________________________________________________________
% Run Pseudo-realtime forecasts
Model = ['RUN_' P.Model];

[fcstR Yref fDate P] = PseudoRealTime(Model,P,X,Y_M,Date,T,fcstH,Q_a);
[fcstQ YrefQ fDateQ errQ] = reshuffle(fcstR,Yref,fDate);

% Calculate RMSE for both evaluation (V) & pre-sample (s) periods
rmse_V  = calc_rmse(errQ,fDateQ,T.startV,T.endF,'Q');

rmse_s = nan(size(rmse_V));
T.endL = [];
if DateFind(fDateQ,T.startV) > 1
    T.endL  = fDateQ(DateFind(fDateQ,T.startV)-1,:);
    rmse_s  = calc_rmse(errQ,fDateQ,T.startF,T.endL,'Q');
end

% %__________________________________________________________________________
% %                               P L O T
% %__________________________________________________________________________
% if plotflag > 0
%     figure
%     for i = plotflag:3:(size(fcstQ,2)-(3-plotflag))
%         plot((1:size(fcstQ,1))', [YrefQ(:,1) fcstQ(:,i)])
%         legend('GDP',['Fcst quarter '  num2str(floor(i/3)-1)])
%         title(['Forecasts from month ' num2str(plotflag)])
%         wait
%     end
% end

%__________________________________________________________________________
%                              S A V E
%__________________________________________________________________________
if saveflag > 0
    if P.crit == 0 ; ext = [P.C3 ' loop'];
    else            ext = [P.C3 ' opt'];
    end
    FileName = [Model(5:end) ' ' Country ' ' ext];
    FileName = [Base_Dir,'Output\',FileName];
   
    disp('_____________________________________________________________');
    disp('Saving output to ')
    disp(FileName);
    delete([FileName '*.*'])
    save(FileName,'fcstQ','YrefQ','fDateQ','rmse_V','rmse_s','P','T')
end

flst = [1:size(rmse_V,1) zeros(1,3)]';
if P.crit > 0
    ParSet = zeros(3,1);
else
    ParSet = P.ParSet;
end
rmse_V = [P.ParSet ; rmse_V];
rmse_s = [P.ParSet ; rmse_s];

FileName =[FileName,'.xlsx'];                                               % add .xlsx extension

% if saveflag == 2
%     wk1write([FileName '_V'],[flipud(flst) rmse_V] ,2,1)
%     wk1write([FileName '_s'],[flipud(flst) rmse_s] ,2,1)
% end

if saveflag  > 2
    warning off MATLAB:xlswrite:AddSheet
    xlswrite(FileName,{'RMSE Evaluation' }             , 'RMSE_V','B2')
    xlswrite(FileName,{ num2str(T.startV)}             , 'RMSE_V','B3')
    xlswrite(FileName,{ num2str(T.endF)  }             , 'RMSE_V','B4')
    xlswrite(FileName,{['ModSel = ' num2str(P.crit)]}  , 'RMSE_V','B5')
    xlswrite(FileName, [flipud(flst) rmse_V]           , 'RMSE_V','B10')
    
    xlswrite(FileName,{'RMSE pre-sample' }             , 'RMSE_s','B2')
    xlswrite(FileName,{ num2str(T.startF)}             , 'RMSE_s','B3')
    xlswrite(FileName,{ num2str(T.endL)  }             , 'RMSE_s','B4')
    xlswrite(FileName, [flipud(flst) rmse_s]           , 'RMSE_s','B10')
end

if saveflag  > 3
    for i = 1:size(fcstQ,3)
        SName = ['r',int2str(P.ParSet(1,i)),...
            'q',int2str(P.ParSet(2,i)),...
            'p',int2str(P.ParSet(3,i))];
        
        fcsti  = squeeze(fcstQ(:,:,i));
        xlswrite(FileName,{'Year','Month','Data','Fcst'},SName,'B2');
        xlswrite(FileName,[fDateQ YrefQ fcsti ]         ,SName,'B3');
    end
end
toc
diary off