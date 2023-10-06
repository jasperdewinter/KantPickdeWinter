%__________________________________________________________________________
%%            RMSE WEIGHTED AVERAGE BRIDGE EQUATION ESTIMATION
%
%                         Jasper de Winter
%                      (De Nederlandsche Bank)
%
%                         February 7 2012
%
%   The program evaluates the forecast performance of a bridge equation
%   to forecast quarterly GDP from monthly series. The forecasts of
%   the individual Bridge Equations are weighted by the average RMSE
%                  at the respective forecasting horizon
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

%__________________________________________________________________________
%               R M S E  W E I G H T E D  F O R E C A S T S
%__________________________________________________________________________

% Preliminaries
clear A B C D F G H I J Q temp fcstT fcsTOT
fcstQ = []; YrefQ = []; fDateQ =[]; errQ =[]; rmse_V=[];
t=DateDiff(T.endF,T.startF)/3;                                              % create time difference end and begin in quarters

for j = 1:size(X,2);
    [fcstQ(:,:,j) YrefQ(:,:,j) fDateQ(:,:,j) errQ(:,:,j)] = reshuffle(fcstR(:,:,j),Yref,fDate);
end


for j = 1:size(X,2);                                                        % voor alle variabelen j
    for i=1:t;                                                              % en alle tijdstippen tussen begin en eind
        temp            = fDateQ(DateFind(fDateQ,T.endF)-i,:);
        T.endR          = temp(:,1:2);
        rmse_V(:,:,i,j) = calc_rmse(errQ(:,:,j),fDateQ(:,:,j),T.startF,T.endR,'Q');
    end;
end

for j = 1:size(X,2);
    A(:,:,j) = squeeze(fcstQ(:,:,j));
    B(:,:,j) = squeeze(errQ(:,:,j));
    C(:,:,j) = squeeze(rmse_V(:,:,:,j));
    D(:,:,j) = fliplr(rot90(C(:,:,j)));                                     % matrix met RMSEs zodanig draaien dat ie klopt
    E        = squeeze(D(:,:,j));
    F        = ones(size(E));
    D(:,:,j) = F./E;                                                        % matrix met geïnverteerde RMSEs
end

E           = [D;D(end,:,:)];                                               % rij toevoegen aan D zodat de laatste voorspelling ook een RMSE heeft;
F           = sum(E,3);                                                     % som van alle RMSEs

temp.A      = squeeze(YrefQ(:,:,1));
temp.B      = sum(isfinite(temp.A));                                        % nr. of non NaN rows in YrefQ
temp.C      = size(YrefQ,1)-temp.B;                                         % nr. of     NaN rows in YrefQ

for j = 1:size(X,2);
    G       = squeeze(E(:,:,j));                                            % squeeze error matrix
    fcst    = squeeze(fcstQ(:,:,j));                                        % squeeze forecast matrix
    H       = G./F;                                                         % RMSE weights in matrix H
    I       = [H((1:temp.B),:)];                                            % weights from first cell to last known Yref
    J       = [I;repmat(I(end,:),temp.C,1)];                                % repeat last known weights in forecasting period
    W       = [J(1,:);J(1:end-1,:)];                                        % shift weights forward
    fcstT(:,:,j) = W.*fcst;
end

% Calculate weighted average
fcstQ  = sum(fcstT,3);

% Transform YrefQ fdateQ errQ back to non-array form
YrefQ  = squeeze(YrefQ(:,:,1));
fDateQ = squeeze(fDateQ (:,:,1));
errQ   = fcstQ - repmat(YrefQ,1,12);


%__________________________________________________________________________
%               R M S E  W E I G H T E D  F O R E C A S T S
%__________________________________________________________________________

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
    FileName = [Base_Dir,'Output\',MontH,'\',FileName ' RMSE'];
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