%__________________________________________________________________________
%%                  TABLE 5, 6 and 7 Kant, Pick and de Winter
%
%               Dennis Kant, Andreas Pick & Jasper de Winter
%
%                         October 1, 2024
%
%              This program does the following:
%            a) Create output for Table 5,6 & 7
%            b) Ouput displaued on screen
%__________________________________________________________________________

% housekeeping

% Set ROOT directory !!!
ROOT = 'project/root/directory/';

% Adjust filePath to read model forecasts
filePath = fullfile(ROOT, '/Results/fcst/');
cd(filePath)

% Choose what to do
dispBias = 1; % display forecast bias
dispStd = 1; % display forecast standard deviation
dispVariance = 0; % display forecast variance
dispModelAve = 1; % display the model averaging results (always all data)
dispDM = 1; % display the Diebold Mariano test statistics 

% load all the forecasts
dfm = xlsread('fcst results DFM.xlsx','Fcst Results');
midas = xlsread('fcst results MIDAS-F.xlsx','Fcst Results');
ar = xlsread('fcst results AR .xlsx','Fcst Results');
en = xlsread('fcst results EN.xlsx','Fcst Results');
lasso = xlsread('fcst results LASSO.xlsx','Fcst Results');
pm = xlsread('fcst results PM.xlsx','Fcst Results');
rf = xlsread('fcst results RF.xlsx','Fcst Results');
rs = xlsread('fcst results RS.xlsx','Fcst Results');
rp = xlsread('fcst results RP.xlsx','Fcst Results');

% Adjust filePath to execute Diebold-Mariano tests
filePath = fullfile(ROOT, '/Set-ups_public/');
cd(filePath)

% === calculate forecast error, rmsfe, bias^2 and variance ===================
y = dfm(4:end-6,3); % realisation

% forecasts: going from backcasts via nowcasts to forecasts backwards
ydfm = dfm(4:end-6,5:end);
ymidas = midas(4:end-6,5:end);
yar = ar(4:end-6,5:end);
yen = en(4:end-6,5:end);
ylasso = lasso(4:end-6,5:end);
ypm = pm(4:end-6,5:end);
yrf = rf(4:end-6,5:end);
yrs = rs(4:end-6,5:end);
yrp = rp(4:end-6,5:end);

T = size(y,1);
yhat = nan(T,11,9);
yhat(:,:,3) = ydfm;
yhat(:,:,4) = ymidas;
yhat(:,:,2) = yar;
yhat(:,:,6) = yen;
yhat(:,:,5) = ylasso;
yhat(:,:,1) = ypm;
yhat(:,:,9) = yrf;
yhat(:,:,7) = yrs;
yhat(:,:,8) = yrp;

% forecast errors
err = nan(T,11,9);
err(:,:,3) = ydfm - y*ones(1,11);
err(:,:,4) = ymidas - y*ones(1,11);
err(:,:,2) = yar - y*ones(1,11);
err(:,:,6) = yen - y*ones(1,11);
err(:,:,5) = ylasso - y*ones(1,11);
err(:,:,1) = ypm - y*ones(1,11);
err(:,:,9) = yrf - y*ones(1,11);
err(:,:,7) = yrs - y*ones(1,11);
err(:,:,8) = yrp - y*ones(1,11);

% rmsfe
rmsfe = nan(11,9); % # horizons, # methods
rmsfe(:,:) = sqrt(mean(err(:,:,:).^2,1));
rmsfe = flipud(rmsfe);
ratio_rmsfe = rmsfe./(rmsfe(:,1)*ones(1,9));
min_rmsfe = min(rmsfe,[],2);

% sq bias
absbias = nan(11,9);
absbias(:,:) = abs(mean(err,1));
absbias = flipud(absbias);
ratio_absbias = absbias./(absbias(:,1)*ones(1,9));
% min absbias
min_absbias = min(ratio_absbias,[],2);

if dispBias == 1
  disp("Table 5. Absolute forecast bias over the period 1992Q1-2018Q4")
  disp("Absolute bias")
  disp([absbias(:,1) ratio_absbias(:,2:end)])
  disp("Smallest bias")
  disp(ratio_absbias == min_absbias)
  disp('=========')
end

% var
variance = nan(11,9);
variance(:,:) = var(err,[],1);
variance = flipud(variance);
ratio_var = variance./(variance(:,1)*ones(1,9));
% min variance
min_var = min(ratio_var,[],2);

if dispVariance == 1
  disp("Variance")
  disp([variance(:,1) ratio_var(:,2:end)])
  disp(ratio_var == min_var)
  disp('=========')
end

stdev = nan(11,9);
stdev(:,:) = std(err,1,1);
stdev = flipud(stdev);
ratio_std = stdev./(stdev(:,1)*ones(1,9));
% min variance
min_std = min(ratio_std,[],2);

if dispStd == 1
  disp("Table 6. Forecast standard devation over the period 1992Q1-2018Q4")
  disp("Standard devation")
  disp([stdev(:,1) ratio_std(:,2:end)])
  disp('smallest std deviation')
  disp(ratio_std == min_std)
  disp('=========')
end

% === forecast averaging ===================================================

forecasts_in_MA = 3:9; % exclude naive forecasts
nForecasts_in_MA = length(forecasts_in_MA);
errMA = nan(T,11,5);

% equal weights
yew = nan(T,11);
yew(:,:) = mean(yhat(:,:,forecasts_in_MA),3);
errew = yew - y*ones(1,11);
rmsfeew = sqrt(mean(errew.^2))';
rmsfeew = flipud(rmsfeew);
ratio_rmsfeew = rmsfeew./rmsfe(:,1);

errMA(:,:,1) = errew;

% inverse proportional msfe: expanding and 10 obs rolling
yip = nan(T,11);
yip10 = nan(T,11);
for t = 1:T
  % expanding
  if t == 1
    yip(1,:) = mean(yhat(1,:,forecasts_in_MA),3);
  else
    rmsfe_t = nan(11,nForecasts_in_MA); % # horizons, # methods
    rmsfe_t(:,:) = sqrt(mean(err(1:t-1,:,forecasts_in_MA).^2,1));
    yhath = nan(nForecasts_in_MA,1);
    for h = 1:11
      yhath(:) = yhat(t,h,forecasts_in_MA);
      wip = 1./rmsfe_t(h,:).^2;
      wip = wip./sum(wip);
      yip(t,h) = wip*yhath;
    end
  end
  % rolling
  if t < 11
    yip10(t,:) = mean(yhat(t,:,forecasts_in_MA),3);
  else
    rmsfe_t = nan(11,nForecasts_in_MA); % # horizons, # methods
    rmsfe_t(:,:) = sqrt(mean(err(t-10:t-1,:,forecasts_in_MA).^2,1));
    yhath = nan(nForecasts_in_MA,1);
    for h = 1:11
      yhath(:) = yhat(t,h,forecasts_in_MA);
      wip = 1./rmsfe_t(h,:).^2;
      wip = wip./sum(wip);
      yip10(t,h) = wip*yhath;
    end
  end
end

% expanding
errip = yip - y*ones(1,11); %
rmsfeip = sqrt(mean(errip.^2))';
rmsfeip = flipud(rmsfeip);
ratio_rmsfeip = rmsfeip./rmsfe(:,1);
% rolling
errip10 = yip10 - y*ones(1,11);
rmsfeip10 = sqrt(mean(errip10.^2))';
rmsfeip10 = flipud(rmsfeip10);
ratio_rmsfeip10 = rmsfeip10./rmsfe(:,1);

errMA(:,:,2) = errip;
errMA(:,:,3) = errip10;


% OLS: Granger Ramanathan: expanding and rolling 40 obs
ygr = nan(T,11);
ygr40 = nan(T,11);
for t = 1:T
  if t < 41
    ygr(t,:) = mean(yhat(t,:,forecasts_in_MA),3);
    ygr40(t,:) = mean(yhat(t,:,forecasts_in_MA),3);
  else
    yhats = nan(t,nForecasts_in_MA);
    for h = 1:11
      yhats(:,:) = yhat(1:t,h,forecasts_in_MA);
      % expanding
      betas = [ones(t-1,1) yhats(1:t-1,:)]\y(1:t-1);
      ygr(t,h) = [1 yhats(t,:)]*betas;
      % rolling
      betas = [ones(40,1) yhats(t-40:t-1,:)]\y(t-40:t-1);
      ygr40(t,h) = [1 yhats(t,:)]*betas;
    end
  end
end

% expanding
errgr = ygr - y*ones(1,11); %
rmsfegr = sqrt(mean(errgr.^2))';
rmsfegr = flipud(rmsfegr);
ratio_rmsfegr = rmsfegr./rmsfe(:,1);

% rolling
errgr40 = ygr40 - y*ones(1,11);
rmsfegr40 = sqrt(mean(errgr40.^2))';
rmsfegr40 = flipud(rmsfegr40);
ratio_rmsfegr40 = rmsfegr40./rmsfe(:,1);

errMA(:,:,4) = errgr;
errMA(:,:,5) = errgr40;

if dispModelAve == 1
  disp('Tabel 7. Forecast accuracy of forecast combinations, RMSFEs')
  disp([ratio_rmsfeew ratio_rmsfeip ratio_rmsfeip10 ratio_rmsfegr ratio_rmsfegr40])
end

% === DM tests ========
% methods
d = nan(T,1);
s = nan(11,9);
pval = nan(11,9);
stars = zeros(11,5);
for m = [1 2 4:9]
  for n = 1:11
    d(:) = err(:,n,3).^2 - err(:,n,m).^2;
    [s(n,m), sstar] = modifiedDieboldMariano (d, 1);
    pval(n,m) = 1-normcdf(s(n,m));
    stars(n,m) = (pval(n,m) < 0.1) + (pval(n,m) < 0.05) + (pval(n,m) < 0.01);
  end
end
pval = flipud(pval);
stars = flipud(stars);

% model averages
d2 = nan(T,1);
s2 = nan(11,5 );
pval2 = nan(11,5);
stars2 = zeros(11,5);
for m = 1:5
  for n = 1:11
    d2(:) = err(:,n,3).^2 - errMA(:,n,m).^2;
    [s2(n,m), sstar] = modifiedDieboldMariano (d2, 1);
    pval2(n,m) = 1-normcdf(s2(n,m));
    stars2(n,m) = (pval2(n,m) < 0.1) + (pval2(n,m) < 0.05) + (pval2(n,m) < 0.01);
  end
end
pval2 = flipud(pval2);
stars2 = flipud(stars2);

if dispDM == 1
  if dispModelAve == 1
    disp('Tabel 7. Forecast accuracy of forecast combinations, DM-test results')
    disp(pval2)
    disp(stars2)
  end
end