function C = Contributions(Y,Weights,Model,Par,TC,Groups,TAgg)
%_____________________________________________________________________________________
% Calculates different sorts of contributions for given data and weights
% Uses weights obtained from KFWeights.m.
%
% INPUT  
%        Y          Data                                      (nobs x n)  
%        weights    Weights of the data for given state vector
%                   (at given time t)                         (r x n x nobs)
%        Model      Model name                                (string)
%        Par        Corresponding parameters
%        TC         Time of forecast
%        Groups     Indices of groups for each of variables   (n x 1)
%        TAgg       Indication for time aggregation           (nobs x 1)
% OUTPUT 
%        C.CS       Contributions for the state vector        (n x nobs x r)
%        C.CF       Contributions for the forecast            (n x nobs)
%        C.CoefF    Coefficients for the forecast             (n x nobs)
%        C.Gr       List of groups                            (no of gr x 1)
%        C.CFGr     Contributions of the groups      (corresponding to C.Gr) 
%                                                      (no of gr x n x nobs)                        
%_____________________________________________________________________________________
%
% Weights correspond to a state vector at specific time t.
% It is assumed that the forecast is calculated as beta times this state 
% vector, so if GDP is not a part of a state vector beta is the parameter
% estimated by OLS, if it belongs to the SV then beta is the row of S.Z
% corresponding to GDP.
%_____________________________________________________________________________________
  eval(['S = ' Model '(Par,[],' num2str(TC) ');']);
  [T,n]       = size(Y);
  r           = size(Weights,1);
  
  switch Model
      case 'ModBasic'
            beta    = Par.beta(2:end)';                 % eerste beta is de constante (S.c1); zie DFM_ECB
            Weights = Weights(1:Par.r,:,:);
      case 'ModBasic2'
            Y(:,end) = Y(:,end)-S.c2(end);
            beta     = S.Z(end,:);
      case {'ModBasic3','ModBasic6','ModBasic7'}
            %Y(:,end) = Y(:,end)-S.c1(end);
            %beta     = S.Z(21,:) %for IP
            beta     = S.Z(end,:);
      case 'ModExt3'
            nQ       = size(S.Z,1)-S.nd-1;
            beta     = zeros(nQ,size(S.Z,2));
            beta(:,end-nQ+1:end) = eye(nQ);
            %Y        = Y-repmat(S.c1',T,1);
      case 'ModExt4'
            nQ       = size(S.Z,1)-S.nd-1;
            beta     = zeros(nQ,size(S.Z,2));
            beta(:,end-nQ+1:end) = eye(nQ);
  end

  
 S=[];
% Calculate contributions and coefficients
  for t = 1:T
      eval(['S = ' Model '(Par,S,' int2str(t) ');']);
      YtC = Y(t,:) - S.c1';
      YtC(isnan(YtC)) = 0;                                      % replace all NaNs with zero;
      CS(:,:,t)    = Weights(:,:,t) .* repmat(YtC,r,1);         % multiply element by element (see p. 7 of notes) last row=the weight on yQt (because Yq_t=Yc_t (see notes page 7. and Banbura and Runstler;
      CF(:,:,t)    = beta * CS(:,:,t);                          % contributions to the forecast (using beta of the state equeation), see page ;
      %CoefF(:,:,t) = beta * Weights(:,:,t);
      
  end
clear CS
  %CS      = permute(CS,[2,3,1]);
  CF      = squeeze(permute(CF,[2,3,1]));
  %CoefF   = squeeze(permute(CoefF,[2,3,1]));
%  C.CS    = CS;
%  C.CF    = CF;
% C.CoefF = CoefF;

% Aggregating contributions over groups
  if nargin > 5
     gList = unique(Groups);
     for i = 1:length(gList)
         gN        = gList(i);
         CFGr(i,:,:) = sum(CF(ismember(Groups,gN),:,:),1);
     end
     C.Gr   = gList;
     CFGr   = squeeze(CFGr);    %CAREFUL here when gList contains only one group!
     C.CFGr = CFGr;
  end

% Aggregating the contributions over time 
% according to TAgg
  if nargin > 6
      gList = unique(TAgg);
      
      for i = 1:length(gList)
          gN              = gList(i);
          CSTAgg(:,i,:)   = sum(CS(:,ismember(TAgg,gN),:),2);
          CFTAgg(:,i,:)   = sum(CF(:,ismember(TAgg,gN),:)  ,2);
          CFGrTAgg(:,i,:) = sum(CFGr(:,ismember(TAgg,gN),:),2);
      end
   %  C.CSTAgg   = CSTAgg;
   %  C.CFTAgg   = CFTAgg;
      C.CFGrTAgg = CFGrTAgg;
  end

