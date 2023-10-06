function C = Contributions(Y,Weights,Model,Par,t)
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
%        Groups     Indices of groups for each of variables   (n x 1)
%        TAgg       Indication for time aggregation           (nobs x 1)
%                   growth (scalar)
% OUTPUT
%        C.CS       Contributions for the state vector        (n x nobs x r)
%        C.CF       Contributions for the forecast            (n x nobs)
%        C.CoefF    Coefficients for the forecast             (n x nobs)
%        C.Gr       List of groups                            (no of gr x 1)
%        C.CFGr     Contributions of the groups               (corresponding to C.Gr)
%                                                      (      (no of gr x n x nobs)
%_____________________________________________________________________________________
%
% Weights correspond to a state vector at specific time t.
% It is assumed that the forecast is calculated as beta times this state
% vector, so if GDP is not a part of a state vector beta is the parameter
% estimated by OLS, if it belongs to the SV then beta is the row of S.Z
% corresponding to GDP.
%_____________________________________________________________________________________


eval(['S = ' Model '(Par,[],' num2str(t) ');']);
[T,n]       = size(Y);
r           = size(Weights,1);
beta        = S.Z(end,:);                                                   % beta is laatste rij state vector

S=[];
% Calculate contributions and coefficients
for t = 1:T
    eval(['S = ' Model '(Par,S,' int2str(t) ');']);
    YtC = Y(t,:) - S.c1';
    YtC(isnan(YtC)) = 0;                                                    % replace all NaNs with zero;
    CS(:,:,t)    = Weights(:,:,t) .* repmat(YtC,r,1);                       % multiply element by element (see p. 7 of notes) last row=the weight on yQt (because Yq_t=Yc_t (see notes page 7. and Banbura and Runstler;
    CF(:,:,t)    = beta * CS(:,:,t);                                        % contributions to the forecast (using beta of the state equeation), see page ;
    CoefF(:,:,t) = beta * Weights(:,:,t);
end

CS_p      = permute(CS,[2,3,1]);
CF_p        = squeeze(permute(CF,[2,3,1]));
CoefF_p    = squeeze(permute(CoefF,[2,3,1]));

C.CS    = CS;
C.CF    = CF;                                            
C.CoefF = CoefF;
C.CS_p    = CS_p;
C.CF_p    = CF_p;
C.CoefF_p = CoefF_p;

end

