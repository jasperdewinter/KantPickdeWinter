function fcst = fcst21(fcsti)
%__________________________________________________________________________
% fcst = fcst21(fcsti)
% Forms the averages of a number of forecasts overs series (k)
%
% INPUTS
%  fcsti      Forecasts                             (nobs x h x k)
%
% OUTPUT
%  fcst       Averaged forecast                     (nobs x h)
%__________________________________________________________________________

 fcst = mean(fcsti,3);