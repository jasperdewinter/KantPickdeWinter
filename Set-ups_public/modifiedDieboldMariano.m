function [s, sstar] = modifiedDieboldMariano (d, h)

% usage: [s, sstar] = modifiedDieboldMariano (d, h)
%
% Calculates the Diebold and Mariano (1995) test of predictive equality
% and the modified test by Harvey, Leybourne and Newbold (1997)
%
% Input: d - diff in functions of the forecast error = g(e_it)-g(e_jt)
%        h - forecast horizon
% Output: s - Diebold-Mariano stat
%     sstar - modified Diebold-Mariano stat

% Andreas Pick - 13. Nov 2007

n = size(d,1);
if size(d,2) == 1
    g = zeros(h-1,1)*NaN;
    for i = 1:(h-1)
        g(i) = 1/n.*((d(i+1:end)-mean(d,1))'*(d(1:end-i)-mean(d,1)));
    end
    V = 1/n*(var(d) + 2*sum(g));
    if V < 0 % if negative V use Bartlett weights
        V = var(d); 
        for i = 1:(n/2)
            V = V + (1/n)*(1-i/(n/2+1))*((d(i+1:end)-mean(d,1))'*(d(1:end-i)-mean(d,1)));
        end   
    end
    s = mean(d)/sqrt(V);
    sstar = sqrt((n+1-2*h+h*(h-1)/n)/n)*s;
elseif size(d,2) == size(h)
    error ('modifiedDieboldMariano: irregular series of forecast horizons not implemented yet');
elseif  size(d,2) == h
        s = zeros(h,1);
        sstar = zeros(h,1);
    for j = 1:h
        g = zeros(j-1,1)*NaN;
        for i = 1:(j-1)
            g(i) = 1/n.*((d(i+1:end,j)-mean(d(:,j),1))'*(d(1:end-i,j)-mean(d(:,j),1)));
        end
        V = 1/n*(var(d) + 2*sum(g));
        s(j) = mean(d)/sqrt(V);
        sstar(j) = sqrt((n+1-2*j+j*(j-1)/n)/n)*s(j);
    end
else
   error ('modifiedDieboldMariano: size of d wrong');
end