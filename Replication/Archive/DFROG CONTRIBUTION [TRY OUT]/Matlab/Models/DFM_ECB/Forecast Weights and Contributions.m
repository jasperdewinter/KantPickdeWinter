%__________________________________________________________________________
%              RECURSIVE EVALUATION OF FORECAST PERFORMANCE 
%                     Written by Jasper de Winter
%                       (Dutch Central Bank)
%
%                 
%      The program creates the cumulative (forecast) weights and 
%      calculates contributions to the forecasts. Run this setup after 
%      running the Dynamic Factor Model setup RecFcst_DFM_ECB.m)
%
%      W contains the smoothed weights matrices at each point in time j
%__________________________________________________________________________

s    = size(W,2);                       % grootte weight matrix
Wcum = cell(1,s);                       % create cell containing cumulative weights at time j=1:s

%__________________________________________________________
% Create array containnig the weights at each point in time
%__________________________________________________________
for j=1:s,
    Wj      = W{j};                     % take out matrix W at time j
    Wj      = permute(Wj,[3 2 1]);      % put the rows of for y_Q_t all in last dimension
    Wj      = Wj(:,:,end);              % extract the last row of the weight matrix
    Wcum{j} = cumsum(Wj);               % create Wcum containing a row for the weights for y_Q-t for each point in time j
end

%___________________________________________________________
% Output weights for first quarter of 2009
%____________________________________________________________
for j=1:s,
    temp     = Wcum{j};
    temp     = temp(531,:);
    w531(j,:)= temp;
end


