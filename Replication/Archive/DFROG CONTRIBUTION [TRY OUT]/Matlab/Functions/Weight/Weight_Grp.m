function [Contr Weight] = Weight_G(Yref, YrefQ,CGr, WGr)

%__________________________________________________________________________
%
%         E X T R A C T  B A C K  N O W  A N D F O R E C A S T S
%
% With endpoint 2011 9 B=[1:3:T] gives you the back-, now and forecasts
% for the 3rd month in the quarter, B=[2:3:T] gives you the back-, now and
% forecasts for the 2nd month in the quarter, B=[3:3:T] gives you the 
% back-, now and forecasts for the 1st month in the quarter.
%
%__________________________________________________________________________



%__________________________________________________________________________
%                         P R E L I M I N A R I E S
%__________________________________________________________________________
T  = size(Yref,1);                                             
B1 = (3:3:T);         % Creates Back-. Now- and Forecasts for the 1st month 
B2 = (2:3:T);         % Creates Back-. Now- and Forecasts for the 2nd month
B3 = (1:3:T);         % Creates Back-. Now- and Forecasts for the 3th month 


%__________________________________________________________________________
% M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T 
% M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T 
% M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T 
% M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T 
% M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T  
% M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T 
% M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T 
% M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T H  1 M O N T 
%__________________________________________________________________________

%__________________________________________________________________________
%                            B A C K C A S T S 
%__________________________________________________________________________

for i=B1
    A(i,:) = eval(['CGr.CGr' num2str(i) '(1,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(1,:)']);
end

CBack1=A(B1,:);        % matrix of backcasts contributions
WBack1=Aw(B1,:);       % matrix of backcasts weights

%__________________________________________________________________________
%                             N O W C A S T S 
%__________________________________________________________________________

for i=B1
    A(i,:) = eval(['CGr.CGr' num2str(i) '(2,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(2,:)']);
end

CNow1=A(B1,:);         % matrix of nowcasts contributions
WNow1=Aw(B1,:);        % matrix of nowcasts weights

%__________________________________________________________________________
%                          1 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B1
    A(i,:) = eval(['CGr.CGr' num2str(i) '(3,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(3,:)']);
end

COnequart1=A(B1,:);    % matrix of 1Q ahead forecasts contributions
WOnequart1=Aw(B1,:);   % matrix of 1Q ahead forecasts weights

%__________________________________________________________________________
%                          2 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B1
    A(i,:) = eval(['CGr.CGr' num2str(i) '(4,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(4,:)']);
end

CTwoquart1=A(B1,:);      % matrix of 2Q ahead forecasts contributions
WTwoquart1=Aw(B1,:);     % matrix of 2Q ahead forecasts weight






%__________________________________________________________________________
% M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T 
% M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T 
% M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T 
% M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T 
% M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T 
% M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T 
% M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T 
% M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T H  2 M O N T 
%__________________________________________________________________________
%__________________________________________________________________________
%                            B A C K C A S T S 
%__________________________________________________________________________

for i=B2
    A(i,:) = eval(['CGr.CGr' num2str(i) '(1,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(1,:)']);
end

CBack2=A(B2,:);        % matrix of backcasts contributions
WBack2=Aw(B2,:);       % matrix of backcasts weights

%__________________________________________________________________________
%                             N O W C A S T S 
%__________________________________________________________________________

for i=B2
    A(i,:) = eval(['CGr.CGr' num2str(i) '(2,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(2,:)']);
end

CNow2=A(B2,:);         % matrix of nowcasts contributions
WNow2=Aw(B2,:);        % matrix of nowcasts weights

%__________________________________________________________________________
%                          1 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B2
    A(i,:) = eval(['CGr.CGr' num2str(i) '(3,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(3,:)']);
end

COnequart2=A(B2,:);    % matrix of 1Q ahead forecasts contributions
WOnequart2=Aw(B2,:);   % matrix of 1Q ahead forecasts weights

%__________________________________________________________________________
%                          2 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B2
    A(i,:) = eval(['CGr.CGr' num2str(i) '(4,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(4,:)']);
end

CTwoquart2=A(B2,:);      % matrix of 2Q ahead forecasts contributions
WTwoquart2=Aw(B2,:);     % matrix of 2Q ahead forecasts weight










%__________________________________________________________________________
% M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T 
% M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T 
% M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T 
% M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T 
% M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T 
% M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T 
% M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T 
% M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T H  3 M O N T 
%__________________________________________________________________________
%__________________________________________________________________________
%                            B A C K C A S T S 
%__________________________________________________________________________

for i=B3
    A(i,:) = eval(['CGr.CGr' num2str(i) '(1,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(1,:)']);
end

CBack3=A(B3,:);        % matrix of backcasts contributions
WBack3=Aw(B3,:);       % matrix of backcasts weights

%__________________________________________________________________________
%                             N O W C A S T S 
%__________________________________________________________________________

for i=B3
    A(i,:) = eval(['CGr.CGr' num2str(i) '(2,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(2,:)']);
end

CNow3=A(B3,:);         % matrix of nowcasts contributions
WNow3=Aw(B3,:);        % matrix of nowcasts weights

%__________________________________________________________________________
%                          1 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B3
    A(i,:) = eval(['CGr.CGr' num2str(i) '(3,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(3,:)']);
end

COnequart3=A(B3,:);    % matrix of 1Q ahead forecasts contributions
WOnequart3=Aw(B3,:);   % matrix of 1Q ahead forecasts weights

%__________________________________________________________________________
%                          2 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B3
    A(i,:) = eval(['CGr.CGr' num2str(i) '(4,:)']);
    Aw(i,:)= eval(['WGr.WGr' num2str(i) '(4,:)']);
end

CTwoquart3=A(B3,:);      % matrix of 2Q ahead forecasts contributions
WTwoquart3=Aw(B3,:);     % matrix of 2Q ahead forecasts weight


%__________________________________________________________________________
%                C O N C A T E N A T E       M A T R I C E S 
%__________________________________________________________________________
temp    = NaN((size(CBack1,1)),1);

% Concatenate horizontal and flip dimension; include Columns of NaN between matrices
CBack        = flipud(horzcat(    CBack3,    (sum(CBack3,2)),temp,    CBack2,(    sum(CBack2,2)),temp,    CBack1,    (sum(CBack1,2)),temp));
CNow         = flipud(horzcat(     CNow3,     (sum(CNow3,2)),temp,     CNow2,     (sum(CNow2,2)),temp,     CNow1,     (sum(CNow1,2)),temp));
COne         = flipud(horzcat(COnequart3,(sum(COnequart3,2)),temp,COnequart2,(sum(COnequart2,2)),temp,COnequart1,(sum(COnequart1,2)),temp));
CTwo         = flipud(horzcat(CTwoquart3,(sum(CTwoquart3,2)),temp,CTwoquart2,(sum(CTwoquart2,2)),temp,CTwoquart1,(sum(CTwoquart1,2)),temp));

WBack        = flipud(horzcat(    WBack3,    (sum(WBack3,2)),temp,    WBack2,(    sum(WBack2,2)),temp,    WBack1,    (sum(WBack1,2)),temp));
WNow         = flipud(horzcat(     WNow3,     (sum(WNow3,2)),temp,     WNow2,     (sum(WNow2,2)),temp,     WNow1,     (sum(WNow1,2)),temp));
WOne         = flipud(horzcat(WOnequart3,(sum(WOnequart3,2)),temp,WOnequart2,(sum(WOnequart2,2)),temp,WOnequart1,(sum(WOnequart1,2)),temp));
WTwo         = flipud(horzcat(WTwoquart3,(sum(WTwoquart3,2)),temp,WTwoquart2,(sum(WTwoquart2,2)),temp,WTwoquart1,(sum(WTwoquart1,2)),temp));

% Concatenate vertical and include rows of NaN between matrices
% 40 komt van DFM zoals gebruikt voor je paper zodat de
% voorspellingen lopen tot en met 2020Q4. De YrefQ was destijd 55 lang. 
% ALs je meer dat hebt moet het aantal rijen in temp navenat kleiner zijn.
% Dus een waarneming van Y meer, dan moet je 40+(55-56)=39 rijen
% tussenvoegen. En als je twee Ys meer hebt dan in het paper moet je
% 40+(55-57)=38 rijen invoegen. En dit loopt zo verder door.

temp         = NaN((40+(55-size(YrefQ,1))),(size(CBack,2)));
Contr        = vertcat(CBack,temp,CNow,temp,COne,temp,CTwo,temp);

temp         = NaN((40+(55-size(YrefQ,1))),(size(WBack,2)));
Weight       = vertcat(WBack,temp,WNow,temp,WOne,temp,WTwo,temp);

end