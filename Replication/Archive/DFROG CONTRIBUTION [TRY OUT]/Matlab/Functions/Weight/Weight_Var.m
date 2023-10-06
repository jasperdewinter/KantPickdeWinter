function [Contr Weight] = Weight_G(Yref, YrefQ, C, W)

%__________________________________________________________________________
%                         P R E L I M I N A R I E S
%__________________________________________________________________________

% Selection matrices
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
    A(i,:)= eval(['W.W' num2str(i) '(1,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(1,:)']);
end
WBack1=A(B1,:); clear A;                % matrix of weight backcasts month 1
CBack1=Ac(B1,:); clear Ac;              % matrix of contr. backcasts month 1
%__________________________________________________________________________
%                            N O W C A S T S 
%__________________________________________________________________________

for i=B1
    A(i,:)= eval(['W.W' num2str(i) '(2,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(2,:)']); 
end
WNow1=A(B1,:); clear A;                 % matrix of weight nowcasts month 1
CNow1=Ac(B1,:); clear Ac;               % matrix f contr.  nowcasts month 1

%__________________________________________________________________________
%                          1 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B1
    A(i,:)= eval(['W.W' num2str(i) '(3,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(3,:)']); 
end
WOnequart1=A(B1,:); clear A;             % matrix of weight 1Q ahead month 1
COnequart1=Ac(B1,:); clear Ac;           % matrix of contr. 1Q ahead month 1

%__________________________________________________________________________
%                          2 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B1
    A(i,:)= eval(['W.W' num2str(i) '(4,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(4,:)']);
end
WTwoquart1=A(B1,:); clear A;             % matrix of weight 2Q ahead month 1
CTwoquart1=Ac(B1,:); clear Ac;           % matrix of contr. 2Q ahead month 1










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
    A(i,:)= eval(['W.W' num2str(i) '(1,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(1,:)']);
end
WBack2=A(B2,:); clear A;                % matrix of weight backcasts month 2
CBack2=Ac(B2,:); clear Ac;              % matrix of contr. backcasts month 2
%__________________________________________________________________________
%                            N O W C A S T S 
%__________________________________________________________________________

for i=B2
    A(i,:)= eval(['W.W' num2str(i) '(2,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(2,:)']); 
end
WNow2=A(B2,:); clear A;                 % matrix of weight nowcasts month 2
CNow2=Ac(B2,:); clear Ac;               % matrix f contr.  nowcasts month 2

%__________________________________________________________________________
%                          1 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B2
    A(i,:)= eval(['W.W' num2str(i) '(3,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(3,:)']); 
end
WOnequart2=A(B2,:); clear A;             % matrix of weight 1Q ahead month 2
COnequart2=Ac(B2,:); clear Ac;           % matrix of contr. 1Q ahead month 2

%__________________________________________________________________________
%                          2 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B2
    A(i,:)= eval(['W.W' num2str(i) '(4,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(4,:)']);
end
WTwoquart2=A(B2,:); clear A;             % matrix of weight 2Q ahead month 2
CTwoquart2=Ac(B2,:); clear Ac;           % matrix of contr. 2Q ahead month 2










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
    A(i,:)= eval(['W.W' num2str(i) '(1,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(1,:)']);
end
WBack3=A(B3,:); clear A;                % matrix of weight backcasts month 3
CBack3=Ac(B3,:); clear Ac;              % matrix of contr. backcasts month 3
%__________________________________________________________________________
%                            N O W C A S T S 
%__________________________________________________________________________

for i=B3
    A(i,:)= eval(['W.W' num2str(i) '(2,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(2,:)']); 
end
WNow3=A(B3,:); clear A;                 % matrix of weight nowcasts month 3
CNow3=Ac(B3,:); clear Ac;               % matrix f contr.  nowcasts month 3

%__________________________________________________________________________
%                          1 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B3
    A(i,:)= eval(['W.W' num2str(i) '(3,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(3,:)']); 
end
WOnequart3=A(B3,:); clear A;             % matrix of weight 1Q ahead month 3
COnequart3=Ac(B3,:); clear Ac;           % matrix of contr. 1Q ahead month 3

%__________________________________________________________________________
%                          2 Q  F O R E C A S T S 
%__________________________________________________________________________

for i=B3
    A(i,:)= eval(['W.W' num2str(i) '(4,:)']);
    Ac(i,:)= eval(['C.C' num2str(i) '(4,:)']);
end
WTwoquart3=A(B3,:); clear A;             % matrix of weight 2Q ahead month 3
CTwoquart3=Ac(B3,:); clear Ac;           % matrix of contr. 2Q ahead month 3




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