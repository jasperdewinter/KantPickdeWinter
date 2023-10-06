%__________________________________________________________________________
%%                  PROCESS DATA FOR ESTIMATION
%
%                         Jasper de Winter
%                      (De Nederlandsche Bank)
%
%                          March 26, 2019
%
%                This program does the following:
% a) Loads data from EXCEL
% b) Transforms the data & brings the timing of m & q data in line
% c) Plots monthly data with missing data & outliers
% d) creates a MATLAB data file that contains the transformed data
%__________________________________________________________________________

%__________________________________________________________________________
% Folder
Folder = pwd;

%__________________________________________________________________________
% 1. Estimation file
ESTIMATION

%__________________________________________________________________________
% 2. Add path
addpath(genpath(Base_Dir));
cd(Base_Dir);

%__________________________________________________________________________
% 3. Define Data-files
Data         = ['data_',CCode, MontH];
Datafile     = ['Data\DNB\data_',CCode];
Datafile     = [Datafile,MontH];
Xfile        = [Datafile,'.xlsx'];                                          % !!!!!!!!!!!!!! ADJUST !!!!!!!!!!!!!!! Excel data file
Mfile        = [Datafile,'.mat'];                                           % !!!!!!!!!!!!!! ADJUST !!!!!!!!!!!!!!! Matlab data (output)

%__________________________________________________________________________
%
%% I. R E A D    D A T A
%__________________________________________________________________________
Xfile       = [Base_Dir,Xfile];

%__________________________________________________________________________
% 1. Read monthly data
disp(['Loading monthly data from ' Xfile]);

[A,B]   = xlsread(Xfile,'monthly');
M.Date  = A(:,1:2);
M.Raw   = A(:,3:size(A,2));

disp(' ');
disp([' Nr series  '  num2str(size(M.Raw,2))]);
disp([' Nr obs     '  num2str(size(M.Raw,1))]);
disp([' Date       '  Date2str(M.Date(1,:)) ' - ' ...
    Date2str(M.Date(end,:))]);
disp(underline(80));

%__________________________________________________________________________
% 2. Read quarterly data
disp(['Loading quarterly data from ' Xfile]);

[A,B]   = xlsread(Xfile,'quarterly');
Q.Date  = A(:,1:2); Q.Date(:,2)=3*Q.Date(:,2);
Q.Raw   = A(:,3:size(A,2));

disp(' ');
disp([' Nr series  '  num2str(size(Q.Raw,2))]);
disp([' Nr obs     '  num2str(size(Q.Raw,1))]);
disp([' Date       '  Date2str(Q.Date(1,:)) ' - ' ...
    Date2str(Q.Date(end,:))]);
disp(underline(80));

%__________________________________________________________________________
% 3. Read quarterly gdp
disp(['Loading quarterly gdp from ' Xfile]);

[A,B]   = xlsread(Xfile,'quarterly');
Q.gdp   = A(:,3);

disp(underline(80));

%__________________________________________________________________________
% 4. Read legend
disp(['Loading legend from ' Xfile]);
[A,B]   = xlsread(Xfile,'description');
L.Name  = B(3:end,5);                                                       % Matlab 2018a
L.Code  = A(3:end,2:4);                                                     % Matlab 2018a
L.GName = B(3:end,20);                                                      % Matlab 2018a
L.GCode = A(3:end,20);                                                      % Matlab 2018a

% L.Name  = B(3:end,5);                                                     % Matlab 2018a
% L.Code  = A(1:end,2:4);                                                   % Matlab 2018a
% L.GName = B(3:end,20);                                                    % Matlab 2018a
% L.GCode = A(1:end,20);                                                    % Matlab 2018a

% L.Name  = B(3:end,5);                                                     % Matlab 2017b
% L.Code  = A(1:end,2:4);                                                   % Matlab 2017b
% L.GName = B(3:end,20);                                                    % Matlab 2017b
% L.GCode = A(1:end,20);                                                    % Matlab 2017b

if size(L.Code,1) ~= (size(M.Raw,2)+size(Q.Raw,2))
    disp(['Dim of Legend is incorrect  ' size(L.Code,1)]);
end
if size(L.Name,1) ~= (size(M.Raw,2)+size(Q.Raw,2))
    disp(['Dim of Names is incorrect   ' size(L.Name,1)]);
end
disp(underline(80));
%__________________________________________________________________________
%
% II. P R O C E S S    D A T A
%__________________________________________________________________________

%__________________________________________________________________________
% 1. Transform monthly data & replace outliers with NA
M.X             = Transform(M.Raw,L.Code(1:size(M.Raw,2),:),P.yoy,0);
% [M.OC, M.List]  = Outliers(M.X,0,[]);
[M.OC, M.List]  = Outliers(M.X,3,[]);

%__________________________________________________________________________
% 2. Trim quarterly data such that timing corresponds to monthly data
date1           = mon2qrt(M.Date(1,:));
date2           = mon2qrt(M.Date(end,:));
[Q.Raw Q.Date]  = TrimData(Q.Raw ,Q.Date,date1,date2,'Q');

%__________________________________________________________________________
% 3. Transform quarterly data & replace outliers with NA
Q.X = Transform(Q.Raw,L.Code(size(M.Raw,2)+1:size(L.Code,1),:),P.yoy,1);        
% [Q.OC, Q.List]  = Outliers(Q.X,0,[]);  
[Q.OC, Q.List]  = Outliers(Q.X,3,[]);                                       

%__________________________________________________________________________
% 4. Calculate Growth rate of GDP
% Q.y             = 100*Transform_ECB(Q.Raw(:,1),[1 1 0]);                  
Q.y             = 100*Transform(Q.Raw(:,1),[1 1 0],P.yoy,1);                    


%__________________________________________________________________________
%
%% III. P L O T    D A T A
%__________________________________________________________________________

% KeyBd = input('Do you want to plot data (Yes = 1)?');
% if KeyBd
%     plotM(M,L,[],[]);
% end

%__________________________________________________________________________
%
%% IV. C R E A T E  M A T L A B   D A T A F I L E
%__________________________________________________________________________

%__________________________________________________________________________
% 1. Remove elements
M = rmfield(M,{'OC','List'});

%__________________________________________________________________________
% 2. Description
Comment  = cat(1,...
    cellstr({['From ' Xfile                             ]}),...
    cellstr('M.Raw  : Raw monthly data as from source    '),...
    cellstr('M.X    : Mon data transformed (see L.Code)  '),...
    cellstr('M.Date : corresponding date vector          '),...
    cellstr('Q.Raw  : Quarterly data as from source      '),...
    cellstr('Q.X    : Quarterly data transformed         '),...
    cellstr('Q.y    : Quarterly GDP growth               '),...
    cellstr('Q.Date : corresponding date vector          '),...
    cellstr('L.Name : series name                        '),...
    cellstr('L.Code:  Transformation codes               '));

%__________________________________________________________________________
% 3. Save Data
% clc
% KeyBd = input(['Save data to ',Data,YCode,' (Yes = 1)?']);
% disp(' ');

M.X = real(M.X)                                                             %NEW, you had imaginary parts somehow (d.d.29.01.2013)

% if KeyBd
    Matfile  = [Base_Dir,Mfile];
    save(Matfile,'Comment','M', 'Q', 'L','-v6');
    
    disp(' ');
    disp(['Data saved to ' Matfile]);
% end