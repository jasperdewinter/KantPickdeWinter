%_____________________________________________________________________________________
%                   P R O G R A M     P R O C E S S   D A T A
%                       written by M Banbura and G Ruenstler
%                              (European Central Bank)
%                  Modified to handle Dutch data by Ard den Reijer 31-1-7
%
% This program processes the data input for the GDP short-term forecasting project
% It
% a) Loads data from EXCEL
% b) Transforms the data & brings the timing of m & q data in line
% c) Plots monthly data with missing data & outliers
% d) creates a MATLAB data file that contains the transformed data
%_____________________________________________________________________________________
clear
clc
Base_Dir  = 'G:\EBO\ECMO\de Winter\Werk\Onderzoek\PROJECT 11.  KANT PICK DE WINTER\Replication\DFROG\';

addpath(genpath(Base_Dir));
cd(Base_Dir);

% Data files
Xfile = 'data_nl_mrt19';    % Excel data file
Mfile = 'data_nl_mrt19';    % Matlab data (output)


%_____________________________________________________________________________________
%                                 R E A D    D A T A
%_____________________________________________________________________________________
Xfile = [Base_Dir 'Data/dnb/' Xfile];

%___________________________________________________
% Read monthly data
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

%___________________________________________________
% Read quarterly data
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

%___________________________________________________
% Read quarterly gdp
disp(['Loading quarterly gdp from ' Xfile]);

[A,B]   = xlsread(Xfile,'quarterly');
Q.gdp   = A(:,3);

disp(underline(80));

%___________________________________________________

% Read legend
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
%_____________________________________________________________________________________
%                             P R O C E S S    D A T A
%_____________________________________________________________________________________
% Transform monthly data & replace outliers with NA
%  M.X          = Transform_ECB(M.Raw,L.Code(1:size(M.Raw,2),:));
M.X             = Transform(M.Raw,L.Code(1:size(M.Raw,2),:),0,0);
[M.OC, M.List]  = Outliers(M.X,3,[]);

% Trim quarterly data such that timing corresponds to monthly data
date1           = mon2qrt(M.Date(1,:));
date2           = mon2qrt(M.Date(end,:));
[Q.Raw Q.Date]  = TrimData(Q.Raw ,Q.Date,date1,date2,'Q');

% Transform quarterly data & replace outliers with NA
%  Q.X            = Transform_ECB(Q.Raw,L.Code(size(M.Raw,2)+1:size(L.Code,1),:));
Q.X            = Transform(Q.Raw,L.Code(size(M.Raw,2)+1:size(L.Code,1),:),0,1);

% [Q.OC, Q.List]  = Outliers(Q.X,0,[]);
[Q.OC, Q.List]  = Outliers(Q.X,3,[]);

% Gro rate of GDP
% Q.y    = 100*Transform_ECB(Q.Raw(:,1),[1 1 0]);
Q.y    = 100*Transform(Q.Raw(:,1),[1 1 0],0,1);

%_____________________________________________________________________________________
%                              P L O T    D A T A
%_____________________________________________________________________________________
KeyBd = input('Do you want to plot data (Yes = 1)?');
if KeyBd
    plotM(M,L,[],[]);
end

%_____________________________________________________________________________________
%                            M A K E   D A T A F I L E
%_____________________________________________________________________________________
% Remove elements
M = rmfield(M,{'OC','List'});

% Descriptions
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


%_____________________________________________________________________________________
%                              S A V E     D A T A
%_____________________________________________________________________________________
clc
KeyBd = input(['Save data to ' Mfile ' (Yes = 1)?']);
disp(' ');

if KeyBd
    Matfile  = [Base_Dir 'Data\dnb\' Mfile];
    save(Matfile,'Comment','M', 'Q', 'L','-v6');
    
    disp(' ');
    disp(['Data saved to ' Matfile]);
end