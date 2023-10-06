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
  Base_Dir  = 'g:\ebo\reo\reijer\indicato\netwerk\ecb_taskforce\software_code\ecb\factors_06_stfc_v3\';
  addpath(genpath(Base_Dir));
  cd(Base_Dir);
  
% Data files  
  Xfile = 'data_nl_jul06.xls';    % Excel data file
  Mfile = 'data_nl_jul06.mat';    % Matlab data (output)
  

%_____________________________________________________________________________________
%                                 R E A D    D A T A   
%_____________________________________________________________________________________
  Xfile = [Base_Dir 'Data/dnb/' Xfile];

%___________________________________________________
% Read monthly data
  disp(['Loading monthly data from ' Xfile]);
 
  [A,B]   = xlsread(Xfile,'monthly');
 % Time    = datenum(B(2:end,1),'dd/mm/yy');
 % Time    = datevec(Time);
  M.Date  = A(:,1:2);
 % sizeDif = size(Time,1)-size(A,1);
 % M.Raw   = [A;nan(sizeDif,size(A,2))];
   M.Raw   = A(:,3:size(A,2))
 
  disp(' ');
  disp([' Nr series  '  num2str(size(M.Raw,2))]);
  disp([' Nr obs     '  num2str(size(M.Raw,1))]);
  disp([' Date       '  date2str(M.Date(1,:)) ' - ' ... 
                        date2str(M.Date(end,:))]);
  disp(underline(80));
  
%___________________________________________________
% Read quarterly data  
  disp(['Loading quarterly data from ' Xfile]);
  
  [A,B]   = xlsread(Xfile,'quarterly');
%  Time    = datenum(B(2:end,1),'dd/mm/yy');
%  Q.Date  = datevec(Time);
%  Q.Date  = Q.Date(:,1:2);
  Q.Date  = A(:,1:2);
%  A       = A(:,1);
%  sizeDif = size(Time,1)-size(A,1);
%  Q.Raw   = [A;nan(sizeDif,size(A,2))];
   Q.Raw   = A(:,3:size(A,2));

  disp(' ');
  disp([' Nr series  '  num2str(size(Q.Raw,2))]);
  disp([' Nr obs     '  num2str(size(Q.Raw,1))]);
  disp([' Date       '  date2str(Q.Date(1,:)) ' - ' ... 
                        date2str(Q.Date(end,:))]);
  disp(underline(80));
  
%___________________________________________________
% Read legend  
  disp(['Loading legend from ' Xfile]);
  [A,B]    = xlsread(Xfile,'Code');
   L.Name  = B(2:end,2);  
   L.Code  = A(:,3:5);

  if size(L.Code,1) ~= size(M.Raw,2)
     disp(['Dim of Legend is incorrect  ' size(L.Trf,1)]);
  end
  if size(L.Name,1) ~= size(M.Raw,2)
     disp(['Dim of Names is incorrect   ' size(L.Name,1)]);
  end
  disp(underline(80));
%_____________________________________________________________________________________
%                             P R O C E S S    D A T A   
%_____________________________________________________________________________________
% Transform monthly data & replace outliers with NA
   M.X            = Transform_ECB(M.Raw,L.Code);
  [M.OC, M.List]  = Outliers(M.X,0,[]);
   
% Trim quarterly data such that timing corresponds to monthly data
  date1           = mon2qrt(M.Date(1,:));
  date2           = mon2qrt(M.Date(end,:));
  [Q.Raw Q.Date]  = TrimData(Q.Raw ,Q.Date,date1,date2,'Q');

% Gro rate of GDP
  Q.y    = (Q.Raw(2:end,:) ./ Q.Raw(1:end-1,:) - 1)*100;
  Q.y    = [nan(1,size(Q.y,2)); Q.y];


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
             cellstr('M.Raw  : Raw monthly data as from Populator '),...
             cellstr('M.X    : Mon data transformed (see L.Code)  '),...
             cellstr('M.Date : corresponding date vector          '),...
             cellstr('Q.Raw  : Quarterly GDP                      '),...
             cellstr('Q.y    : Quarterly GDP growth               '),...
             cellstr('Q.Date : corresponding date vector          '),...
             cellstr('L.Name : Mon series mame                    '),...
             cellstr('L.Code:  Transformation codes               '));
 
         
%_____________________________________________________________________________________
%                              S A V E     D A T A   
%_____________________________________________________________________________________
  clc
  KeyBd = input(['Save data to ' Mfile ' (Yes = 1)?']); 
  disp(' ');

  if KeyBd
     Matfile  = [Base_Dir 'Data\EA\' Mfile];
     save(Matfile,'Comment','M', 'Q', 'L','-v6');
  
     disp(' ');
     disp(['Data saved to ' Matfile]);
  end
