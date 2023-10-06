%__________________________________________________________________________
%      SEASONAL ADJUSMENT OF DATA AND MAKE READY FOR PSEUDO REAL
%                          TIME ANALYSIS
%
%                       Jasper de Winter
%                   (De Nederlandsche Bank)
%
%                          March 26, 2019
%
% The program uses the IRIS toolbox to seasonally adjust the data using the
% Census X12 procedure. (http://code.google.com/p/iris-toolbox-project/)
%
% Moreover the setup lenghtens the data by exactly three years, keeping
% intact the ragged edges in the original data.
%__________________________________________________________________________

%__________________________________________________________________________
%
%% I. P R E L I M I N A R I E S
%
%__________________________________________________________________________

%__________________________________________________________________________
% 1. Housekeeping
home(); clear(); close('all');
% addpath 'C:\Werk\Beleid\DFROG\IRIS';
addpath 'G:\EBO\ECMO\de Winter\Werk\Onderzoek\DFROG\IRIS';

%__________________________________________________________________________
% 2. Define database
Base_Dir = pwd;
FileName = [Base_Dir,'\Data_DFROG.csv'];
delete('NL_X12.csv','NL_X12_E.csv');                                       % Delete old csv files

%__________________________________________________________________________
%
%% I. S E A S O N A L  A D J U S T M E N T
%
%__________________________________________________________________________

%__________________________________________________________________________
% 1. Startup IRIS Session
irisstartup;                                                               % Irisstartup
% xls2csv('Data_DFROG.xls');
db         = dbload(FileName);                                             % Load data (first column dates, first row variable names
range      = mm(1985,1):mm(2019,3);                                       % !!!!!!!!!!!!! Define range, endpoint range is last datapoint for the longest serie !!!!!!!!!!!!!

%__________________________________________________________________________
% 2. Apply Census X12 to non-seasonaly adjusted series

disp('_____________________________________________________________');
disp('Applying Census X12 to the data')
disp(FileName);

% db.IINLAMMB = x12(db.IINLAMMB,range);
% db.IINLAQMB = x12(db.IINLAQMB,range);
% db.IINLBCMB = x12(db.IINLBCMB,range);
db.IINLJEME = x12(db.IINLJEME,range);
db.IINLKXMA = x12(db.IINLKXMA,range);
db.IINLKZMA = x12(db.IINLKZMA,range);
% db.IJDEFDMA = x12(db.IJDEFDMA,range);
% db.IJNLFAMA = x12(db.IJNLFAMA,range);
db.IJNLFLMG = x12(db.IJNLFLMG,range);
db.IKGB01MA = x12(db.IKGB01MA,range);
db.IKGD41MA = x12(db.IKGD41MA,range);
db.IKGE11MA = x12(db.IKGE11MA,range);
db.IKHB04MA = x12(db.IKHB04MA,range);
db.IKHB07MA = x12(db.IKHB07MA,range);
db.IMJA01MA = x12(db.IMJA01MA,range);
db.IRAA01MA = x12(db.IRAA01MA,range);
db.IRAA04MA = x12(db.IRAA04MA,range);
db.IRAA05MA = x12(db.IRAA05MA,range);
db.IRAA07MA = x12(db.IRAA07MA,range);
db.IRAA08MA = x12(db.IRAA08MA,range);
db.IRAA38MA = x12(db.IRAA38MA,range);
db.IRAL50MA = x12(db.IRAL50MA,range);
% db.IRAR02MB = x12(db.IRAR02MB,range);
% db.IRAR03MB = x12(db.IRAR03MB,range);
db.IRBA40MA = x12(db.IRBA40MA,range);
db.IRBA42MA = x12(db.IRBA42MA,range);
% db.IRBA51MA = x12(db.IRBA51MA,range);
db.IRBA52MA = x12(db.IRBA52MA,range);
db.IRBA53MA = x12(db.IRBA53MA,range);
db.IRBA54MA = x12(db.IRBA54MA,range);
db.IRBA62MA = x12(db.IRBA62MA,range);
db.IRBA63MA = x12(db.IRBA63MA,range);
db.IRCM20MA = x12(db.IRCM20MA,range);
db.IRDE31MA = x12(db.IRDE31MA,range);
db.IRDE37MA = x12(db.IRDE37MA,range);
db.IRDE85MA = x12(db.IRDE85MA,range);
db.IRDE86MA = x12(db.IRDE86MA,range);
db.IRDE87MA = x12(db.IRDE87MA,range);
% db.IRDE89MA = x12(db.IRDE89MA,range);
db.IRDF31MA = x12(db.IRDF31MA,range);
db.IRDF34MA = x12(db.IRDF34MA,range);
db.IRDF35MA = x12(db.IRDF35MA,range);
db.IRDF36MA = x12(db.IRDF36MA,range);
db.IRDF37MA = x12(db.IRDF37MA,range);
db.IRDG02MA = x12(db.IRDG02MA,range);
db.IRDG03MA = x12(db.IRDG03MA,range);
db.IRDG04MA = x12(db.IRDG04MA,range);
db.IRDG05MA = x12(db.IRDG05MA,range);
db.IRDG06MA = x12(db.IRDG06MA,range);
db.IRDG07MA = x12(db.IRDG07MA,range);
db.IRDG08MA = x12(db.IRDG08MA,range);
db.IRDG11MA = x12(db.IRDG11MA,range);
db.IRDG12MA = x12(db.IRDG12MA,range);
db.IRDG20MA = x12(db.IRDG20MA,range);
db.IRDK11MA = x12(db.IRDK11MA,range);
% db.IRAD01MA = x12(db.IRAD01MA,range);
% db.IRAD02MA = x12(db.IRAD02MA,range);
% db.IRAD03MA = x12(db.IRAD03MA,range);
% db.IRAD04MA = x12(db.IRAD04MA,range);
% db.IRAD11MA = x12(db.IRAD11MA,range);
% db.IRAD12MA = x12(db.IRAD12MA,range);
% db.IRAD13MA = x12(db.IRAD13MA,range);
% db.IRAD14MA = x12(db.IRAD14MA,range);
% db.IRAD21MA = x12(db.IRAD21MA,range);
% db.IRAD22MA = x12(db.IRAD22MA,range);
% db.IRAD23MA = x12(db.IRAD23MA,range);
% db.IRAD24MA = x12(db.IRAD24MA,range);
% db.IRAD31MA = x12(db.IRAD31MA,range);
% db.IRAD32MA = x12(db.IRAD32MA,range);
% db.IRAD33MA = x12(db.IRAD33MA,range);
% db.IRAD34MA = x12(db.IRAD34MA,range);
% db.IRAD41MA = x12(db.IRAD41MA,range);
% db.IRAD42MA = x12(db.IRAD42MA,range);
% db.IRAD43MA = x12(db.IRAD43MA,range);
% db.IRAD44MA = x12(db.IRAD44MA,range);
% db.IRAD51MA = x12(db.IRAD51MA,range);
% db.IRAD52MA = x12(db.IRAD52MA,range);
% db.IRAD53MA = x12(db.IRAD53MA,range);
% db.IRAD54MA = x12(db.IRAD54MA,range);
% db.IRAD81MA = x12(db.IRAD81MA,range);
% db.IRAD82MA = x12(db.IRAD82MA,range);
% db.IRAD83MA = x12(db.IRAD83MA,range);
% db.IRAD84MA = x12(db.IRAD84MA,range);
db.IRAD85MA = x12(db.IRAD85MA,range);
% db.IRAD86MA = x12(db.IRAD86MA,range);
% db.IRAD87MA = x12(db.IRAD87MA,range);
db.IRAD88MA = x12(db.IRAD88MA,range);
db.IRAD89MA = x12(db.IRAD89MA,range);
db.IRAD90MA = x12(db.IRAD90MA,range);
db.IRAD91MA = x12(db.IRAD91MA,range);
db.IRAD92MA = x12(db.IRAD92MA,range);
% db.IRAD93MA = x12(db.IRAD93MA,range);
% db.IRAD94MA = x12(db.IRAD94MA,range);
% db.IRAD95MA = x12(db.IRAD95MA,range);
% db.IRAD96MA = x12(db.IRAD96MA,range);
% db.IRAD97MA = x12(db.IRAD97MA,range);
% db.IRAD98MA = x12(db.IRAD98MA,range);
% db.IRAD99MA = x12(db.IRAD99MA,range);
% db.IRAD100MA = x12(db.IRAD100MA,range);
% db.IRAD101MA = x12(db.IRAD101MA,range);
% db.IRAD102MA = x12(db.IRAD102MA,range);
% db.IRAD103MA = x12(db.IRAD103MA,range);
% db.IRAD104MA = x12(db.IRAD104MA,range);
% db.IRAD105MA = x12(db.IRAD105MA,range);
% db.IRAD106MA = x12(db.IRAD106MA,range);
% db.IRAD107MA = x12(db.IRAD107MA,range);
% db.IRAD108MA = x12(db.IRAD108MA,range);
% db.IRAD109MA = x12(db.IRAD109MA,range);
% db.IRAD110MA = x12(db.IRAD110MA,range);
% db.IRAD111MA = x12(db.IRAD111MA,range);
% db.IRAD112MA = x12(db.IRAD112MA,range);
% db.IRAD113MA = x12(db.IRAD113MA,range);
% db.IRAD114MA = x12(db.IRAD114MA,range);
% db.IRAD115MA = x12(db.IRAD115MA,range);
db.IRAD116MA = x12(db.IRAD116MA,range);
db.IRAD117MA = x12(db.IRAD117MA,range);
db.IRAD118MA = x12(db.IRAD118MA,range);
db.IRAD119MA = x12(db.IRAD119MA,range);
db.IRAD120MA = x12(db.IRAD120MA,range);
db.IRAD121MA = x12(db.IRAD121MA,range);
db.IRAD122MA = x12(db.IRAD122MA,range);
db.IRAD123MA = x12(db.IRAD123MA,range);
db.IRAD124MA = x12(db.IRAD124MA,range);
% db.IRAD125MA = x12(db.IRAD125MA,range);
% db.IRAD126MA = x12(db.IRAD126MA,range);
% db.IRAD127MA = x12(db.IRAD127MA,range);
% db.IRAD128MA = x12(db.IRAD128MA,range);
% db.IRAD129MA = x12(db.IRAD129MA,range);
% db.IRAD130MA = x12(db.IRAD130MA,range);
% db.IRAD131MA = x12(db.IRAD131MA,range);
% db.IRAD132MA = x12(db.IRAD132MA,range);
% db.IRAD133MA = x12(db.IRAD133MA,range);
% db.IRAD134MA = x12(db.IRAD134MA,range);
% db.IRAD135MA = x12(db.IRAD135MA,range);
% db.IRAD136MA = x12(db.IRAD136MA,range);
% db.IRAD137MA = x12(db.IRAD137MA,range);
% db.IRAD138MA = x12(db.IRAD138MA,range);
% db.IRAD139MA = x12(db.IRAD139MA,range);
% db.IRAD140MA = x12(db.IRAD140MA,range);
% db.IRAD141MA = x12(db.IRAD141MA,range);
% db.IRAD142MA = x12(db.IRAD142MA,range);
% db.IRAD143MA = x12(db.IRAD143MA,range);
% db.IRAD144MA = x12(db.IRAD144MA,range);
% db.IRAD145MA = x12(db.IRAD145MA,range);
% db.IRAD146MA = x12(db.IRAD146MA,range);
% db.IRAD147MA = x12(db.IRAD147MA,range);
% db.IRAD148MA = x12(db.IRAD148MA,range);
% db.IRAD149MA = x12(db.IRAD149MA,range);
% db.IRAD150MA = x12(db.IRAD150MA,range);
% db.IRAD151MA = x12(db.IRAD151MA,range);
% db.IRAD152MA = x12(db.IRAD152MA,range);
% db.IRAD153MA = x12(db.IRAD153MA,range);
% db.IRAD154MA = x12(db.IRAD154MA,range);
% db.IRAD155MA = x12(db.IRAD155MA,range);
% db.IRAD156MA = x12(db.IRAD156MA,range);
% db.IRAD157MA = x12(db.IRAD157MA,range);
db.IRAD158MA = x12(db.IRAD158MA,range);
db.IRAD159MA = x12(db.IRAD159MA,range);
db.IRAD160MA = x12(db.IRAD160MA,range);
db.IRAD161MA = x12(db.IRAD161MA,range);
db.IRAD162MA = x12(db.IRAD162MA,range);
db.IRAD163MA = x12(db.IRAD163MA,range);
db.IRAD164MA = x12(db.IRAD164MA,range);
db.IRAD165MA = x12(db.IRAD165MA,range);
db.IRAD166MA = x12(db.IRAD166MA,range);
% db.IRAD167MA = x12(db.IRAD167MA,range);
% db.IRAD168MA = x12(db.IRAD168MA,range);
% db.IRAD169MA = x12(db.IRAD169MA,range);
% db.IRAD170MA = x12(db.IRAD170MA,range);
% db.IRAD171MA = x12(db.IRAD171MA,range);
% db.IRAD172MA = x12(db.IRAD172MA,range);
% db.IRAD173MA = x12(db.IRAD173MA,range);
% db.IRAD174MA = x12(db.IRAD174MA,range);
% db.IRAD175MA = x12(db.IRAD175MA,range);
% db.IRAD176MA = x12(db.IRAD176MA,range);
% db.IRAD177MA = x12(db.IRAD177MA,range);
% db.IRAD178MA = x12(db.IRAD178MA,range);
% db.IRAD179MA = x12(db.IRAD179MA,range);
% db.IRAD180MA = x12(db.IRAD180MA,range);
% db.IRAD181MA = x12(db.IRAD181MA,range);
% db.IRAD182MA = x12(db.IRAD182MA,range);
% db.IRAD183MA = x12(db.IRAD183MA,range);
% db.IRAD184MA = x12(db.IRAD184MA,range);
% db.IRAD185MA = x12(db.IRAD185MA,range);
% db.IRAD186MA = x12(db.IRAD186MA,range);
% db.IRAD187MA = x12(db.IRAD187MA,range);
% db.IRAD188MA = x12(db.IRAD188MA,range);
% db.IRAD189MA = x12(db.IRAD189MA,range);
% db.IRAD190MA = x12(db.IRAD190MA,range);
% db.IRAD191MA = x12(db.IRAD191MA,range);
% db.IRAD192MA = x12(db.IRAD192MA,range);
% db.IRAD193MA = x12(db.IRAD193MA,range);
% db.IRAD194MA = x12(db.IRAD194MA,range);
% db.IRAD195MA = x12(db.IRAD195MA,range);
% db.IRAD196MA = x12(db.IRAD196MA,range);
% db.IRAD197MA = x12(db.IRAD197MA,range);
% db.IRAD198MA = x12(db.IRAD198MA,range);
% db.IRAD199MA = x12(db.IRAD199MA,range);
% db.IRAD200MA = x12(db.IRAD200MA,range);
% db.IRAD201MA = x12(db.IRAD201MA,range);
% db.IRAD202MA = x12(db.IRAD202MA,range);
% db.IRAD203MA = x12(db.IRAD203MA,range);
% db.IRAD204MA = x12(db.IRAD204MA,range);
% db.IRAD205MA = x12(db.IRAD205MA,range);
% db.IRAD206MA = x12(db.IRAD206MA,range);
% db.IRAD207MA = x12(db.IRAD207MA,range);
% db.IRAD208MA = x12(db.IRAD208MA,range);
% db.IRAD209MA = x12(db.IRAD209MA,range);
% db.IRAD210MA = x12(db.IRAD210MA,range);
% db.IRAD211MA = x12(db.IRAD211MA,range);
% db.IRAD212MA = x12(db.IRAD212MA,range);
% db.IRAD213MA = x12(db.IRAD213MA,range);
% db.IRAD214MA = x12(db.IRAD214MA,range);
% db.IRAD215MA = x12(db.IRAD215MA,range);

% 4. Housekeeping
[X] = db2array(db);
clear db range;
save NL_X12;

%__________________________________________________________________________
% 5. End IRIS-session
try
    iriscleanup;
catch
    disp('Error closing IRIS session.');
    disp('Execution will continue.');
end

%__________________________________________________________________________
%
%% III. C R E A T E  P S E U D O  R E A L - T I M E  D A T A B A S E
%
%__________________________________________________________________________

%__________________________________________________________________________
% 1. Housekeeping
home();
clear();
close('all');

%__________________________________________________________________________
% 2. Lengthen Data by "extra" years
extra         = 3;

%__________________________________________________________________________
% 3. Write all series to Matlab-file
disp('_____________________________________________________________');
disp('Saving seasonally adjusted data to ')
load NL_X12;                                                                % laden NL_X12; make sure the last row contains the last observation for the most timely
Base_Dir = [pwd];
FileName = [Base_Dir,'\NL_X12.xls'];
disp(FileName);
xlswrite('NL_X12.xls',X);

s1            = size(X,1);                                                  % observation (empty cells for the other variables)
s2            = size(X,2);

extra         = extra * 12;                                                 % lenghten vector by X years (X*12 months)
X_nan         = NaN(s1+extra,s2);
X_nan(1:s1,:) = X;
X             = X_nan;


%__________________________________________________________________________
% 3. replace last observation plus j by the last observation -j
l(1,:)        = sum(isnan(X(s1-10-extra:end,:)),1);
l(2,:)        = size(X,1);
l(3,:)        = l(2,:)-l(1,:);
l             = l(3,:);

for i=1:1:extra
    for j=1:1:s2
        X(l(:,j)+i,j)=X(l(:,j)-i,j);
    end
end

%__________________________________________________________________________
% Write all series to Excel-file

disp('_____________________________________________________________');
disp('Saving pseudo real-time dataset to')
Base_Dir = [pwd];
FileName = [Base_Dir,'\NL_X12_E.csv'];
disp(FileName);
xlswrite('NL_X12_E.xls',X);

