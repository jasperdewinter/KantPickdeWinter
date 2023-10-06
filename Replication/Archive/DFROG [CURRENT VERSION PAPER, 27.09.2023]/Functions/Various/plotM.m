function plotM(M,L,date1,date2)
%__________________________________________________________________________
% function plotM(M,date1,date2)
% plots raw and transformed monthly data
%
% INPUTS (see ReadPopulator.m)
%      M       Structure of monthly data as generated in ReadPopulator.m
%      L       Legend as loaded from Legend.xls
%      date1/2 Start/end date of plot
%__________________________________________________________________________
% Trim  
  if isempty(date1);  date1 = M.Date(1,:)  ; end
  if isempty(date2);  date2 = M.Date(end,:); end
  
  [X1 Date] = TrimData(M.Raw ,M.Date,date1,date2,'M');
  [X2 Date] = TrimData(M.X   ,M.Date,date1,date2,'M');
  [X3 Date] = TrimData(M.OC  ,M.Date,date1,date2,'M');
  [X4 Date] = TrimData(M.List,M.Date,date1,date2,'M');

% Plot  
  Date = Date(:,1) + Date(:,2)/12;
  for j = 1:size(X1,2)
      subplot(4,1,1); 
         plot(Date,X1(:,j)); 
         title([num2str(j) '  ' char(L.Name(j))]);
      subplot(4,1,2); 
         plot(Date,[X2(:,j) X3(:,j)]); 
         title(['Transf Code ' num2str(L.Code(j,:))]);
      V  = axis;   
      subplot(4,1,3); 
         plot(Date,X3(:,j)); 
         title(['Cleaned series']);
         axis(V)
      subplot(4,1,4);  
          bar(Date,X4(:,j));
          title(['Outliers (+) & NaN (-)']);
          axis([V(1:2) -1 1])
      pause
  end

