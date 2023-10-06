function [laggeddata newnames]=lagxn(data,lags,names)

% This procedure takes the marix 'data', the matrix that contains 
% the information about all included lags, called 'lags' and a cell 
% of labels called 'names'. Then every column of 'data' matrix (we assume 
% that columns contain the original data series) is lagged according to 
% the info conveyed by the 'lags' matrix. The initial length of each 
% of the series is preserved as missing observations are substituted 
% with 'NaN's. Then all the lags are concatenated. The output is 
% 'laggeddata' matrix which consists of all the variables and all their lags
% and 'newnames' vector which contains the previous names expanded 
% by a lag tag. The code for 'do nothing' (for the lags matrix) equals
% (-1).
%
% INPUT:
% data - T x n matrix, n time series in cols
% lags - (maxlag+1) x n, matrix of lags
% names - n x 1, cell strings, denoting time series' names 
%
% OUTPUT:
% laggeddata - matrix af data lagged accoridng to lags
% newnames - n x 1, cell of time series' names expanded by 
% transformation tags 
% 
% EXAMPLE:
% If:
% data = rand(100,2) % [x(t) y(t)];
% lags =[ 0 1; 2 -1];
% names ={'name1';'name2'};
% [laggeddata newnames]=lagxn(data,lags,names);
%
% Then:
% laggeddata=[x(t) x(t-2) y(t-1)];
% newnames ={'name1(t)';'name1(t-2)';'name2(t-1)'};

  if size(data,2)~=size(lags,2)
    error('Lagging your martix: the number of columns in lags and data matrices must be equal.')
    return
  end    
  if size(lags,2)~=size(names,1)
    error('Lagging your martix: the number of labels must be equal to the number of time series.')
    return
  end    

  check      = (lags~=-1);
  maxind     = sum(sum(check,1),2);
  [T,n]      = size(data);
  rowslags   = size(lags,1);
  laggeddata = zeros(T,maxind);
  newnames   = cell(n,1);
  index = 0;

  for j = 1:n
      for i = 1:rowslags
          clag = lags(i,j);
          if clag ~= -1
             index  = index+1;
             series = nan(T,1);
             
             series(clag+1:T,:)  = lag(data(:,j),clag);
             laggeddata(:,index) = series;
                
             if clag~=0
                newname = strcat(names{j,1},'(t-',num2str(clag),')');
             else
                newname = names{j,1};
             end
             newnames{index,1} = newname;
          end
      end
    end
  