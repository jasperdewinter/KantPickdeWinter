function [XB Date c] = Run_EM(X,Date,maxfacn,minfacn)
%__________________________________________________________________________
% function [XB Date c] = Run_EM(X,Date)
%
% Fills in the missing values by EM algorithm. 
% The algorithm does the following:
% (a) selects the balanced panel BP
% (b) computes the set of factors F on BP
% (c) interpolates missing values in non-balanced panel NBP
%     via regression with respect to 'facn' eigenvectors of BP
%     corresponding to the largest eigenvalues
% (d) calculates 'facn' eigenvalues for entire data matrix
% (e) again interpolates missing values via regressiion with respect 
%     to new eigenvectors
% (f) repeats (d) and (e) until convergence is achieved
%
% The code can also interpolate quarterly data. Please note that
% in general case (a) is somehow tricky (and thus in some cases the
% code will fail to work properly, but it is still more waterproof 
% then 'BalanceX' function)
%
% INPUT
%   X         T x n Data matrix
%   Date      T x 2 date matrix
%   maxfacn   Max Nr of factors for the imputation procedure
%   minfacn   Min nr of factors for the imputation procedure
%
% OUTPUT
%   XB      Balanced panel
%   Date    Adjusted dates
%   c       n x 1 vector:  size of shift for each series  
%__________________________________________________________________________

% Algorithm flow
  minofts = 10;    % Min number of time series for the balanced panel
% minfacn = 3;     % Min number of factors for the imputation procedure
% maxfacn = 3;     % Min number of factors for the imputation procedure
  maxiter = 50;    % Max number of EM iterations for the data interpolation

  relconv = 0.001; % Convergence check
  
% Output printing flag
  output = 0;
  
%___________________________________________________  
  [T,n]  = size(X);
  
  d      = zeros(1,n);
  for j = 1:n
      k = 1;
      while isnan(X(T-k+1,j)) && (k<=T)
            d(j) = d(j)+1;
            k = k+1;
      end
  end
  
  e      = zeros(1,n);
  for j = 1:n
      k = 1;
      while isnan(X(k,j)) && (k<=T)
            e(j) = e(j)+1;
            k = k+1;
      end
  end

  lags  = rearrange(d)';
  nofl  = zeros(size(lags,1),1);
  for j = 1:size(lags,1)
      nansum     = sum(d==lags(j,1));
      if nansum ~= 0
         nofl(j,1) = nansum;
      end
  end
  
%___________________________________________________  
  clag  = lags(1);    % Current lag
  cbtsn = 0;          % Current balanced time series number
  es    = 0;          % Examined series
  cpm=0;              % Minimal number of obs to be cut at the begining   

  BP    = []; 
  NBP   = X; 
  c     = zeros(1,size(X,2));
  
%___________________________________________________  
  while (cbtsn<minofts)&&(es<n)
     addBPtag = (d == clag);
    
     for j = 1:size(NBP,2)
         if addBPtag(1,j) == 1
             cure=e(1,j);
             z = lagx(NBP(cure+1:T,j),clag);
             z = z(clag+1:end);
             if sum(isnan(z)) ~= 0
                addBPtag(1,j) = 0;
             else
                c(1,j) = clag;
                cpm=max(cpm,clag+cure);
             end
         end
     end
     addBPind = trim(cumsum(ones(1,size(NBP,2))).*addBPtag);
     nonBPtag = 1-addBPtag;
     nonBPind = trim(cumsum(ones(1,size(NBP,2))).*nonBPtag);
    
     if ~isempty(addBPind)
        BP    = [BP lagx(NBP(:,addBPind),clag*ones(1,size(addBPind,2)))];
     end
     NBP   = NBP(:,nonBPind);
     cbtsn = size(BP,2);
     es    = es+nofl(1);
    
     nofl(1,1) = nofl(1,1)-size(addBPind,2); 
     
     if size(nofl,1) > 1
        nofl(2,1) = nofl(2,1)+nofl(1,1);
        nofl(1,1) = 0;
     end
     
     nofl      = trim(nofl);
     lags(1,1) = 0; 
     lags      = trim(lags);
     if ~isempty(lags)
        clag      = lags(1,1);
     else
        clag=0;
     end
    
     if ~isempty(addBPind)
        d=d+1; e=e+1;
        d(1,addBPind)= 0;
        d      = trim(d);
        e(1,addBPind)= 0;
        e      = trim(e);
        d=d-1; e=e-1;
     end
     
     for j = 1:size(d,2)
         if d(1,j) < clag
            d(1,j) = clag;
         end
     end
  end
  
%___________________________________________________

  if (es >= n) && (cbtsn < minofts)
    error('Algorithm failed to produce sufficient nr of balanced series')
  end
    
  BP   = BP(cpm+1:T,:); NBP  = NBP(cpm+1:T,:);
  BPBP = BP*BP'; 
 
  if sum(sum(isnan(BPBP))) > 0
     BPBP = strange_nan(BPBP,BP);
     disp('Correcting missing values in matrix BPBP')
  end
  F  = GR_eigen(BPBP,maxfacn);
  
% Now we substitute missing values.
% The loop in facn was introduced in order to double-check the number
% of factors that should be taken into account at the stage of data
% imputation and is not really necessary

  facn = minfacn;
  while facn <= maxfacn
    fest = F(:,1:facn);
    
    if output
        disp('Substituting missing values...'); 
    end
  
  % Here the main loop begins
    check       = zeros(maxiter,1);
    iter        = 0;
    convergence = 0;
    
    while iter <= maxiter && convergence == 0
        iter    = iter+1;
                
      % Check the convergence of E-M algorithm.
        lambp       = flamcal1(BP,fest);
        ebp         = BP-(fest*lambp);
        ssr         = sum(sum(ebp.^2,1),2); 
        check(iter) = ssr;
        [rowsNBP colsNBP] = size(NBP);
        newnbp      = zeros(rowsNBP,colsNBP);
        
        for i = 1:colsNBP
            newnbp(:,i) = EM_step(NBP(:,i),fest,1);
        end

        NNBPNNBP = newnbp*newnbp';
        if sum(sum(isnan(NNBPNNBP))) > 0
           NNBPNNBP = strange_nan(NNBPNNBP,newnbp);
           disp('Correcting missing values in matrix NNBPNNBP')
        end

        XX       = BPBP+NNBPNNBP;
        F        = GR_eigen(XX,facn);
        fest     = F(:,1:facn);

      % Check convergence  
        if iter == 1
            ssrold = ssr;
        else
            convergence = ((ssr/ssrold)<(1+relconv));
            ssrold      = ssr;
        end
    end 
    facn = facn+1;
  end 

% Full panel with substituted missing values.
  P = [BP newnbp];
  for i = 1:size(P,2)
      P(:,i) = normx(P(:,i));
  end    
  if output
     disp('Missing values have been replaced');
  end

  c    = c';
  Date = Date(size(Date,1)-size(P,1)+1:size(Date,1),:);
  XB   = P;
  
  
%__________________________________________________________________________
function V = GR_eigen(C,r)
%__________________________________________________________________________
% [F,v,F1] = svd(C);

  if sum(sum(isnan(C))) > 0
     error('Missing values in input matrix')
     return
  end   
  d.disp = 0;
  [V D]  = eigs(C,r,'lm',d);	
  
  
%__________________________________________________________________________
  function XX = strange_nan(XX,X)
 %__________________________________________________________________________

 [r1 c1] = find(isnan(XX));
 
 for i = 1:size(r1,1)
     XX(r1(i),c1(i)) = X(r1(i)) * X(c1(i))';    
 end    
        