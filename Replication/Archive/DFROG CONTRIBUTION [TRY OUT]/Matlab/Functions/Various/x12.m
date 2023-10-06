function varargout = x12(x,startdate,varargin)
%
% X12  Census X12-ARIMA seasonal adjustment or other time series components.
%
% Syntax:
%   [x1,...,xn,output,error] = x12(x,startdate)
%   [x1,...,xn,output,error] = x12(x,startdate,...)
% Arguments:
%   x1,...,xn numeric; output cellstr; error cellstr; x numeric; startdate numeric
% Options:
%   'arima' logical (false)
%   'display' logical (false)
%   'mode' char|numeric ('sign')
%   'output' char ('d11')
%   'tdays' logical (false)
%
% IRIS Toolbox 2006/11/20 

if ~isnumeric(x) && ~isnumeric(freq), error('Incorrect type of input argument(s).'); end

default = {
  'arima',false,...
  'display',false,...
  'method',[],...
  'mode','sign',...
  'output','d11',...
  'tdays',false,...
};
options = passopt(default,varargin{1:end});
if ~isempty(options.method), options.mode = options.method; end

if isa(options.display,'char')
  if strcmp(options.display,'on')
    options.display = true;
  elseif strcmp(options.display,'off')
    options.display = false;
  end
end

if isnumeric(options.mode)
  if options.mode == 0
    options.mode = 'mult';
  elseif options.mode == 1
    options.mode = 'add';
  elseif options.mode == -1
    options.mode = 'sign';
  end
elseif isa(options.mode,'char')
  if strcmp(options.mode,'m')
    options.mode = 'mult';
  elseif strcmp(options.mode,'a')
    options.mode = 'add';
  elseif strcmp(options.mode,'s')
    options.mode = 'sign';
  end
end

% function body ---------------------------------------------------------------------------------------------

config = load('irisconfig.mat');

if isempty(config.x12exepath)
  error('Census X12 procedure not linked. Unable to use tseries/x12 function.');
end

if options.arima == true && isempty(config.x12mdlpath)
  error('Automodel for Census X12-ARIMA procedure not linked. Unable to use tseries/x12 function with ''arima'' option.');
end

if length(startdate) == 1, startdate = startdate(1,ones([1,size(x,2)])); end
freq = datfreq(startdate);
if any(freq ~= 4 & freq ~= 12)
  warning_(1);
  return
end

outputfile = {};
errorfile = {};

output = regexp(options.output,'[a-zA-Z]\d\d','match');
noutput = length(output);
nx = size(x,2);
[varargout{1:noutput}] = deal(nan(size(x)));
% output file(s)
varargout{noutput+1}(1:nx) = {''};
% error file(s)
varargout{noutput+2}(1:nx) = {''};

for i = 1 : nx
  sample = getsample(transpose(x(:,i)));
  data = x(sample,i);
  if length(data) < 3*freq
    warning_(2);
  elseif any(isnan(data))
    warning_(3); 
  else
    offset = find(sample,1) - 1;
    aux = adjust_(data,startdate(i)+offset,config.x12exepath,config.x12mdlpath,output,options);
    for j = 1 : noutput, varargout{j}(:,i) = aux(:,j); end
    % output file
    if exist('iri$.out') == 2, varargout{noutput+1}(i) = {file2char('iri$.out')}; end
    % error file
    if exist('iri$.err') == 2, varargout{noutput+2}(i) = {file2char('iri$.err')}; end
    delete('iri$.*');
  end
end

end % of primary function -----------------------------------------------------------------------------------

  function data = adjust_(data,startdate,x12exepath,x12mdlpath,output,options) % subfunction ----------------
  ndata = length(data);
  flag = specfile_(data,startdate,x12mdlpath,options);
  if flag == false
    warning_(4);
    return
  end
  if options.display == false, redirect = '>> iri$.catch';
    else redirect = ''; end
  command = sprintf('%s iri$ %s',x12exepath,redirect);
  status = system(command);
  if status ~= 0
    warning_(5);
    return
  end
  [data,flag] = getoutput_(ndata,output);
  if flag == false
    warning_(6);
    return
  end
  end % of subfunction --------------------------------------------------------------------------------------

  function flag = specfile_(data,startdate,x12mdlpath,options); % subfunction -------------------------------
  if strcmp(options.mode,'sign')
    if any(data <= 0), options.mode = 'add';
      else options.mode = 'mult'; end
  end
  newline = [char(13),char(10)];
  [year,per,freq] = dat2ypf(startdate);
  startdate = sprintf('%i.%i',year,per);
  code = file2char('series.spc');
  if options.arima == true
    if strcmp(options.mode,'mult')
      code = [code,newline,file2char('transform.spc')];
    end
    code = [code,newline,file2char('automdl.spc')];
  end
  code = [code,newline,file2char('x11.spc')];
  if options.tdays == true
    code = [code,newline,file2char('x11regression.spc')];
  end
  code = strrep(code,'@output',options.output);
  code = strrep(code,'@mode',options.mode);
  code = strrep(code,'@startdate',startdate);
  code = strrep(code,'@freq',sprintf('%i',freq));
  code = strrep(code,'@data',sprintf('%.8f\n',data));
  code = strrep(code,'@automdlmode','both');
  code = strrep(code,'@automdlfile',sprintf('"%s"',x12mdlpath));
  flag = char2file(code,'iri$.spc');
  end % of subfunction --------------------------------------------------------------------------------------

  function [data,flag] = getoutput_(ndata,output) % subfunction ---------------------------------------------
  flag = true;
  data = nan([ndata,0]);
  for ioutput = output
    fid = fopen(sprintf('iri$.%s',ioutput{1}),'r');  
    if fid > -1
      fgetl(fid); % skip first 2 lines
      fgetl(fid);  
      read = fscanf(fid,'%f %f');
      fclose(fid);
    else
      read = [];
    end
    if length(read) == 2*ndata
      read = transpose(reshape(read,[2,ndata]));
      data(:,end+1) = read(:,2);
    else
      data(:,end+1) = NaN;
      flag = false;
    end
  end
  end % of subfunction --------------------------------------------------------------------------------------