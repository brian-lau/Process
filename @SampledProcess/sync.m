% should allow resampling
% should handle different Fs for object array
function s = sync(self,event,varargin)

p = inputParser;
p.KeepUnmatched= false;
p.FunctionName = 'SampledProcess sync';
p.addRequired('event',@(x) isnumeric(x));
p.addOptional('window',[],@(x) isnumeric(x) && (size(x,1)==1) && (size(x,2)==2)); 
p.addOptional('commonTime',true,@(x) islogical(x)); 
%p.addParamValue('resample',[]);
p.parse(event,varargin{:});

assert(numel(event)==numel(self),'SampledProcess:sync:InputValue',...
   'numel(event) should match numel(SampledProcess)');

self.setInclusiveWindow;

if isempty(p.Results.window)
   % find window that includes all data
   temp = vertcat(self.window);
   temp = bsxfun(@minus,temp,event(:));
   window = [min(temp(:,1)) max(temp(:,2))];
   window = self.checkWindow(window,size(window,1));
else
   window = self.checkWindow(p.Results.window,size(p.Results.window,1));
end

% Window at original sample times, then shift
origWindow = window;
nObj = numel(self);
window = repmat(window,nObj,1);
window = bsxfun(@plus,window,event(:));
window = mat2cell(window,ones(nObj,1),2);

self.setWindow(window);
self.setOffset(-event);

[times,values] = arrayfun(@(x) deal(x.times{1},x.values{1}),self,'uni',false);
if isequal(times{:})
   if nargout
      s.times = times{1};
      s.values = cell2mat(values);
   end
elseif p.Results.commonTime && isequal(self.Fs) % interpolate
   %disp('interpolating');
   % Common sampling grid
   n = max(cellfun('prodofsize',times));
   dt = 1/self(1).Fs;
   while (n*dt) <= (origWindow(2) - origWindow(1))
      n = n + 1;
   end
   t = SampledProcess.tvec(origWindow(1),dt,n);
      
   for i = 1:numel(values)
      temp(:,i) = interp1(times{i},values{i},t);
      % Replace times & values in SampledProcess
      self(i).times = {t};
      self(i).values = {temp(:,i)};
   end

   if nargout
      s.times = t;
      s.values = temp;
   end
else
   % Different sampling frequencies
   if nargout
      s.times = times;
      s.values = values;
   end
   % TODO allow resampling to common Fs?
end

% [y, Fout]=SincResample([x],2*length(x),1,0,'lanczos');
