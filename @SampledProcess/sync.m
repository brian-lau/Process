% should allow resampling
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

origWindow = window;
nObj = numel(self);
window = repmat(window,nObj,1);
window = bsxfun(@plus,window,event(:));
window = mat2cell(window,ones(nObj,1),2);

self.setWindow(window);
self.setOffset(-event);

%fixedWindow = true; % THIS MAY NEVER BE FALSE?

if nargout
   [times,values] = arrayfun(@(x) deal(x.times{1},x.values{1}),self,'uni',false);
   
   if isequal(times{:})
      s.times = times{1};
      s.values = cell2mat(values);
   elseif p.Results.commonTime && isequal(self.Fs)
      % we need to interpolate
      n = max(cellfun('prodofsize',times));
      dt = 1/self(1).Fs;
      while (n*dt) <= (origWindow(2) - origWindow(1))
         n = n + 1;
      end
   	t = SampledProcess.tvec(origWindow(1),dt,n);

      for i = 1:numel(values)
         temp(:,i) = interp1(times{i},values{i},t);
      end
      
      s.times = t;
      s.values = temp;
   else
      % return with potentially different sampling
   end
end

%  t = SampledProcess.tvec(-2,1,4)
% Vq = interp1(times{1},values{1},t)