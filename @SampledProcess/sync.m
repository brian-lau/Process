% should allow resampling
function s = sync(self,event,varargin)
p = inputParser;
p.KeepUnmatched= false;
p.FunctionName = 'SampledProcess sync';
p.addRequired('event',@(x) isnumeric(x));
p.addOptional('window',[],@(x) isnumeric(x) && (size(x,1)==1) && (size(x,2)==2)); 
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

nObj = numel(self);
window = repmat(window,nObj,1);
window = bsxfun(@plus,window,event(:));
window = mat2cell(window,ones(nObj,1),2);

self.setWindow(window);
self.setOffset(-event);

fixedWindow = true; % THIS MAY NEVER BE FALSE?

if nargout
   [times,values] = arrayfun(@(x) deal(x.times{1},x.values{1}),self,'uni',false);
   if fixedWindow
      s.times = times{1};
      s.values = cell2mat(values);
   end
end
