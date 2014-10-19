%      function self = sync(self,event,varargin)
% should allow resampling
function [values,times] = sync(self,event,varargin)
p = inputParser;
p.KeepUnmatched= false;
p.FunctionName = 'SampledProcess sync';
p.addRequired('event',@(x) isnumeric(x));
p.addParamValue('window',[]);
%p.addParamValue('resample',[]);
p.parse(event,varargin{:});

self.setInclusiveWindow;

if isempty(p.Results.window)
   temp = vertcat(self.window);
   temp = bsxfun(@minus,temp,event(:));
   window = [min(temp(:,1)) max(temp(:,2))];
   window = self.checkWindow(window,size(window,1));
else
   window = self.checkWindow(p.Results.window,size(p.Results.window,1));
end

nObj = numel(self);
if size(window,1) == 1
   window = repmat(window,nObj,1);
   window = bsxfun(@plus,window,event(:));
   %window = mat2cell(window,ones(nObj,1),2);
   
   self.setWindow(window);
   self.setOffset(-event);
   
   fixedWindow = true;
else
   error('not done')
end

if nargout
   [times,values] = arrayfun(@(x) deal(x.times{1},x.values{1}),self,'uni',false);
   if fixedWindow
      times = times{1};
      values = cell2mat(values);
   end
end
