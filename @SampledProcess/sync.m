% should allow resampling
% should handle different Fs for object array
% multiple events? EventProcess?
function self = sync(self,event,varargin)

p = inputParser;
p.KeepUnmatched = true;
p.FunctionName = 'SampledProcess sync';
p.addRequired('event',@(x) isnumeric(x) || isa(x,'metadata.Event'));
p.addParameter('window',[],@(x) isnumeric(x) && (size(x,1)==1) && (size(x,2)==2)); 
p.addParameter('eventStart',true,@(x) isscalar(x) && islogical(x)); 
p.addParameter('commonTime',true,@(x) islogical(x));
p.addParameter('interpMethod','',@(x) ischar(x));
p.parse(event,varargin{:});
par = p.Results;

if ~self.short_
   disp('notification sent');
   self.short_ = true;
   notify(self,'isSyncing',SyncEventData(par));
   return;
else
   self.short_ = false;
end
disp('running sync');

nObj = numel(self);
if (numel(event)==1) && (nObj>1)
   event = repmat(event,size(self));
end
assert(numel(event)==numel(self),'SampledProcess:sync:InputValue',...
   'numel(event) should match numel(SampledProcess)');

if all(isa(event,'metadata.Event'))
   if par.eventStart
      offset = [event.tStart]';
   else
      offset = [event.tEnd]';
   end
else
   offset = event(:);
end

if isempty(par.window) % FIXME: not working?
   % find window that includes all data
   temp = vertcat(self.window);
   temp = bsxfun(@minus,temp,offset);
   window = [min(temp(:,1)) max(temp(:,2))];
   window = checkWindow(window,size(window,1));
   clear temp;
else
   window = par.window;
end

origWindow = window;
% Window at original sample times
if (size(window,1)>1) || (numel(offset)>1)
   window = bsxfun(@plus,window,offset);
   window = bsxfun(@plus,window,-vec([self.cumulOffset]));
   window = num2cell(window,2);
else
   window = window + offset - self.cumulOffset;
end

self.setWindow(window);

if par.commonTime
   if isempty(par.interpMethod)
      dt = 1/self(1).Fs; % FIXME, Fs uniformity check
      t = SampledProcess.tvec(origWindow(1),dt,diff(origWindow)/dt);
      
      % Shift by event, rounded to nearest sample time
      if numel(offset) == 1
         offset = nearest(offset,t+offset);
      else
         offset = nearest(offset,bsxfun(@plus,t,vec(offset)'));
      end
      self.setOffset(-offset);
   else
      self.setOffset(-offset);
      % Extract
      [times,values] = arrayfun(@(x) deal(x.times{1},x.values{1}),self,'uni',false);
      % Interpolate to common sampling grid defined by window
      n = max(cellfun('prodofsize',times));
      dt = 1/self(1).Fs;
      while (n*dt) <= (origWindow(2) - origWindow(1))
         n = n + 1;
      end
      t = SampledProcess.tvec(origWindow(1),dt,n);
      
      for i = 1:numel(values)
         temp = interp1(times{i},values{i},t,par.interpMethod);
         % Replace times & values in SampledProcess
         self(i).times = {t};
         self(i).values = {temp};
      end
   end
else
   self.setOffset(-offset);
end
