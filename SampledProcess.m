% Regularly sampled processes
% If multiple processes, currently cannot be multidimensional,
% time = rows

classdef(CaseInsensitiveProperties = true) SampledProcess < Process   
   properties(AbortSet)%(AbortSet, Access=?Segment)
      tStart % Start time of process
      tEnd   % End time of process
   end
   properties(SetAccess = private)
      Fs % Sampling frequency
   end
   properties(SetAccess = private, Dependent = true, Transient = true)
      dim
      dt
   end   
   properties(SetAccess = protected, Hidden = true)
      Fs_ % Original sampling frequency
   end
   
   methods
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %% Constructor
      function self = SampledProcess(varargin)
         self = self@Process;
         if nargin == 0
           return;
         end
         
         if nargin == 1
            values = varargin{1};
            assert(isnumeric(values),...
               'SampledProcess:Constructor:InputFormat',...
               'Single inputs must be passed in as array of values');
            if isnumeric(values)
               varargin{1} = 'values';
               varargin{2} = values;
            end
         end

         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'SampledProcess constructor';
         p.addParamValue('info',containers.Map('KeyType','char','ValueType','any'));
         p.addParamValue('Fs',1);
         p.addParamValue('values',[],@ismatrix );
         p.addParamValue('labels',{});
         p.addParamValue('quality',[]);
         p.addParamValue('window',[],@isnumeric);
         p.addParamValue('offset',[],@isnumeric);
         p.addParamValue('tStart',0);
         p.addParamValue('tEnd',[],@isnumeric);
         p.parse(varargin{:});
         
         self.info = p.Results.info;
         
         % Create values array
         if isvector(p.Results.values)
            self.values_ = p.Results.values(:);
         else
            % Assume leading dimension is time
            % FIXME, should probably force to 2D? Actually, maybe not,
            % allow user to preserve dimensions after leading
            self.values_ = p.Results.values;
         end
         self.Fs_ = p.Results.Fs;
         self.Fs = self.Fs_;
         dt = 1/self.Fs_;
         self.times_ = self.tvec(p.Results.tStart,dt,(size(self.values_,1)));
         
         % Define the start and end times of the process
         self.tStart = p.Results.tStart;
         if isempty(p.Results.tEnd)
            self.tEnd = self.times_(end);
         else
            self.tEnd = p.Results.tEnd;
         end
         
         % Set the window
         if isempty(p.Results.window)
            self.setInclusiveWindow();
         else
            self.window = self.checkWindow(p.Results.window,size(p.Results.window,1));
         end
         
         % Set the offset
         if isempty(p.Results.offset)
            self.offset = 0;
         else
            self.offset = self.checkOffset(p.Results.offset,size(p.Results.offset,1));
         end         

         % Create labels
         self.labels = p.Results.labels;
         
         self.quality = p.Results.quality;

         % Store original window and offset for resetting
         self.window_ = self.window;
         self.offset_ = self.offset;
      end% constructor
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      function set.tStart(self,tStart)
         if isscalar(tStart) && isnumeric(tStart)
            pre = self.extendPre(self.tStart,tStart,1/self.Fs_);
            preV = nan(size(pre,1),size(self.values_,2));
            self.times_ = [pre ; self.times_];
            self.values_ = [preV ; self.values_];
            self.tStart = tStart;
         else
            error('bad start');
         end
         self.discardBeforeStart();
         if ~isempty(self.tEnd)
            self.setInclusiveWindow();
         end
      end
      
      function set.tEnd(self,tEnd)
         % TODO, validate against tStart
         % what is the point of tStart and tEnd??? should they be public?
         if isscalar(tEnd) && isnumeric(tEnd)
            post = self.extendPost(self.tEnd,tEnd,1/self.Fs_);
            postV = nan(size(post,1),size(self.values_,2));
            self.times_ = [self.times_ ; post];
            self.values_ = [self.values_ ; postV];
            self.tEnd = tEnd;
         else
            error('bad end');
         end
         self.discardAfterEnd();
         if ~isempty(self.tStart)
            self.setInclusiveWindow();
         end
      end
      
      function dt = get.dt(self)
         dt = 1/self.Fs;
      end
      
      function dim = get.dim(self)
         dim = cellfun(@(x) size(x),self.values,'uni',false);
      end
            
      function self = setInclusiveWindow(self)
         % Set windows to earliest and latest event times
         %
         % SEE ALSO
         % window, setWindow, applyWindow
         for i = 1:numel(self)
            self(i).window = [min(self(i).times_) max(self(i).times_)];
         end
      end
      
      function self = reset(self)
         % Reset windows & offsets to state when object was created if it
         % has not been chopped, otherwise to when it was chopped.
         %
         % SEE ALSO
         % setInclusiveWindow
         for i = 1:numel(self)
            self(i).window = self(i).window_;
            % Directly apply windod in case window_ = window
            self.offset = 'windowIsReset';
            applyWindow(self);
            self(i).offset = self(i).offset_;
            self(i).Fs = self(i).Fs_;
         end
      end
      
      function obj = chop(self,shiftToWindow)
         % TODO
         % can we rechop?
         %     yes, not sure its useful, but i guess it should work.
         %     eg., chop first by trials, then chop relative to an event
         %     within each trial?
         %
         % need to handle case where there is an offset?, or perhaps there
         % should be a convention?
         if nargin == 1
            shiftToWindow = true;
         end
         
         if numel(self) > 1
            error('SampledProcess:chop:InputCount',...
               'You can only chop a scalar SampledProcess.');
         end
         
         nWindow = size(self.window,1);
         % FIXME, http://www.mathworks.com/support/bugreports/893538
         % May need looped allocation if there is a circular reference.
         obj(nWindow) = SampledProcess();
         oldOffset = self.offset;
         self.offset = 0;
         for i = 1:nWindow            
            obj(i).info = copyInfo(self);
            
            if shiftToWindow
               shift = self.window(i,1);
            else
               shift = 0;
            end
            
            obj(i).times_ = self.times{i} - shift;
            obj(i).values_ = self.values{i};
            obj(i).Fs_ = self.Fs;
            obj(i).Fs = self.Fs;
            % FIXME, do we need current Fs instead of Fs_? 
            % probably, since Fs may not equal Fs_

            obj(i).tStart = self.window(i,1) - shift;
            obj(i).tEnd = self.window(i,2) - shift;
            obj(i).window = self.window(i,:) - shift;
            obj(i).offset = oldOffset(i);
            
            obj(i).labels = self.labels;
            obj(i).quality = self.quality;
            
            % Need to set offset_ and window_
            obj(i).window_ = obj(i).window;
            obj(i).offset_ = self.offset_ + self.window(i,1);
         end
         
         if nargout == 0
            % Currently Matlab OOP doesn't allow the handle to be
            % reassigned, ie self = obj, so we do a silent pass-by-value
            % http://www.mathworks.com/matlabcentral/newsreader/view_thread/268574
            assignin('caller',inputname(1),obj);
         end
      end % chop

%      function self = sync(self,event,varargin)
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
      end
      
      function self = resample(self,newFs)
         % use lcm?
         % http://www.mathworks.com/matlabcentral/fileexchange/45329-sample-rate-conversion/content/SRC/srconv.m
         
         % resample data in window
         if self.Fs == newFs
            return;
         end
         [p,q] = rat(newFs/self.Fs);
         %values = cellfun(@(x) resample(x,p,q),self.values,'uni',0);
         [values{1},b] = resample(self.values{1},p,q);
         nWindow = size(self.window,1);
         if  nWindow > 1
            values(2:nWindow,1) = cellfun(@(x) resample(x,p,q,b),...
               self.values(2:nWindow,1),'uni',0);
         end
         
         times = cellfun(@(x,y) self.tvec(x(1),1/newFs,size(y,1)),...
            self.times,values,'uni',0);
         
         self.times = times;
         self.values = values;
         self.Fs = newFs;
      end
      
      function filter(self,b,a,fix)
         if nargin < 4
            fix = false;
         end
         if nargin < 3
            a = 1;
         end
         for i = 1:numel(self)
            for j = 1:size(self.window,1)
               if fix
                  self(i).values_ = filtfilt(b,a,self(i).values_);
                  oldOffset = self(i).offset;
                  %self.offset = 'windowIsReset';
                  applyWindow(self(i));
                  self(i).offset = oldOffset;
               else
                  self(i).values{j} = filtfilt(b,a,self(i).values{j});
               end
            end
         end
      end
      
      function [self,b] = highpass(self,corner,order,fix)
         if nargin < 4
            fix = false;
         end
         if nargin < 3
            order = 100;
         end
         assert(corner > (corner-1),'Corner frequency too low');
         Fs = unique([self.Fs]);
         assert(numel(Fs)==1,'Must have same Fs');
         nyquist = self.Fs/2;
         
         b = firls(order,[0 (corner-1)/nyquist corner/nyquist 1],[0 0 1 1]);
         %keyboard
         %freqz(b,1,[],'whole',Fs);
         self.filter(b,1,fix);
      end
      
      function [self,b] = lowpass(self,corner,order)
         
      end
      
      function [self,b] = bandpass(self,corner,order)
      %b = firls(MUA_FILT_ORDER,[0 450/nyquist 500/nyquist 2500/nyquist 2550/nyquist 1],[0 0 1 1 0 0]);
      end

      function self = interp(self)
      end

      function self = smooth(self)
         
      end
      
      function windowFun(self,fun) % apply applyFunc func?
      end
      
      % detrend
      % 
      function plot(self)
         figure; hold on
         for i = 1:numel(self)
            subplot(numel(self),1,i);
            plot(self(i).times{1},self(i).values{1});
            %strips(self.times{i},self.values{i});
         end
      end
      
      function out = extract(self,labels)
         for i = 1:numel(self)
            ind = ismember(self(i).labels,labels);
            if any(ind)
               if size(self(i).window,1) == 1
                  out{i} = self(i).values{1}(:,ind);
               else
                  out{i} = cellfun(@(x) x(:,ind),self(i).values,'uni',0);
               end
            end
         end
      end

   end
   
   methods(Access = protected)
      function applyWindow(self)
         % Window original event times, setting
         %     times
         %     values
         %     index
         %     isValidWindow
         % TODO
         % Windows are inclusive on both sides, does this make sense???
         if isempty(self.times_)
            return;
         end

         % NaN-pad when window extends beyond process. This extension is
         % done to the nearest sample that fits in the window.
         nWindow = size(self.window,1);
         window = self.window;
         minWin = min(window(:,1));
         maxWin = max(window(:,2));
         
         pre = self.extendPre(self.tStart,minWin,1/self.Fs_);
         preV = nan(size(pre,1),size(self.values_,2));
         post = self.extendPost(self.tEnd,maxWin,1/self.Fs_);
         postV = nan(size(post,1),size(self.values_,2));
         times = [pre ; self.times_ ; post];
         values = [preV ; self.values_ ; postV];
                  
         windowedTimes = cell(nWindow,1);
         windowedValues = cell(nWindow,1);
         windowIndex = cell(nWindow,1);
         isValidWindow = false(nWindow,1);
         for i = 1:nWindow
            ind = (times>=window(i,1)) & (times<=window(i,2));
            windowedTimes{i,1} = times(ind);
            windowedValues{i,1} = values(ind,:); % FIXME, only works for 2D
            windowIndex{i,1} = find(ind);
            if (window(i,1)>=self.tStart) && (window(i,2)<=self.tEnd)
               isValidWindow(i) = true;
            else
               isValidWindow(i) = false;
            end
         end
         self.times = windowedTimes;
         self.values = windowedValues;
         self.index = windowIndex;
         self.isValidWindow = isValidWindow;
      end
      
      function applyOffset(self,undo)
         % Offset times, 
         if nargin == 1
            undo = false;
         end
         if undo
            offset = -self.offset;
         else
            offset = self.offset;
         end
         for i = 1:numel(offset)
            self.times{i,1} = self.times{i,1} + offset(i);
         end
      end
            
      function discardBeforeStart(self)
         ind = self.times_ < self.tStart;
         if any(ind)
            self.times_(ind) = [];
            self.values_(ind,:) = [];
         end
      end
      
      function discardAfterEnd(self)
         ind = self.times_ > self.tEnd;
         if any(ind)
            self.times_(ind) = [];
            self.values_(ind,:) = [];
         end
      end
   end
   
   methods(Static)
      function obj = loadobj(S)
         obj = SampledProcess('tStart',...
            S.tStart,...
            'Fs',S.Fs,...
            'values',S.values_,...
            'offset',S.offset_,...
            'window',S.window_,...
            'labels',S.labels,...
            'info',S.info,...
            'quality',S.quality);
         obj.window = S.window;
         obj.offset = S.offset;
      end
      
      function t = tvec(t0,dt,n)
         t = t0 + (0:dt:(dt*(n-1)))';
      end
      
      function pre = extendPre(tStartOld,tStartNew,dt)
         if tStartNew < tStartOld
            pre = flipud(((tStartOld-dt):-dt:tStartNew)');
         else
            pre = [];
         end
      end
      
      function post = extendPost(tEndOld,tEndNew,dt)
         if tEndNew > tEndOld
            post = ((tEndOld+dt):dt:tEndNew)';
         else
            post = [];
         end
      end
   end
end % classdef
