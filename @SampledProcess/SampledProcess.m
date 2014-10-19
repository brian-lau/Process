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
            
      self = setInclusiveWindow(self)
      self = reset(self)
      obj = chop(self,shiftToWindow)
      [values,times] = sync(self,event,varargin)
      self = resample(self,newFs)
      self = filter(self,b,a,fix)

      [self,b] = highpass(self,corner,order,fix)
      function [self,b] = lowpass(self,corner,order)
      end
      
      function [self,b] = bandpass(self,corner,order)
      %b = firls(MUA_FILT_ORDER,[0 450/nyquist 500/nyquist 2500/nyquist 2550/nyquist 1],[0 0 1 1 0 0]);
      end

      function self = interp(self)
      end

      function self = smooth(self)
      end
      
      function self = interpFreq(self,freqs,freqrange,chunksize)
         for i = 1:numel(self)
            for j = 1:size(self(i).window,1)
               for k = 1:size(self.values{j},2)
                  self(i).values{j}(:,k) = chunkwiseDeline(self(i).values{j}(:,k),...
                     self(i).Fs,freqs,freqrange,chunksize);
               end
            end
         end
      end
      
      self = detrend(self)

      function windowFun(self,fun) % apply applyFunc func?
      end
      
      plot(self)
      out = extract(self,labels)
   end
   
   methods(Access = protected)
      applyWindow(self)
      applyOffset(self,undo)
      discardBeforeStart(self)
      discardAfterEnd(self)
   end
   
   methods(Static)
      obj = loadobj(S)
      t = tvec(t0,dt,n)
      pre = extendPre(tStartOld,tStartNew,dt)
      post = extendPost(tEndOld,tEndNew,dt)
   end
end % classdef
