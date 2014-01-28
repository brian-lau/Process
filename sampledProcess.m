% Regularly sampled processes
% If multiple processes, currently cannot be multidimensional,
% time = rows

classdef(CaseInsensitiveProperties = true) sampledProcess < process   
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
      function self = sampledProcess(varargin)
         self = self@process;
         if nargin == 0
           return;
         end
         
         if nargin == 1
            values = varargin{1};
            assert(isnumeric(values),...
               'sampledProcess:Constructor:InputFormat',...
               'Single inputs must be passed in as array of values');
            if isnumeric(values)
               varargin{1} = 'values';
               varargin{2} = values;
            end
         end

         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'sampledProcess constructor';
         p.addParamValue('info',containers.Map('KeyType','char','ValueType','any'));
         p.addParamValue('Fs',1);
         p.addParamValue('values',[],@ismatrix );
         p.addParamValue('window',[],@isnumeric);
         p.addParamValue('offset',[],@isnumeric);
         p.addParamValue('tStart',0);
         p.addParamValue('tEnd',[],@isnumeric);
         p.parse(varargin{:});
         
         self.info = p.Results.info;
         
         self.tStart = p.Results.tStart;
         
         if isvector(p.Results.values)
            self.values_ = p.Results.values(:);
         else
            self.values_ = p.Results.values;
         end
         self.Fs_ = p.Results.Fs;
         self.Fs = self.Fs_;
         dt = 1/self.Fs_;
         self.times_ = self.tStart + (0:dt:(dt*(size(self.values_,1)-1)))';
         if isempty(p.Results.tEnd)
            self.tEnd = self.times_(end);
         else
            self.tEnd = p.Results.tEnd;
         end
         % Set the window
         if isempty(p.Results.window)
            self.window = [min(self.times_) max(self.times_)];
         else
            self.window = self.checkWindow(p.Results.window,size(p.Results.window,1));
         end
         
         % Set the offset
         if isempty(p.Results.offset)
            self.offset = 0;
         else
            self.offset = self.checkOffset(p.Results.offset,size(p.Results.offset,1));
         end         

         % Store original window and offset for resetting
         self.window_ = self.window;
         self.offset_ = self.offset;
      end% constructor
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      function set.Fs(self,Fs)
         if isnan(Fs)
            self.Fs = 1;
         elseif isscalar(Fs) && isnumeric(Fs)
            self.Fs = Fs;
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
         
         % FIXME, what happens for sampledProcess??? if we resample???
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
            self(i).offset = self(i).offset_;
            self(i).Fs = self(i).Fs_;
         end
      end
      
      function chop(self,shiftToWindow)
      end
      
      function windowFun(self,fun) % apply applyFunc func?
      end
      
      % detrend
      % resample
      % 
      
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
         nWindow = size(self.window,1);
         %times = self.times_;
         if isempty(self.times_)
            return
         end
         values = self.values_;
         dt = 1/self.Fs_;
         window = self.window;

         % NaN-pad when window extends beyond process. This extension is
         % done to the nearest sample that fits in the window.
         minWin = min(window(:,1));
         maxWin = max(window(:,2));
         if minWin < self.tStart
            pre = flipud([(self.tStart-dt):-dt:minWin]');
            preV = nan(size(pre,1),size(self.values_,2));
         else
            pre = [];
            preV = [];
         end
         if maxWin > self.tEnd
            post = [(self.tEnd+dt):dt:maxWin]';
            postV = nan(size(post,1),size(self.values_,2));
         else
            post = [];
            postV = [];
         end
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
   end
end
