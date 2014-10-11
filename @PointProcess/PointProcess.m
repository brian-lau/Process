classdef(CaseInsensitiveProperties = true) PointProcess < Process         
   properties(AbortSet)
      tStart % Start time of process
      tEnd   % End time of process
   end
   % These dependent properties all apply the window property
   properties(SetAccess = private, Dependent = true, Transient = true)
      % # of events within window
      count
   end
   
   methods
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %% Constructor
      function self = PointProcess(varargin)
         % Constructor, arguments are taken as name/value pairs
         % info     - Information about point process
         %            containers.Map
         %            cell array, converted to map with generic keys
         % times    - Vector of event times
         % values   - Data corresponding to each event time
         % window   - Defaults to window that includes all event times,
         %            If a smaller window is passed in, event times outside
         %            the window will be DISCARDED.
         
         self = self@Process;
         if nargin == 0
            return;
         end

         if nargin == 1
            times = varargin{1};
            assert(isnumeric(times) || iscell(times),...
               'PointProcess:Constructor:InputFormat',...
                  ['Single inputs must be passed in as array of event times'...
               ', or cell array of arrays of event times.']);
            if isnumeric(times)
               varargin{1} = 'times';
               varargin{2} = times;
            else
               assert(all(cellfun(@isnumeric,times)),...
                  'PointProcess:Constructor:InputFormat',...
                  'Each element of cell array must be a numeric array.');
               varargin{1} = 'times';
               varargin{2} = times;
            end
         end
         
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'PointProcess constructor';
         p.addParamValue('info',containers.Map('KeyType','char','ValueType','any'));
         p.addParamValue('times',{},@(x) isnumeric(x) || iscell(x));
         p.addParamValue('values',{},@(x) isvector(x) || iscell(x) );
         p.addParamValue('labels',{});
         p.addParamValue('quality',[]);
         p.addParamValue('window',[],@isnumeric);
         p.addParamValue('offset',0,@isnumeric);
         p.addParamValue('tStart',[],@isnumeric);
         p.addParamValue('tEnd',[],@isnumeric);
         p.parse(varargin{:});
         
         self.info = p.Results.info;
         
         % Create the values cell array
         % FIXME not sorting values yet
         if ~isempty(p.Results.times)
            if isnumeric(p.Results.times)
               [a,b] = unique(p.Results.times(:));
               eventTimes{1} = a;
               tInd{1} = b;
            elseif iscell(p.Results.times)
               [eventTimes,tInd] = cellfun(@(x) unique(x(:)),p.Results.times,'uni',0);
            end
            
            if isempty(p.Results.values)
               values = cellfun(@(x) ones(size(x)),eventTimes,'uni',0);
            else
               if ~iscell(p.Results.values)
                  values = {p.Results.values};
               else
                  values = p.Results.values;
               end
               assert(numel(eventTimes)==numel(values),...
                  'PointProcess:constuctor:InputSize',...
                  '# of ''times'' must equal # of ''values''');
               values = reshape(values,size(eventTimes));
               assert(all(cellfun(@(x,y) numel(x)==numel(y),...
                  values,eventTimes)),'PointProcess:constuctor:InputSize',...
                  '# of ''times'' must equal # of ''values''');
               values = cellfun(@(x) x(:),values,'uni',0);
            end
         else
            if ~isempty(p.Results.values)
               warning('PointProcess:Constructor:InputCount',...
                  'Values ignored without event times');
            end
            return;
         end
         
         % If we have event times
         self.times_ = eventTimes;
         self.values_ = values;
                  
         % Define the start and end times of the process
         if isempty(p.Results.tStart)
            self.tStart = min([cellfun(@min,eventTimes) 0]);
         else
            self.tStart = p.Results.tStart;
         end
         if isempty(p.Results.tEnd)
            self.tEnd = max(cellfun(@max,eventTimes));
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
      end % constructor
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      function set.tStart(self,tStart)
         if isscalar(tStart) && isnumeric(tStart)
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
         if isscalar(tEnd) && isnumeric(tEnd)
            self.tEnd = tEnd;
         else
            error('bad end');
         end
         self.discardAfterEnd();
         if ~isempty(self.tStart)
            self.setInclusiveWindow();
         end
      end
      
      function self = setInclusiveWindow(self)
         % Set windows to earliest and latest event times
         %
         % SEE ALSO
         % window, setWindow, applyWindow
         for i = 1:numel(self)
            tempMin = cellfun(@min,self(i).times_,'uni',0);
            tempMin = min(vertcat(tempMin{:}));
            tempMax = cellfun(@max,self(i).times_,'uni',0);
            tempMax = max(vertcat(tempMax{:}));
            self(i).window = [tempMin tempMax];
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
         end
      end
      
      function count = get.count(self)
         % # of event times within windows
         if isempty(self.times)
            count = 0;
         else
            count = cellfun(@(x) numel(x),self.times);
         end
      end
      
      %%
      output = windowFun(self,fun,nOpt,varargin)
            
      self = insert(self,times,values,labels)
      
      self = remove(self,times,labels)
      
      function output = valueFun(self,fun,varargin)
         % Apply a function to windowValues
         
         % need to deal with arbitrary arguments to fun
         % need to deal with arbitrary outputs from fun
         % numel(self) > 1, see windowFun
         
         % trap parameters
         % Specific to cellfun 'UniformOutput' & ErrorHandler
         % Non-specific
         % args
         % recurse
         p = inputParser;
         p.KeepUnmatched= true;
         p.FunctionName = 'PointProcess valueFun method';
         % Intercept some parameters to override defaults
         p.addParamValue('nOutput',1,@islogical);
         p.addParamValue('args',{},@iscell);
         p.parse(varargin{:});
         % Passed through to cellfun
         params = p.Unmatched;

         nWindow = size(self.window,1);
         nArgs = numel(p.Results.args);
%          if nArgs ~= nargin(fun)
%             error('');
%          end

         for i = 1:nWindow
            % Construct function arguments (see cellfun) as a cell array to
            % use comman-separated expansion
            temp = p.Results.args;
            args = cell(1,nArgs);
            for j = 1:numel(temp)
               args{j} = repmat(temp(j),1,self.count(i));
            end
            output{i,1} = cellfun(fun,self.windowedValues{i},args{:});
         end
      end
      
      % valueIsEqual
      % valueIsString
      
      function [bool,times] = hasValue(self,value) % valueIsEqual
         % TODO handle PointProcess array
         % return array in case of one window?
%          nWindow = size(self.window,1);
%          for i = 1:nWindow
%             bool(i,1) = valueFun(self,@(x) x==value);
%             times{i,1} = self.windowedTimes{i}(bool{i,1});
%          end
         bool = valueFun(self,@(x) x==value);
         
         nWindow = size(self.window,1);
         for i = 1:nWindow
            times{i,1} = self.times{i}(bool{i,1});
         end
      end
      
      obj = chop(self,shiftToWindow)
      
      [values,times] = sync(self,event,varargin)
      
%       %% Intervals?
%       function iei = intervals(self)
%          % Interevent interval representation
%          iei = cellfun(@diff,self.times,'UniformOutput',false);
%       end

%       function cp = countingProcess(self)
%          % Counting process representation
%          if any(isnan(self.window))
%             cp = [NaN NaN];
%          else
%             %window = self.window;
%             %times = getTimes(self,window);
%             times = times{1};
%             count = cumsum(ones(size(times)));
%             tStart = max(-inf,unique(min(times)));
%             cp = [[tStart;times] , [0;count]];
%          end
%       end
      
      %% Display
      function [h,yOffset] = plot(self,varargin)
         [h,yOffset] = raster(self,varargin{:});
      end
      
      function [h,yOffset] = raster(self,varargin)
         % Raster plot
         %
         % For a full description of the possible parameters, 
         %
         % SEE ALSO
         % plotRaster

         p = inputParser;
         p.KeepUnmatched= true;
         p.FunctionName = 'PointProcess raster method';
         % Intercept some parameters to override defaults
         p.addParamValue('grpBorder',false,@islogical);
         p.addParamValue('labelXAxis',false,@islogical);
         p.addParamValue('labelYAxis',false,@islogical);
         p.parse(varargin{:});
         % Passed through to plotRaster
         params = p.Unmatched;
         
         n = numel(self);
         if n == 1
            times = self.times;
         else
            times = [self.times];
            %window = self.checkWindow(cat(1,self.window),n);
         end
         
         if isempty(times)
            % need to return handle and yOffset if they exist? TODO
            if isfield(params,'h')
               h = params.h;
            end
            if isfield(params,'yOffset')
               yOffset = params.yOffset;
            end
         else
            [h,yOffset] = plotRaster(times,p.Results,params);
            xlabel('Time');
            %xlabel(['Time (' self.unit ')']);
         end         
      end
                  
      %% Operators
      plus(x,y)

      minus(x,y)
   
      bool = eq(x,y)
   end % methods (Public)
   
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
         times = self.times_;
         if isempty(times)
            return
         end
         nTimes = size(times,2);
         values = self.values_;
         window = self.window;
         windowedTimes = cell(nWindow,nTimes);
         windowedValues = cell(nWindow,nTimes);
         windowIndex = cell(nWindow,nTimes);
         isValidWindow = false(nWindow,1);
         for i = 1:nWindow
            for j = 1:nTimes
               ind = (times{j}>=window(i,1)) & (times{j}<=window(i,2));
               windowedTimes{i,j} = times{j}(ind);
               windowedValues{i,j} = values{j}(ind);
               windowIndex{i,j} = find(ind);
               if (window(i,1)>=self.tStart) && (window(i,2)<=self.tEnd)
                  isValidWindow(i) = true;
               else
                  isValidWindow(i) = false;
               end
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
         nTimes = size(self.times,2);
         for i = 1:numel(offset)
            for j = 1:nTimes
               self.times{i,j} = self.times{i,j} + offset(i);
            end
         end
      end
      
      function discardBeforeStart(self)
         ind = cellfun(@(x) x<self.tStart,self.times_,'uni',0);
         if any(cellfun(@any,ind))
            for i = 1:numel(self.times_)
               self.times_{i}(ind{i}) = [];
               self.values_{i}(ind{i}) = [];
            end
         end
      end
      
      function discardAfterEnd(self)
         ind = cellfun(@(x) (x>self.tEnd),self.times_,'uni',0);
         if any(cellfun(@any,ind))
            for i = 1:numel(self.times_)
               self.times_{i}(ind{i}) = [];
               self.values_{i}(ind{i}) = [];
            end
         end
      end
   end % methods(Protected)
   
%    methods(Static)
%       function sync(p,)
%          
%       end
%    end
end % classdef

