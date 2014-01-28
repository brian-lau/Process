classdef(CaseInsensitiveProperties = true) pointProcess < process         
   % These dependent properties all apply the window property
   properties(SetAccess = private, Dependent = true, Transient = true)
      % # of events within window
      count
   end
   
   methods
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %% Constructor
      function self = pointProcess(varargin)
         % Constructor, arguments are taken as name/value pairs
         % info     - Information about point process
         %            containers.Map
         %            cell array, converted to map with generic keys
         % times    - Vector of event times
         % values   - Data corresponding to each event time
         % window   - Defaults to window that includes all event times,
         %            If a smaller window is passed in, event times outside
         %            the window will be DISCARDED.
         
         self = self@process;
         if nargin == 0
            return;
         end

         if nargin == 1
            times = varargin{1};
            assert(isnumeric(times) || iscell(times),...
               'pointProcess:Constructor:InputFormat',...
                  ['Single inputs must be passed in as array of event times'...
               ', or cell array of arrays of event times.']);
            if isnumeric(times)
               varargin{1} = 'times';
               varargin{2} = times;
            else
               assert(all(cellfun(@isnumeric,times)),...
                  'pointProcess:Constructor:InputFormat',...
                  'Each element of cell array must be a numeric array.');
               varargin{1} = 'times';
               varargin{2} = times;
            end
         end
         
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'pointProcess constructor';
         p.addParamValue('info',containers.Map('KeyType','char','ValueType','any'));
         p.addParamValue('times',{},@(x) isnumeric(x) || iscell(x));
         p.addParamValue('values',{},@(x) isvector(x) || iscell(x) );
         p.addParamValue('labels',{});
         p.addParamValue('window',[],@isnumeric);
         p.addParamValue('offset',[],@isnumeric);
         p.addParamValue('tStart',[],@isnumeric);
         p.addParamValue('tEnd',[],@isnumeric);
         p.parse(varargin{:});
         
         self.info = p.Results.info;
         
         % Create the values cell array
         if ~isempty(p.Results.times) %&& ~any(isnan(p.Results.times))
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
                  'pointProcess:constuctor:InputSize',...
                  '# of ''times'' must equal # of ''values''');
               values = reshape(values,size(eventTimes));
               assert(all(cellfun(@(x,y) numel(x)==numel(y),...
                  values,eventTimes)),'pointProcess:constuctor:InputSize',...
                  '# of ''times'' must equal # of ''values''');
               values = cellfun(@(x) x(:),values,'uni',0);
            end
         else
            if ~isempty(p.Results.values)
               warning('pointProcess:Constructor:InputCount',...
                  'Values ignored without event times');
            end
            return;
         end
         
         %% If we have event times
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
         
         % Create labels
         if isempty(p.Results.labels)
            for i = 1:numel(eventTimes)
               labels{1,i} = ['id' num2str(i)];
            end
            self.labels = labels;
         else
            %FIXME
         end
         
         % Discard event times & values outside of start and end
         ind = cellfun(@(x) (x>=self.tStart) & (x<=self.tEnd),eventTimes,'uni',0);
         if ~any(cellfun(@any,ind))
            self.times_ = {};
            self.values_ = {};
            return;
         end
         for i = 1:numel(eventTimes)
            self.times_{i} = eventTimes{i}(ind{i});
            self.values_{i} = values{i}(ind{i});
         end

         % Set the window
         if isempty(p.Results.window)
            self.window = [min(cellfun(@min,self.times_)) max(cellfun(@max,self.times_))];
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
      end % constructor
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      function self = setInclusiveWindow(self)
         % Set windows to earliest and latest event times
         %
         % SEE ALSO
         % window, setWindow, applyWindow
         
         % FIXME, what happens for sampledProcess??? if we resample???
         for i = 1:numel(self)
            self(i).window = [min(cellfun(@min,self.times_)) max(cellfun(@max,self.times_))];
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
      function output = windowFun(self,fun,nOpt,varargin)
         % Apply a function to windowedTimes
         % 
         % FUN should expect an array of event times. The output format of 
         % windowFun depends on three factors:
         %   1) The number of outputs requested from FUN (NOPT)
         %   2) The output format of FUN
         %   3) Whether the pointProcess object is an array of objects
         %
         % If one output is requested from FUN (nOpt = 1, the default),
         % then the expectation is that FUN returns scalar outputs that can
         % be concatonated, and windowFun will return and array (see cellfun).
         %   If more than one output is requested from FUN (nOpt > 1), then
         % outputs will be collected in a cell array, with the elements
         % corresponding to FUN outputs. Again, the expectation is that
         % each of the outputs of FUN are scalars that can be concatonated.
         %   If FUN does not return scalars, set 'UniformOutput' false, in 
         % which case, ARRAY is returned as a cell array. For the case of
         % multiple outputs, this will be a cell array of cell arrays.
         %
         % For arrays of pointProcess objects, ARRAY is a cell array where
         % each element is the output of windowFun called on the
         % corresponding pointProcess object. Depending on 'UniformOutput',
         % this can again be an array or a cell array.
         %
         % INPUTS
         % fun      - Function handle
         % nOpt     - # of outputs to return from FUN
         % varargin - Additional arguments, the underlying call is to
         %            cellfun, so varargin should be formatted accordingly
         %
         % EXAMPLE
         % % process with different rates in two different windows
         % spk = pointProcess('times',[rand(100,1) ; 1+rand(100,1)*10],'window',[0 1;1 10]);
         % spk.raster('style','line');
         %
         % % Average inter-event interval in each window
         % spk.windowFun(@(x) mean(diff(x)))
         %
         % % Maximum event time and index in each window
         % spk.windowFun(@(x) max(x))
         %
         % % Return the maximum and it's index (nOpt = 2). Since both
         % % outputs of MAX are scalar, the elements of RESULT are vectors,
         % % one element corresponding to each window of SPK
         % result = spk.windowFun(@(x) max(x),2)
         %
         % % Estimate a PSTH for each window. The outputs of GETPSTH are
         % % not scalar, so result is a nested cell array, the outer cell
         % % array corresponding to the different outputs of FUN, and the
         % % inner cell array corresponding to outputs for each window of spk
         % result = spk.windowFun(@(x) getPsth(x,0.025),2,'UniformOutput',false)
         % figure; hold on
         % plot(result{2}{1},result{1}{1},'r'); plot(result{2}{2},result{1}{2},'b')
         %
         % SEE ALSO
         % cellfun
         
         % TODO perhaps we should do a try/catch to automatically attempt to set
         % uniformoutput false?
         % Collection should coordinate how to handle the outputs of windowFun
         % analysis method, need to handle the following situations
         % 1) function applied to each element, and returned individually, eg. first
         % spike time in each window, or the number of spikes in each window
         % 2) function applied to groupings of elements, eg., psth
         
         % first is easy, we just return big cell arrays full of stuff
         % second is less obvious. Requires arranging windowedTimes across all
         % elements into a format that the function expects. For consistency, we
         % want a general format that can be passed around?
         % how about the one for getPsth and plotRaster
         if nargin < 3
            nOpt = 1;
         end
         
         if numel(self) == 1
            if nOpt == 1
               output = cellfun(fun,self.times,varargin{:});
            else
               [output{1:nOpt}] = cellfun(fun,self.times,varargin{:});
            end
         else
            output = cell(size(self));
            for i = 1:numel(self)
               output{i} = windowFun(self(i),fun,nOpt,varargin{:});
            end
         end
      end
            
      function self = insertTimes(self,times,values,labels)
         % Insert times
         % Note that this adjusts tStart and tEnd to include all times.
         % Note that if there is already an offset, new times are added to
         % original times (w/out offset), and then rewindowed and offset
         %
         % times  - either an array of event times to insert
         %          or a containers.Map object, with keys of type 'double'
         %          defining the event times to add
         % values - values associated with event times to insert
         % labels - strings defining which process to insert to
         %
         % SEE ALSO
         % removeTimes
         
         % TODO
         % perhaps additional flags to overwrite? no, replaceTimes
         if nargin < 3
            error('pointProcess:insertTimes:InputFormat',...
               'There must be values for each inserted time');
         end
         if numel(times) ~= numel(values)
            error('pointProcess:insertTimes:InputFormat',...
               'There must be values for each inserted time');
         end
         for i = 1:numel(self)
            if nargin < 4
               % Insert same times & values from all
               labels = self(i).labels;
            end
            
            indL = find(ismember(self(i).labels,labels));
            if any(indL)
               % Index of redundant times
               indR = cellfun(@(x) ismember(times,x),self(i).times_(indL),'uni',0);
               for j = 1:numel(indL)
                  times2Insert{j} = times;
                  values2Insert{j} = values;
                  if any(indR{j})
                     fprintf('%g/%g redundant event times ignored for %s.\n',...
                        sum(indR{j}),length(indR{j}),self(i).labels{indL(j)});
                     times2Insert{j}(indR{j}) = [];
                     values2Insert{j}(indR{j}) = [];
                  end
                  
                  if any(times2Insert{j})
                     % Check that we can concatenate values
                        % Values must match type of present values for contcatenation
                     if isequal(class(values2Insert{j}),class(self.values_{indL(j)}))
                        % Merge
                        [self(i).times_{indL(j)},I] = ...
                           sort([self(i).times_{indL(j)} ; times2Insert{j}(:)]);
                        temp = [self(i).values_{indL(j)} ; values2Insert{j}(:)];
                        %temp = self(i).values_{indL(j)};
                        %temp((end+1):(end+1+numel(values2Insert{j}))) = ...
                        %   values2Insert{j};
                        self(i).values_{indL(j)} = temp(I);
                        inserted(j) = true;
                     else
                        inserted(j) = false;
                        warning('pointProcess:insertTimes:InputFormat',...
                           ['times not added for ' self(i).labels{indL} ...
                           ' because value type does not match']);
                     end
                  else
                     inserted(j) = false;
                  end
               end
               
               if any(inserted)
                  timesInserted = cell2mat(times2Insert(inserted));
                  % Reset properties that depend on event times
                  if min(timesInserted) < self(i).tStart
                     self(i).tStart = min(timesInserted);
                  end
                  if max(timesInserted) > self(i).tEnd
                     self(i).tEnd = max(timesInserted);
                  end
                  oldOffset = self(i).offset;
                  self(i).offset = 'windowIsReset';
                  applyWindow(self(i));
                  self(i).offset = oldOffset;
               end
            end
         end
      end
      
      function self = removeTimes(self,times,labels)
         % Remove times and associated values
         % Note that this does NOT change tStart or tEnd.
         %
         % times  - array of event times to remove
         % labels - string, cell array of strings
         %          indicating which process to remove times
         %          from.
         %          default = all
         %
         % SEE ALSO
         % insertTimes
         if nargin < 2
            error('pointProcess:removeTimes:InputFormat',...
               'You must provide times to remove.');
         end
         for i = 1:numel(self)
            if nargin < 3
               % Remove same times from all
               labels = self(i).labels;
            end
            indL = find(ismember(self(i).labels,labels));
            if any(indL)
               indT = cellfun(@(x) ismember(x,times),self(i).times_(indL),'uni',0);               
               for j = 1:numel(indL)
                  if any(indT{j})
                     self(i).times_{indL(j)}(indT{j}) = [];
                     self(i).values_{indL(j)}(indT{j}) = [];                     
                  end
               end
               if any(cellfun(@(x) any(x),indT))
                  % Reset properties that depend on event times
                  oldOffset = self(i).offset;
                  self(i).offset = 'windowIsReset';
                  applyWindow(self(i));
                  self(i).offset = oldOffset;
               end               
            end
         end
      end
      
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
         p.FunctionName = 'pointProcess valueFun method';
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
         % TODO handle pointProcess array
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
      
      function chop(self,shiftToWindow)
         % TODO
         % can we rechop?
         %     yes, not sure its useful, but i guess it should work.
         %     eg., chop first by trials, then chop relative to an event
         %     within each trial?
         %     references to info will get ugly, produce unexpected
         %     behavior?
         %
         % need to handle case where there is an offset?, or perhaps there
         % should be a convention?
         if nargin == 1
            shiftToWindow = true;
         end
         
         if numel(self) > 1
            error('pointProcess:chop:InputCount',...
               'You can only chop a scalar pointProcess.');
         end
         
         nWindow = size(self.window,1);
         % TODO, looped allocation
         % http://www.mathworks.com/support/bugreports/893538
         obj(nWindow) = pointProcess();
         oldOffset = self.offset;
         self.offset = 0;
         for i = 1:nWindow
            % The info map will be a reference for all elements
            obj(i).info = self.info;
            
            if shiftToWindow
               shift = self.window(i,1);
            else
               shift = 0;
            end
            
            obj(i).times_ = cellfun(@(x) x - shift,...
                                    self.times(i,:),'uni',0);
            obj(i).values_ = self.values(i,:);

            obj(i).tStart = self.window(i,1) - shift;
            obj(i).tEnd = self.window(i,2) - shift;
            obj(i).window = self.window(i,:) - shift;
            obj(i).offset = oldOffset(i);
            
            % Need to set offset_ and window_
            obj(i).window_ = obj(i).window;
            obj(i).offset_ = self.offset_ + self.window(i,1);
         end
         
         % Currently Matlab OOP doesn't allow the handle to be
         % reassigned, ie self = obj, so we do a silent pass-by-value
         % http://www.mathworks.com/matlabcentral/newsreader/view_thread/268574
         assignin('caller',inputname(1),obj);
      end % chop
      
      %% Intervals?
      function iei = intervals(self)
         % Interevent interval representation
         iei = cellfun(@diff,self.times,'UniformOutput',false);
      end

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
      function h = plot(self,varargin)
        % alias to raster
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
         p.FunctionName = 'pointProcess raster method';
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
      function plus(x,y)
         % Overloaded addition (plus, +)
         % When one of (x,y) is a pointProcess, and the other a scalar, +
         % will change the offset according to the scalar.
         % When x & y are both pointProcesses, they will be merged
         if isa(x,'pointProcess') && isa(y,'pointProcess')
            % TODO not done yet
            % should merge the objects
            % order will matter, how to deal with names & info?
            % since x,y will be handles, we need to destroy one, and
            % reassignin to the leading variable?
         elseif isa(x,'pointProcess') && isnumeric(y)
            if numel(x) > 1
               [x.offset] = deal(list(y));
            else
               [x.offset] = deal(y);
            end
         elseif isa(y,'pointProcess') && isnumeric(x)
            if numel(y) > 1
               [y.offset] = deal(list(x));
            else
               [y.offset] = deal(x);
            end
         else
            error('Plus not defined for inputs');
         end
      end
      
      function minus(x,y)
         % Overloaded subtraction (minus, -)
         if isa(x,'pointProcess') && isa(y,'pointProcess')
            % not done yet
            % should delete the common times from object
         elseif isa(x,'pointProcess') && isnumeric(y)
            if numel(x) > 1
               [x.offset] = deal(list(-y));
            else
               [x.offset] = deal(-y);
            end
         elseif isa(y,'pointProcess') && isnumeric(x)
            if numel(y) > 1
               [y.offset] = deal(list(-x));
            else
               [y.offset] = deal(-x);
            end
         else
            error('Minus not defined for inputs');
         end
      end
   
      function bool = eq(x,y)
         % Equality (==, isequal)
         % Window-dependent properties are not used for comparison
         %
         % TODO
         % check units ?
         % maybe a 'strict' flag to compare window-dependent properties?
         % handle case where both inputs are a vector?
         if isa(x,'pointProcess') && isa(y,'pointProcess')
            % Handle case where one input is a vector
            nX = numel(x);
            nY = numel(y);
            if nX < nY
               if nX ~= 1
                  error('At least one argument must have numel==1');
               else % y is a vector
                  for i = 1:nY
                     bool(i) = x == y(i);
                  end
                  return;
               end
            elseif nY < nX
               if nY ~= 1
                  error('At least one argument must have numel==1');
               else % x is a vector
                  for i = 1:nX
                     bool(i) = y == x(i);
                  end
                  return;
               end
            end
            
            if x.info ~= y.info
               bool = false;
               return;
            elseif numel(x.times_) ~= numel(y.times_)
               bool = false;
               return;
            elseif any(x.times_ ~= y.times_)
               bool = false;
               return;
            %elseif any(x.map ~= y.map)
            %   bool = false;
            %   return;
            elseif x.tStart ~= y.tStart
               bool = false;
               return;
            elseif x.tEnd ~= y.tEnd
               bool = false;
               return;
            else
               bool = true;
            end
         else
            error('Eq is not defined for inputs');
         end
      end
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
   end % methods(Private)
end % classdef

