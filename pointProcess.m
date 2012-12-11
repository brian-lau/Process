% point process class
% simulation
% analyses
% x intensity (basic)
%  isi, 
%      cv, cv2, lvr
% conditional mean
% hazard
%
% time conversion between units
% this will mean that times property will not necessarily be vector??
%   eg., if events are coded as hours, min, seconds
% This may not even really make sense, not really units, more
% representation
%
% when info values are themselves pointProcesses, should we check time
% consistency with the parent pointProcess? ie tAbs?
% Also, when we reset, should we reset events?
%
% currently tAbs field is used for resetting and for consistency in
% pointProcessCollections. Should there be a boolean to determine whether
% to add tAbs to all operations when handling an array of pointProcesses???
%
% any way to ensure column or row outputs when someone creates matrices of
% pointProcesses?
% As many of the methods as possible for pointProcess will handle array
% calls. Since there isn't clearly a way to enforce how users create these
% arrays (ie, they can be multidimensional), we should enforce some sanity
% in the methods?
%   - for nDims> 2, always concatonate in a consistent way? issue warning
%   - for nDimes = 1, should handle row and column differently, ie,
%     getTimes should return a row if row inputs, and a column if column
%     inputs
%   THIS is done for getTimes now, need to check ALL METHODS
%
%
% HANDLE versus VALUE class
%
%
% How to ensure we can always load the data? How to save? as struct and use
% savObj and loadObj methods
%  Do I need to suppport construction from a struct?
%http://www.mathworks.com/help/matlab/matlab_oop/example--maintaining-class-compatibility.html
%http://www.mathworks.com/help/matlab/matlab_oop/passing-arguments-to-constructors-during-load.html
%http://www.mathworks.com/matlabcentral/newsreader/view_thread/261903
%http://www.cs.ubc.ca/~murphyk/Software/matlabObjects.html
%http://www.mathworks.com/support/solutions/en/data/1-BU9EU7/index.html?product=M
%http://fluffynukeit.com/tag/saveobj/
%http://www.mathworks.com/matlabcentral/fileexchange/34564-fast-serializedeserialize
%https://groups.google.com/forum/?fromgroups=#!topic/comp.soft-sys.matlab/-m6graaO5Ig
%http://stackoverflow.com/questions/3161649/loading-saved-objects-from-disk-is-slow-in-matlab
%http://www.mathworks.com/matlabcentral/answers/8056
%http://kabamaru.blogspot.fr/2012/06/saving-and-loading-functionality-to-gml.html
%   started by including version property
%
%   If I do the above, should also be a method to "break" a multiply
%   windowed pointProcess into a collection?
%
% Went with the above in a branch
% Works nicely, but an explicit decision should be made about how to handle
% arrays of pointProcesses. When a method is called on an array (not the
% default getters or setters), self is also an array.
%   One possibility is to try and ensure that all methods can sensibly
%   handle array inputs. One sensible way is to simply loop over self and
%   simply call the method for each element. 
%   The other possibility is to force the collection object to handle this,
%   in which case, it probably makes sense to forbid calling pointProcess
%   methods with array inputs???
%
% How to handle align? Perhaps we only the possibilities:
%   1) scalar, applied to all windows
%   2) vector, one per window...
%   probably need an offset property (replace tAbsShift)
%   either way, there should be one interface, through getWindowedTimes
%
% Perhaps generalize the interface to the idea that we view eventTimes
%   through a transform.
%     offset               scalar per window
%     window               % 2x1 vector per window
%     scale (time-rescale) rate function per window
% then windowedTimes -> transformTimes
% getWindowedTimes -> transform
%
%
% need an isValidWindow property
% use the start and end times to check whether there is actually the
% potential for event times in the windows, otherwise assign nan
%
% consider creating an event class that will include a single time, then
% pointProcess will have an array of events that have a common origin. This
% is a natural way to keep the mark together with it's time. Problem is
% there will be many events, we will run into penalties for object access.
%
% window should have the option to destroy data? implement in chop method

% OBJECT ARRAYS
% standard getters seems to sequentially call their method on
% each element of the object array
% calling a standard setters raises an error
%   spk2.offset = 1
%   But it turns out you can use deal, which iterates over each element
%   [spk2.offset] = deal(1,10) % different for each element
%   t = num2cell([1 10]);
%   [spk2.offset] = deal(x{:}) % same as above
%   [spk2.offset] = deal(13) % same for each element
%  
% calling other methods just passes in the entire object array...
% which means that all other methods must handle object arrays
%
% Overload other operators? 
% / or ./ to chop, 
% < > <= >= could be used to restrict eventTimes (set window)?

classdef pointProcess
%
   properties(GetAccess = public, SetAccess = private)
      % String identifier
      name
      
      % container.Map with information about process (requires Matlab 2008b)
      info
            
      % Vector of event times
      times

      % Time representation 
      unit = 'seconds';
      
      % If marked point process, vector of associated magnitudes
      % maybe this should be a container? use keys that correspond either
      % to the eventTimes (double) or eventTime index (integer). then this
      % can always be filtered, and is always correctly associated with
      % times? If pointProcess is a neuron, you might put waveforms in here
      % if pointProcess is a set of event times, like stimulus onset, you
      % might put image name or something in here. More likely, different
      % images would be different pointProcess objects? What if you do
      % revcorr and want to store each stimulus (ori, sf, color, etc)? then
      % each mark would contain a structure)
      %
      % Does this really need to be a map? I essentially use it as a list,
      % since the key values are integer index. would be more useful if
      % keys were the actual times?
      map
   end
   
   properties(GetAccess = public, SetAccess = private)
   %properties(GetAccess = public, SetAccess = immutable)
      % Start time of point process, defines origin (defaults to zero)
      tStart
      
      % End time of point process (defaults to last event time)
      tEnd
   end
   
   properties(GetAccess = public, SetAccess = public)
      % [min max] time window of interest
      window
      
      % offset of event times relative to window
      offset
   end
   
   % These dependent properties all apply the window property
   properties (GetAccess = public, SetAccess = private, Dependent = true, Transient = true)
      % # of events within window
      count
   end
   
   % Also window-dependent, but only calculated on window change
   % Possibly set Hidden = true, or define display method to hide?
   % http://blogs.mathworks.com/loren/2012/03/26/considering-performance-in-object-oriented-matlab-code/
   properties (SetAccess = private, Transient = true)
      % Should be function handle? defines intensity representations
      lambdaEstimator = '';
      
      % count/window Hz? intensity should be a class, subclass of
      % sampledProcess
%      lambda
      
      % Cell array of event times contained in window
      windowedTimes
      
      % Cell array of indices into event times for times contained in window
      windowIndex
      
      % Boolean for whether or not window lies within tStart and tEnd
      isValidWindow
   end
      
      
   properties(GetAccess = public, SetAccess = immutable)
      % Original [min max] time window of interest
      window_
      
      % Original offset
      offset_

      % Classdef version for loadobj & saveobj
      version = 0.1;
   end
   
   methods
      %% Constructor
      function self = pointProcess(varargin)
         % Constructor, arguments are taken as name/value pairs
         % name     - string identifier
         % info     - cell array of information about process
         % infoKeys - cell array of strings labelling info elements
         % times    - Vector of event times
         % map    - Corresponding vector of magnitudes for "marked" process
         % window   - Defaults to window that includes all event times,
         %            If a smaller window is passed in, event times outside
         %            the window will be DISCARDED.
         % tAbs     - Time that event times are relative
         
         if nargin == 1
            % TODO allow struct input, perhaps even obj input?
            if false
            else
               error('pointProcess:pointProcess:InputFormat',...
                  'When passing one input, it must be a struct or another pointProcess');
            end
         else
            p = inputParser;
            p.KeepUnmatched= false;
            p.FunctionName = 'pointProcess constructor';
            p.addParamValue('name','',@ischar);
            %p.addParamValue('name',datestr(now,'yyyy-mm-dd HH:MM:SS:FFF'),@ischar);
            p.addParamValue('info',[],@(x) (iscell(x) || isa(x,'containers.Map')) );
            p.addParamValue('infoKeys',[],@iscell);
            p.addParamValue('times',NaN,@isnumeric);
            p.addParamValue('map',[],@(x) isnumeric(x) || iscell(x) || isa(x,'containers.Map'));
            p.addParamValue('window',[],@isnumeric);
            p.addParamValue('offset',[],@isnumeric);
            p.addParamValue('tStart',0,@isnumeric);
            p.addParamValue('tEnd',[],@isnumeric);
            p.parse(varargin{:});
         end
         
         self.name = p.Results.name;
         
         % Sort event times
         % TODO perhaps don't use unique, as some events may all occur at
         % the same time? (eg., time-insensitive event codes strobed at the
         % same time)
         [eventTimes,tInd] = unique(p.Results.times(:)');

         % Sort corresponding map if not already a dictionary
         if isempty(p.Results.map)
            eventMap = [];
         elseif isa(p.Results.map,'containers.Map')
            if all(isKey(p.Results.map,num2cell(eventTimes)))%all(isKey(p.Results.map,num2cell(tInd)))
               % TODO THERE IS NO SORTING HERE? NOT SURE IT MAKES SENSE TO
               eventMap = p.Results.map;
            else
               error('bad map dictionary');
            end
         else
            eventMap = p.Results.map(tInd);
         end
         
         % Define the start and end times of the process
         % TODO note the possibility of negative times passed in, is this
         % default sensible???
         if p.Results.tStart == 0
            self.tStart = 0;
         else
            self.tStart = p.Results.tStart;
         end
         if isempty(p.Results.tEnd)
            self.tEnd = eventTimes(end);
         else
            self.tEnd = p.Results.tEnd;
         end
         
         % Discard event times (& map) outside of start and end
         % TODO, error when map.Count ~= numel(times)
         ind = (eventTimes>=self.tStart) & (eventTimes<=self.tEnd);
         if isempty(ind)
            self.times = [];
         else
            self.times = eventTimes(ind);
         end
         if isempty(eventMap)
            self.map = containers.Map();
         else
            if iscell(eventMap)
               %self.map = containers.Map(1:length(ind),eventMap(ind));
               self.map = containers.Map(eventTimes(ind),eventMap(ind));
            elseif isa(eventMap,'containers.Map')
               self.map = eventMap;
            else
               %self.map = containers.Map(1:length(ind),{eventMap(ind)});
               self.map = containers.Map(eventTimes(ind),{eventMap(ind)});
            end
         end
         
         % Create an info dictionary
         % TODO, force keys to be chars
         if isempty(p.Results.info)
            self.info = containers.Map();
         elseif isa(p.Results.info,'containers.Map')
            % Passing in a  map, ignore infoKeys
            self.info = p.Results.info;
         else
            if isempty(p.Results.infoKeys)
               for i = 1:length(p.Results.info)
                  infoKeys{i,1} = ['key' num2str(i)];
               end
            else
               infoKeys = p.Results.infoKeys;
            end
            self.info = containers.Map(infoKeys,p.Results.info);
         end
         
         % Set the window
         if isempty(self.times)
            self.window = [NaN NaN];
         else
            if isempty(p.Results.window)
               self.window = [min(self.times) max(self.times)];
            elseif numel(p.Results.window) == 2
               self.window = checkWindow(p.Results.window);
            else
               error('pointProcess constructor requires a single window');
            end
         end

         % Store original window and offset for resetting
         self.window_ = self.window;
         self.offset_ = self.offset;
      end % constructor
      
      %% Set functions
      function self = set.window(self,window)
         % Set the window property
         %
         % Does not work for vector inputs, need setWindow()
         self.window = checkWindow(window,size(window,1));
         % Reset offset, which is always relative to window
         self.offset = 'windowIsReset';
         % Expensive, only call when windows are changed
         self = windowTimes(self);
      end
      
      function self = setWindow(self,window)
         % Set the window property
         %
         % Allows array object input, 
         % window must either be
         % [1 x 2] vector applied to all elements of the object array
         % {nObjs x 1} cell vector containing windows for each element of
         %     the object array
         %
         % SEE ALSO
         % window
         n = numel(self);
         if iscell(window)
            window = checkWindow(window,n);
            [self.window] = deal(window{:});
         elseif isnumeric(window)
            window = checkWindow(window);
            [self.window] = deal(window);
         else
            error('pointProcess:setWindow:InputFormat','Bad window');
         end
      end
      
      function self = setInclusiveWindow(self)
         % Set windows to earliest and latest event times
         for i = 1:numel(self)
            self(i).window = [min(self(i).times) max(self(i).times)];
         end
      end
      
      function self = set.offset(self,offset)
         % Set the offset property
         %
         % Does not work for vector inputs, need setOffset()
         if strcmp(offset,'windowIsReset')
            self.offset = zeros(size(self.window,1),1);
         else
            newOffset = checkOffset(offset,size(self.window,1));
            % Reset offset, which is always relative to window
            self = offsetTimes(self,true);
            self.offset = newOffset;
            % Only call when offsets are changed
            self = offsetTimes(self);
         end
      end
      
      function self = setOffset(self,offset)
         % Set the offset property
         % Allows array object input, 
         % offset must either be
         % scalar applied to all elements of the object array
         % {nObjs x 1} cell vector containing offsets for each element of
         %     the object array
         n = numel(self);
         if iscell(offset)
            offset = checkOffset(offset,n);
            [self.offset] = deal(offset{:});
         elseif isnumeric(offset)
            offset = checkOffset(offset);
            [self.offset] = deal(offset);
         else
            error('pointProcess:setOffset:InputFormat','Bad offset');
         end
      end
                  
      function self = transform(self,functionHandle,varargin)
         keyboard
         
         % Transformation like time-rescaling, thinning, etc?
         % should be a property transformFunction that contains a function
         % handle that accepts spikes and possibly other inputs
         % applied after windowing, so the most general form would be an
         % array of function handles, but this really seems excessive...
      end
      
      function self = reset(self)
         % Reset times and windows to state when object was created         
         n = length(self);
         for i = 1:n
            self(i).window = self(i).window_;
            self(i).offset = self(i).offset_;
         end
      end
      
      %% Get Functions
      function count = get.count(self)
         % # of events within windows
         times = self.windowedTimes;
         count = cellfun(@(x) numel(x),self.windowedTimes);
      end
      
%       function times = getMarkTimes(self)
%          %    return times associated with marks that have these values
%          %    logic = and, or
%          %    values = 'object' 'numeric value' 'string'
%          % criteria, 'key' 'value' 'time
%          values = getMarkValues(self);
%          keyboard
%          
%       end
      
%       function self = selectByTimes(self,window)
%          % Return pointProcess restricted by window
%          % Destructive (ie, discards data outside window)
%          %
%          % TODO handle array input
%          self.window = window;
%          self = chop(self);
%       end
      
      function self = selectByWindow()
         % Should do the above, and make the selectByTimes actually search
         % for an array of times
      end
      
      function array = mapFun(self,fun,varargin)
         % TODO array input
         % TODO, how to respect window?
         % This should return one bool per window...
         % array = mapfun(fun,{self.map},varargin{:});
         array = mapfun(fun,self.map,varargin{:});
      end
      
      function bool = doesMapHaveValue(self,value,varargin)
         % Boolean for whether MAP dictionary has value
         %
         % It is possible to restrict to keys by passing in additional args
         % self.doesHashmapHaveValue(value,'keys',{cell array of keys})
         bool = self.doesHashmapHaveValue({self.map},value,varargin{:});
      end
      
      function array = infoFun(self,fun,varargin)
         % TODO array input
         % array = mapfun(fun,{self.map},varargin{:});
         array = mapfun(fun,self.info,varargin{:});
      end
      
      function array = getInfoKeys(self,flatBool)
         % Return array of keys in INFO dictionary
         %
         % If flatBool is true (default false), the returned cell array
         % will be collapsed across all pointProcess elements passed in
         if nargin < 2
            flatBool = false;
         end
         
         n = numel(self);
         array = cell(size(self));
         for i = 1:n
            array{i} = self(i).info.keys;
         end
         if flatBool
            array = unique(deCell(array));
         end
      end
      
      function bool = doesInfoHaveKey(self,key)
         % Boolean for whether INFO dictionary has key
         bool = arrayfun(@(x,y) x.info.isKey(y),self,repmat({key},size(self)));
      end
            
      function bool = doesInfoHaveValue(self,value,varargin)
         % Boolean for whether INFO dictionary has value
         %
         % It is possible to restrict to keys by passing in additional args
         % self.doesInfoHaveValue(value,'keys',{cell array of keys})
         bool = self.doesHashmapHaveValue({self.info},value,varargin{:});
      end
      
%       function values = getMapValues(self)
%          % Return all map values
%          values = deCell(arrayfun(@(x) x.map.values,self,'uni',false));
%       end
%       function self = selectByMarks(self,value)
%          % search for marks containing value(s) [union]
%          % deleteMarks
%          % will return pointProcess with events that contain mark value
%          % eg, search for trial boundaries
%       end
   

      function bool = inWindow()
         % function to determine if some property is in a window?
         % don't really need for times, since count does this?
         % for marks, search by value
         % eg, self.inWindow('Large/Large cue') should return a boolean
         % vector (for single object) over windows, or a cell array of
         % vectors (for multiple objects)
      end
      
      
      function self = deleteTimes(self,times)
         % Remove times and associated marks
         % need to call window and offset setters to reassign dependents
      end
      function self = deleteMap(self,keys)
         % Remove map elements and associated times
         % need to call window and offset setters to reassign dependents
      end
      
%       function lambda = get.lambda(self)
%          % # of events within windows
%          times = self.windowedTimes;
%          for i = 1:length(times)
%             lambda(i,1) = length(times{i}) / (self.window(i,2)-self.window(i,1));
%          end
%       end

%       function intervals = get.intervals(self)
%          % Interevent interval representation
% %         times = getTimes(self,self.window);
% %         intervals = diff(times{1});
%          
%          times = self.windowedTimes;
%          for i = 1:length(times)
%             intervals{i,1} = diff(times{i});
%          end
%       end
      
%       function countingProcess = get.countingProcess(self)
%          % Counting process representation
%          if any(isnan(self.window))
%             countingProcess = [NaN NaN];
%          else
%             window = self.window;
%             times = getTimes(self,window);
%             times = times{1};
%             count = cumsum(ones(size(times)));
%             tStart = max(-inf,unique(min(times)));
%             countingProcess = [[tStart;times] , [0;count]];
%          end
%       end

      %% Functions
      function self = addInfo(self)
         % key-value addition to info property
      end
      
      function self = removeInfo(self)
         % key-value deletion to info property
      end
      
      function self = chop(self,window)
         % TODO
         % marks create new map for each window
         % can we rechop?
         %     yes, not sure its useful, but i guess it should work.
         %     eg., chop first by trials, then chop relative to an event
         %     within each trial?
         % can we reconstruct? ie, coalesce back to original?
         %     no, chop allows overlapping or gapped windows, so there is
         %     no restriction that the data can be reconstructed
         %
         % need to handle case where there is an offset?, 
         if nargin == 2
            
         else
            % else we will chop based on the current windows
            nWindow = size(self.window,1);
            if 0%nWindow == 1
               % restrict event times to window? destructive
               return;
            else
               obj(1:nWindow) = pointProcess();
               for i = 1:nWindow
                  % Leave info alone
                  % Copy marks in window to new object, resetting keys
%                   temp = self.windowIndex{i};                  
%                   marks = copyMap(self.marks,num2cell(temp),...
%                      num2cell(temp - temp(1) + 1));
                  % how to deal with offset, should zero to window, but
                  % store windowStart as offset_? perhaps add original
                  % offset_?
%                  keyboard
%                   obj(i) = pointProcess(...
%                      'name',self.name,...
%                      'info',self.info,...
%                      'times',self.windowedTimes{i},...
%                      'marks',marks,...
%                      'tStart',self.window(i,1),...
%                      'tEnd',self.window(i,2),...
%                      'window',self.window(i,:),...
%                      'offset',self.offset(i)...
%                      );
                  
                  temp = self.windowedTimes{i};                  
                  map = copyMap(self.map,num2cell(temp));
                  % II. Avoid constructor, faster, but some properties
                  % cannot be immutable
                  obj(i).name = self.name;
                  obj(i).info = self.info;
                  obj(i).times = self.windowedTimes{i};
                  obj(i).map = map;
                  obj(i).tStart = self.window(i,1);
                  obj(i).tEnd = self.window(i,2);
                  obj(i).window = self.window(i,:);
                  obj(i).offset = self.offset(i);
                  % Need to set offset_ and window_
               end
               self = obj;
            end
         end
      end
            
      function h = plot(self,varargin)
         % Plot times & counting process
         % TODO 
         % vector input? Maybe just pool all times
         % allow passing in handle
         times = getTimes(self,self.window);
         if isempty(times)
            fprintf('No times in window.\n');
            return;
         end
         countingProcess = self.countingProcess;

         h = plotRaster(times,'grpBorder',false,...
            'window',self.window,'yOffset',0.1,'markerStyle','x','markersize',6,...
            'labelXAxis',false,'labelYaxis',false,varargin{:});
         stairs(countingProcess(:,1),countingProcess(:,2));

         axis tight;
         xlabel('Time');
         %xlabel(['Time (' self.timeUnits ')']);
         ylabel('Cumulative events (N_t)');
      end
      
      function [h,yOffset] = raster(self,varargin)
         % Raster plot
         % For a full description of the possible parameters, 
         % see also: plotRaster

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
         
         n = length(self);
         if n == 1
            times = self.windowedTimes;
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
      
      function [r,t,r_sem,count,reps] = getPsth(self,bw,varargin)
         % Get history-independent intensity (marginal)
         % For a full description of the possible parameters, 
         % see also: getPsth

         % TODO
         % When self.unit functioning, need to reconcile units with bandwidth
         % here
         % check output when vector input is a row
         
         p = inputParser;
         p.KeepUnmatched= true;
         p.FunctionName = 'pointProcess psth method';
         p.addRequired('bw', @isnumeric);
         p.parse(bw,varargin{:});
         % Passed through to getPsth
         params = p.Unmatched;
         
         n = length(self);
         % These window changes will NOT be persistent (not copied into object)
         if isfield(params,'window')
            window = self.checkWindow(params.window,n);
         else
            window = self.checkWindow(cat(1,self.window),n);
         end

         times = getTimes(self,window);
         [r,t,r_sem,count,reps] = getPsth(times,p.Results.bw,params);
      end
            
      %% Operators
      function obj = plus(x,y)
         % Addition
         if isa(x,'pointProcess') && isa(y,'pointProcess')
            % not done yet
            % should merge the objects
            % order will matter, how to deal with names & info?
         elseif isa(x,'pointProcess') && isnumeric(y)
            [x.offset] = deal(y);
            obj = x;
         elseif isa(y,'pointProcess') && isnumeric(x)
            [y.offset] = deal(x);
            obj = y;
         else
            error('Plus not defined for inputs');
         end
      end
      
      function obj = minus(x,y)
         % Subtraction
         if isa(x,'pointProcess') && isa(y,'pointProcess')
            % not done yet
            % should delete the common times from object
         elseif isa(x,'pointProcess') && isnumeric(y)
            [x.offset] = deal(-y);
            obj = x;
         elseif isa(y,'pointProcess') && isnumeric(x)
            [y.offset] = deal(-x);
            obj = y;
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
            
            if strcmp(x.name,y.name) ~= true
               bool = false;
               return;
            elseif x.info ~= y.info
               bool = false;
               return;
            elseif numel(x.times) ~= numel(y.times)
               bool = false;
               return;
            elseif any(x.times ~= y.times)
               bool = false;
               return;
            elseif any(x.map ~= y.map)
               bool = false;
               return;
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
   end
   
   methods(Access = private)
      function self = windowTimes(self)
         nWindow = size(self.window,1);
         times = self.times;
         window = self.window;
         windowedTimes = cell(nWindow,1);
         windowIndex = cell(nWindow,1);
         isValidWindow = false(nWindow,1);
         for i = 1:nWindow
            ind = (times>=window(i,1)) & (times<=window(i,2));
            windowedTimes{i,1} = times(ind);
            windowIndex{i,1} = find(ind);
            if (window(i,1)>=self.tStart) && (window(i,2)<=self.tEnd)
               isValidWindow(i) = true;
            else
               isValidWindow(i) = false;
            end
         end
         self.windowedTimes = windowedTimes;
         self.windowIndex = windowIndex;
         self.isValidWindow = isValidWindow;
      end
      
      function self = offsetTimes(self,reset)
         if nargin == 1
            reset = false;
         end
         if reset
            offset = -self.offset;
         else
            offset = self.offset;
         end
         for i = 1:length(offset)
            self.windowedTimes{i,1} = self.windowedTimes{i,1} + offset(i);
         end
      end
   end
   methods(Static)
      function bool = doesHashmapHaveValue(map,value,varargin)
         % Boolean for whether dictionary has value
         %
         % It is possible to restrict to keys by passing in additional args
         % self.doesHashmapHaveValue(value,'keys',{cell array of keys})
         temp = mapfun(@(x,y) isequal(x,y),map,'params',{value},varargin{:});
         bool = cellfun(@(x) any(x),temp);
      end      
   end
   
end

