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
%
% define an events class with enumerations of event codes? this might be
% nice because we can filter objects by isa(eevent)

% 13.12.2012 switch to handle class for pass-by-reference behavior. This
% makes more sense when dealing with a collection which has an array of
% pointProcess objects. 
% more straightforward and less error-prone to call methods
% coll.pointProcess.method
% rather than
% coll.pointProcess = coll.pointProcess.method

% REQUIREMENTS
% R2008b - containers.Maps
% R2010a - containers.Maps constructor to specify key and value type
% R2011a - matlab.mixin.Copyable for copying handle objects


classdef (CaseInsensitiveProperties = true) pointProcess < dynamicprops & hgsetget & matlab.mixin.Copyable
%
   properties
      % String identifier
      name
      
      % Information about point process. This is a dictionary keyed with
      % strings. Values can be arbitrary data types.
      info      
   end
   
   properties(GetAccess = public, SetAccess = private)
      % Vector of event times
      times

      % Information associated with each event time. This is a dictionary
      % keyed by each event time. Values can be arbitrary data types.
      % TODO setting map is actually not private, due to handle???
      map

      % Time representation (placeholder)
      unit = 'seconds';
      
      % Identifier for clock used to measure time. Anticipating drift-correction
      clock
   end
   
   properties(GetAccess = public, SetAccess = private)
      % Start time of point process, (defaults to min(0,min(times)))
      tStart
      
      % End time of point process (defaults to last event time)
      tEnd
   end
   
   properties(GetAccess = public, SetAccess = public, SetObservable, AbortSet)
      % [min max] time window of interest
      window
      
      % Offset of event times relative to window
      offset
   end
   
   % These dependent properties all apply the window property
   properties(GetAccess = public, SetAccess = private, Dependent = true, Transient = true)
      % # of events within window
      count
   end
   
   % Also window-dependent, but only calculated on window change
   % http://blogs.mathworks.com/loren/2012/03/26/considering-performance-in-object-oriented-matlab-code/
   properties(SetAccess = private, Transient = true)
      % Cell array of event times contained in window(s). Note that any
      % offset is applied *after* windowing, so windowedTimes can be outside 
      % of the windows property
      windowedTimes
      
      % Cell array of indices into event times for times contained in window
      windowIndex
      
      % Boolean for whether or not window lies within tStart and tEnd
      isValidWindow
   end
      
   properties(GetAccess = public, SetAccess = private, Hidden = true)
      % Original [min max] time window of interest
      window_
      
      % Original offset
      offset_
      
      % Anticipating need to handle case of non-unique times?
      threshold_ = 10*eps;
   end
   
   properties(GetAccess = public, SetAccess = private)
      % Classdef version for loadobj & saveobj
      version = 0.1;
   end
   
   methods
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %% Constructor
      function self = pointProcess(varargin)
         % Constructor, arguments are taken as name/value pairs
         % name     - string identifier
         % info     - cell array of information about process
         % infoKeys - cell array of strings labelling info elements
         % times    - Vector of event times
         % map      - Corresponding vector of magnitudes for "marked" process
         % window   - Defaults to window that includes all event times,
         %            If a smaller window is passed in, event times outside
         %            the window will be DISCARDED.
         % tAbs     - Time that event times are relative
         
         if nargin == 0
            return;
         end
         
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'pointProcess constructor';
         p.addParamValue('name',datestr(now,'yyyy-mm-dd HH:MM:SS:FFF'),@ischar);
         p.addParamValue('info',[],@(x) (iscell(x) || isa(x,'containers.Map')) );
         p.addParamValue('infoKeys',[],@iscell);
         p.addParamValue('times',NaN,@isnumeric);
         p.addParamValue('map',[],@(x) isnumeric(x) || iscell(x) || isa(x,'containers.Map'));
         p.addParamValue('window',[],@isnumeric);
         p.addParamValue('offset',[],@isnumeric);
         p.addParamValue('tStart',[],@isnumeric);
         p.addParamValue('tEnd',[],@isnumeric);
         p.parse(varargin{:});
         
         self.name = p.Results.name;
         
         % Create the info dictionary
         if isempty(p.Results.info)
            self.info = containers.Map('KeyType','char','ValueType','any');
         elseif isa(p.Results.info,'containers.Map')
            % Passing in a map, ignore infoKeys
            if ~strcmp(p.Results.info.KeyType,'char')
               error('pointProcess:Constructor:InputFormat',...
                  'info keys must be chars.');
            else
               self.info = p.Results.info;
            end
         else
            if isempty(p.Results.infoKeys)
               for i = 1:length(p.Results.info)
                  infoKeys{i,1} = ['key' num2str(i)];
               end
            else
               infoKeys = p.Results.infoKeys;
            end
            self.info = containers.Map(infoKeys,p.Results.info,...
            'uniformValues',false);
         end
         
         if any(strcmp(varargin,'times')) && isa(p.Results.map,'containers.Map')
            error('pointProcess:Constructor:InputCount',...
               'Map is a dictionary, it''s keys are assumed to be pointProcess times.');
         elseif isa(p.Results.map,'containers.Map')
            % We have a map, assume the keys are our times
            map = p.Results.map;
            if strcmp(map.KeyType,'double')
               eventTimes = cell2mat(map.keys);
               eventMap = map;
            else
               error('pointProcess:Constructor:InputFormat',...
                  'If map is a dictionary, it''s keys are assumed to be doubles.');
            end
         elseif ~isempty(p.Results.times) && ~any(isnan(p.Results.times))
            % We have a simple array of times, sort and apply to map
            [eventTimes,tInd] = unique(p.Results.times(:)');
            if isempty(p.Results.map)
               eventMap = [];
            else
               % Not a containers.Map
               eventMap = p.Results.map(tInd);
            end
         else
            % No event times
            return;
%             error('pointProcess:Constructor:InputCount',...
%                'Map is not a containers.Map, you need to pass in valid times.');
         end
         
         %% If we have event times
         
         % Define the start and end times of the process
         if isempty(p.Results.tStart)
            self.tStart = min([min(eventTimes) 0]);
         else
            self.tStart = p.Results.tStart;
         end
         if isempty(p.Results.tEnd)
            self.tEnd = eventTimes(end);
         else
            self.tEnd = p.Results.tEnd;
         end
         
         % Discard event times (& map) outside of start and end
         ind = (eventTimes>=self.tStart) & (eventTimes<=self.tEnd);
         if sum(ind) == 0
            % No times left
            self.times = [];
            return;
         else
            self.times = eventTimes(ind);
            if isa(eventMap,'containers.Map')
               % Remove keys that are not in times
               keys = eventMap.keys;
               ind = ~ismember(cell2mat(keys),self.times);
               if sum(ind) > 0
                  eventMap.remove(keys(ind));
               end
            elseif ~isempty(eventMap)
               eventMap = eventMap(ind);
            end
         end
         
         % Create the map dictionary if we have times left
         if isempty(eventMap)
            % Set a default map, with event times as keys
            self.map = containers.Map(self.times,cell(size(self.times)),...
               'uniformValues',false);
         else
            if iscell(eventMap)
               self.map = containers.Map(self.times,eventMap,...
                  'uniformValues',false);
            elseif isa(eventMap,'containers.Map')
               self.map = eventMap;
            elseif isvector(eventMap)
               self.map = containers.Map(self.times,num2cell(eventMap),...
                  'uniformValues',false);
            else
               error('Should not get here');
            end
         end
                  
         % Set the window
         if isempty(p.Results.window)
            self.window = [min(self.times) max(self.times)];
         else
            self.window = checkWindow(p.Results.window,size(p.Results.window,1));
         end
         
         % Set the offset
         if isempty(p.Results.offset)
            self.offset = 0;
         else
            self.offset = checkOffset(p.Results.offset,size(p.Results.offset,1));
         end         

         % Store original window and offset for resetting
         self.window_ = self.window;
         self.offset_ = self.offset;
         
         % Property listeners
         %addlistener(self,'window','PostSet',@pointProcess.zeroOffset);
         %addlistener(self,'offset','PreSet',@pointProcess.undoOffset);
      end % constructor
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      %% Set functions
      function set.window(self,window)
         % Set the window property (does not work for arrays of objects)
         %
         % SEE ALSO
         % setWindow, windowTimes
         self.window = checkWindow(window,size(window,1));
         % Reset offset, which always follows window
          self.offset = 'windowIsReset';
         % Expensive, only call when windows are changed
         windowTimes(self);
      end
      
      function setWindow(self,window)
         % Set the window property. Works for array object input, where
         % window must either be
         %   [1 x 2] vector applied to all elements of the object array
         %   {nObjs x 1} cell vector containing windows for each element of
         %       the object array
         %
         % SEE ALSO
         % window, windowTimes
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
      
      function setInclusiveWindow(self)
         % Set windows to earliest and latest event times
         %
         % SEE ALSO
         % window, setWindow, windowTimes
         for i = 1:numel(self)
            self(i).window = [min(self(i).times) max(self(i).times)];
         end
      end
      
      function set.offset(self,offset)
         % Set the offset property (does not work for arrays of objects)
         %
         % SEE ALSO
         % setOffset, offsetTimes
          if strcmp(offset,'windowIsReset')
             self.offset = zeros(size(self.window,1),1);
          else
            newOffset = checkOffset(offset,size(self.window,1));
            % Reset offset, which is always follows window
            offsetTimes(self,true);
            self.offset = newOffset;
            % Only call when offsets are changed
            offsetTimes(self);
         end
       end
      
      function setOffset(self,offset)
         % Set the offset property. Works for array object input, where
         % offset must either be
         %   scalar applied to all elements of the object array
         %   {nObjs x 1} cell vector containing offsets for each element of
         %       the object array
         %
         % SEE ALSO
         % offset, offsetTimes
         
         % TODO, this will bomb if a vector is passed in, even when it is
         % the numel(self). Reason being, I don't know whether you want to
         % try and apply each element of offset to eachof(self), or all of
         % offset to eachof(self). Can I try/catch and infer?
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

      function array = windowFun(self,fun,nOpt,varargin)
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
         % result = spk.windowFun(@(x) max(x))
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
         if nargin < 3
            nOpt = 1;
         end
         
         if numel(self) == 1
            if nOpt == 1
               array = cellfun(fun,self.windowedTimes,varargin{:});
            else
               [array{1:nOpt}] = cellfun(fun,self.windowedTimes,varargin{:});
            end
         else
            array = cell(size(self));
            for i = 1:numel(self)
               array{i} = windowFun(self(i),fun,nOpt,varargin{:});
            end
         end
      end
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
            
      function reset(self)
         % Reset times and windows to state when object was created         
         for i = 1:numel(self)
            self(i).window = self(i).window_;
            self(i).offset = self(i).offset_;
         end
      end
      
      %% Get Functions
      function count = get.count(self)
         % # of event times within windows
         times = self.windowedTimes;
         if isempty(times)
            count = [];
         else
            count = cellfun(@(x) numel(x),self.windowedTimes);
         end
      end
            
%       function self = selectByTimes(self,window)
%          % Return pointProcess restricted by window
%          % Destructive (ie, discards data outside window)
%          %
%          % TODO handle array input
%          self.window = window;
%          self = chop(self);
%       end
      
%       function selectByWindow()
%          % Should do the above, and make the selectByTimes actually search
%          % for an array of times
%       end
      
      function addTimes(self,times)
         % Add times
         % should handle numeric or map input
         % should check existing times, 
         % and prevent overwriting existing times, perhaps with flag to
         % 1) overwrite
         % 2) add eps to make unique
         
      end
      
      function removeTimes(self,times)
         % Remove times and associated map keys
         % Note that this does NOT change tStart or tEnd
         %
         % times - array of event times to remove
         %
         % SEE ALSO
         % removeMapKeys
         for i = 1:numel(self)
            ind = ismember(self(i).times,times);
            if any(ind)
               % Map is handle object
               self(i).map.remove(num2cell(self(i).times(ind)));
               self(i).times(ind) = [];
               % Reset properties that depend on event times
               oldOffset = self(i).offset;
               self(i).window = self(i).window;
               self(i).offset = oldOffset;
            end
         end
      end
      
      function array = mapFun(self,fun,varargin)
         % TODO array input
         % TODO, how to respect window?
         % array = mapfun(fun,{self.map},varargin{:});
         array = mapfun(fun,self.map,varargin{:});
      end
      
      function array = getMapKeys(self)
         % Should be searchable by value?
         % Useful if we know a value (ie, 'start trial'), and want to know
         % the time(s)
         % alias to getMapTimes
         array = arrayfun(@(x) x.map.keys,self,'UniformOutput',false);
      end
      
      function array = getMapValues(self)
         array = arrayfun(@(x) x.map.values,self,'UniformOutput',false);
      end
      
      function [bool,keys] = doesMapHaveValue(self,value,varargin)
         % Boolean for whether MAP dictionary has value
         %
         % It is possible to restrict to keys by passing in additional args
         % self.doesHashmapHaveValue(value,'keys',{cell array of keys})
         [bool,keys] = self.doesHashmapHaveValue({self.map},value,varargin{:});
      end
      
      function removeMapKeys(self,keys)
         % Remove map keys and associated times
         %
         % SEE ALSO
         % removeTimes
         if iscell(keys)
            ind = cellfun(@(x) ~isnumeric(x),keys);
            if any(ind)
               fprintf('%g non-numeric keys ignored.',sum(ind));
            end
            keys = cell2mat(keys(~ind));
         else
            error('pointProcess:removeMapKeys:InputFormat',...
               'keys must be numeric or cell array');
         end
         removeTimes(self,keys);
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
         
         % TODO replace with arrayfun
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
         % yuck...
         % events(1).map.values(num2cell(events(1).times((events(1).windowIndex{1}))))
      end
      
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
      
      function chop(self,window)
         % TODO
         % marks create new map for each window
         % can we rechop?
         %     yes, not sure its useful, but i guess it should work.
         %     eg., chop first by trials, then chop relative to an event
         %     within each trial?
         % can we reconstruct? ie, coalesce back to original?
         %     no, chop allows overlapping or gapped windows, so there is
         %     no restriction that the data can be reconstructed
         %     however, may still be useful. info is the same for all
         %     elements (reference), but maps will have to be concatonated
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
               obj(nWindow) = pointProcess();
               for i = 1:nWindow
                  % Leave info alone
                  % Copy marks in window to new object, resetting keys
%                   temp = self.windowIndex{i};                  
%                   marks = copyMap(self.marks,num2cell(temp),...
%                      num2cell(temp - temp(1) + 1));
                  % how to deal with offset, should zero to window, but
                  % store windowStart as offset_? perhaps add original
                  % offset_?
                  
                  temp = self.windowedTimes{i};                  
                  map = copyMap(self.map,num2cell(temp));
                  
                  obj(i).name = self.name;
                  obj(i).info = self.info;
                  obj(i).times = self.windowedTimes{i};
                  obj(i).map = map;
                  obj(i).tStart = self.window(i,1);
                  obj(i).tEnd = self.window(i,2);
                  obj(i).window = self.window(i,:);
                  obj(i).offset = self.offset(i);
                  
                  %addlistener(obj(i),'window','PostSet',@pointProcess.zeroOffset);
                  %addlistener(obj(i),'offset','PreSet',@pointProcess.undoOffset);
                  % Need to set offset_ and window_
               end
               
               % Currently Matlab OOP doesn't allow the handle to be
               % reassigned, ie self = obj, so we do a silent pass-by-value
               % http://www.mathworks.com/matlabcentral/newsreader/view_thread/268574
               assignin('caller',inputname(1),obj);
            end
         end
      end
            
      function h = plot(self,varargin)
%          % Plot times & counting process
%          % TODO 
%          % vector input? Maybe just pool all times
%          % allow passing in handle
%          times = getTimes(self,self.window);
%          if isempty(times)
%             fprintf('No times in window.\n');
%             return;
%          end
%          countingProcess = self.countingProcess;
% 
%          h = plotRaster(times,'grpBorder',false,...
%             'window',self.window,'yOffset',0.1,'markerStyle','x','markersize',6,...
%             'labelXAxis',false,'labelYaxis',false,varargin{:});
%          stairs(countingProcess(:,1),countingProcess(:,2));
% 
%          axis tight;
%          xlabel('Time');
%          %xlabel(['Time (' self.timeUnits ')']);
%          ylabel('Cumulative events (N_t)');
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
      
%       function [r,t,r_sem,count,reps] = getPsth(self,bw,varargin)
%          % Get history-independent intensity (marginal)
%          % For a full description of the possible parameters, 
%          % see also: getPsth
% 
%          % TODO
%          % When self.unit functioning, need to reconcile units with bandwidth
%          % here
%          % check output when vector input is a row
%          
%          p = inputParser;
%          p.KeepUnmatched= true;
%          p.FunctionName = 'pointProcess psth method';
%          p.addRequired('bw', @isnumeric);
%          p.parse(bw,varargin{:});
%          % Passed through to getPsth
%          params = p.Unmatched;
%          
%          n = length(self);
%          % These window changes will NOT be persistent (not copied into object)
%          if isfield(params,'window')
%             window = self.checkWindow(params.window,n);
%          else
%             window = self.checkWindow(cat(1,self.window),n);
%          end
% 
%          times = getTimes(self,window);
%          [r,t,r_sem,count,reps] = getPsth(times,p.Results.bw,params);
%       end
            
      %% Operators
      function plus(x,y)
         % Overloaded addition (plus, +)
         if isa(x,'pointProcess') && isa(y,'pointProcess')
            % not done yet
            % should merge the objects
            % order will matter, how to deal with names & info?
         elseif isa(x,'pointProcess') && isnumeric(y)
            [x.offset] = deal(y);
            %obj = x;
         elseif isa(y,'pointProcess') && isnumeric(x)
            [y.offset] = deal(x);
            %obj = y;
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
            [x.offset] = deal(-y);
            %obj = x;
         elseif isa(y,'pointProcess') && isnumeric(x)
            [y.offset] = deal(-x);
            %obj = y;
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
   end % methods (Public)
   
   methods(Access = private)
      function windowTimes(self)
         %
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
      
      function offsetTimes(self,reset)
         %
         if nargin == 1
            reset = false;
         end
         if reset
            offset = -self.offset;
         else
            offset = self.offset;
         end
         for i = 1:numel(offset)
            self.windowedTimes{i,1} = self.windowedTimes{i,1} + offset(i);
         end
      end
   end % methods(Private)
   
   methods(Static, Access = private)
      function [bool,keys] = doesHashmapHaveValue(map,value,varargin)
         % Boolean for whether dictionary contains value
         %
         % OUTPUT
         % bool - boolean indicating whethe value exists in map
         % keys - corresponding keys for which bool is true
         %
         % It is possible to restrict to keys by passing in additional args
         % self.doesHashmapHaveValue(value,'keys',{cell array of keys})
         %
         % SEE ALSO
         % mapfun
         
         % TODO, checking for cell array input?
         
         [temp,keys] = mapfun(@(x,y) isequal(x,y),map,'params',{value},varargin{:});
         bool = cellfun(@(x) any(x),temp);
         if nargout == 2
            for i = 1:numel(temp)
               keys{i} = keys{i}(logical(temp{i}));
            end
         end
      end
      
%       function zeroOffset(obj,varargin)
%          aObj = varargin{1}.AffectedObject;
%          if numel(aObj) > 1
%             error('How did I get here???');
%          end
%          aObj.offset = zeros(size(aObj.window,1),1);
%          fprintf('Zeroed offset!\n');
%       end
%       
%       function undoOffset(obj,varargin)
%          aObj = varargin{1}.AffectedObject;
%          if numel(aObj) > 1
%             error('How did I get here???');
%          end
%          
%          offset = -aObj.offset;
%          if numel(offset) == numel(aObj.windowedTimes)
%             for i = 1:numel(offset)
%                aObj.windowedTimes{i,1} = aObj.windowedTimes{i,1} + offset(i);
%             end
%             fprintf('Undid offset!\n');
%          else
%             fprintf('Did not undo offset. numel(offset) ~= numel(windows)\n');
%          end
%      end

   end % methods(Static)
   
end % classdef

