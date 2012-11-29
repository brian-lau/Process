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
% let's be concrete.
% one neuron
% let's say 10 orientations presented randomly every 20 ms
%
% want to be able to do
%
% spk.align([ori[1]times,ori[2]times... ori[10]times,'window',[-.1 0])
% spk.rate should return a comma separated list of rates for each window
%
% 


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
      timeUnits = 'seconds';
      
      % If marked point process, vector of associated magnitudes
      marks
   end
   
   properties(GetAccess = public, SetAccess = public)

      % This is likely just for info, not useful to convert
      % perhaps unnecessary or nonsense if marks is itself an array of
      % objects?
%      markUnits = 'none';
 
      % [min max] time window of interest
      window
      
      % offset of event times relative to window
      offset
   end

   % These dependent properties all apply the window property
   properties (GetAccess = public, SetAccess = private, Dependent = true, Transient = true)
      
%      % Interevent interval representation
%      intervals
      
%       % Counting process representation
%       countingProcess
      
      % # of events within window
      count
      
      % count/window Hz?
      rate 
      
      % Minimum event time within window
      minTime
      
      % Minimum event time within window
      maxTime
   end
   
   % Also window-dependent, but only calculated on window change
   % Possibly set Hidden = true, or define display method to hide?
   % http://blogs.mathworks.com/loren/2012/03/26/considering-performance-in-object-oriented-matlab-code/
   properties (SetAccess = private, Transient = true)
      % Cell array of event times contained in window
      windowedTimes
      
      % Cell array of indices into event times for times contained in window
      windowIndex
   end
      
   properties(GetAccess = public, SetAccess = immutable)
      % Time that event times are relative to when object is constructed
      tAbs
      
      %
      version = 0.1;
   end
      
   properties(GetAccess = private, SetAccess = immutable)
      % Original [min max] time window of interest
      window_
      
      % Original offset
      offset_
   end
   
   methods
      %% Constructor
      function self = pointProcess(varargin)
         % Constructor, arguments are taken as name/value pairs
         % name     - string identifier
         % info     - cell array of information about process
         % infoKeys - cell array of strings labelling info elements
         % times    - Vector of event times
         % marks    - Corresponding vector of magnitudes for "marked" process
         % window   - Defaults to window that includes all event times,
         %            If a smaller window is passed in, event times outside
         %            the window will be DISCARDED.
         % tAbs     - Time that event times are relative
         
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'pointProcess constructor';
         p.addParamValue('name',datestr(now,'yyyy-mm-dd HH:MM:SS:FFF'),@ischar);
         p.addParamValue('info',[],@iscell);
         p.addParamValue('infoKeys',[],@iscell);
         p.addParamValue('times',NaN,@isnumeric);
         p.addParamValue('marks',[],@isnumeric);
         p.addParamValue('window',[],@isnumeric);
         p.addParamValue('offset',[],@isnumeric);
         p.addParamValue('tAbs',0,@isnumeric);
         p.parse(varargin{:});
         
         self.name = p.Results.name;
         self.times = sort(p.Results.times(:)');
         
         % Create a dictionary
         if isempty(p.Results.info)
            self.info = containers.Map();
         else
            if isempty(p.Results.infoKeys)
               for i = 1:length(p.Results.info)
                  infoKeys{i,1} = ['dim' num2str(i)];
               end
            else
               infoKeys = p.Results.infoKeys;
            end
            self.info = containers.Map(infoKeys,p.Results.info);
         end
         
         % Is this a marked point process?
         % TODO sort marks according to times!!!
         if ~isempty(p.Results.marks)
            marks = p.Results.marks(:);
            if sum(size(marks)==size(self.times)) == 2
               self.marks = marks;
            else
               self.marks = [];
            end
         end
         
         if isempty(p.Results.times)
            self.window = [-inf inf];
         else
            if isempty(p.Results.window)
               self.window = [min(self.times) max(self.times)];
            elseif numel(p.Results.window) == 2
               self.window = self.checkWindow(p.Results.window);
            else
               error('pointProcess constructor requires a single window');
            end
         end

         % Tuck away the original window for resetting
         self.window_ = self.window;
         
         % TODO allow offset parameter?
         
         % TODO, don't discard any data, leave it to user
         % allow multiple windows on input
         % Discard event times (& marks) outside of user-supplied window
         ind = (self.times>=self.window(1)) & (self.times<=self.window(2));
         self.times = self.times(ind);
         if ~isempty(self.marks)
            self.marks = self.marks(ind);
         end
         
         if p.Results.tAbs == 0
            self.tAbs = 0;
         else
            self.tAbs = p.Results.tAbs;
         end
         
      end
      
      %% Set functions
      function self = set.window(self,window)
         % Set the window property
         %
         % Does not work for vector inputs, see setWindow()
         
         self.window = self.checkWindow(window,size(window,1));
         % Reset offset, which is always relative to window
         self.offset = 'windowIsReset';
         % Expensive, only call when windows are changed
         self = windowTimes(self);
      end
      
      function self = set.offset(self,offset)
         % Set the offset property
         if strcmp(offset,'windowIsReset')
            self.offset = zeros(size(self.window,1));
         else
            newOffset = self.checkOffset(offset,size(self.window,1));
            % Reset offset, which is always relative to window
            self = offsetTimes(self,true);
            self.offset = newOffset;
            % Only call when offsets are changed
            self = offsetTimes(self);
         end
      end
            
      function self = setInclusiveWindow(self)
         % Set windows to earliest and latest event times
         n = length(self);
         for i = 1:n
            self(i).window = [min(self(i).times) max(self(i).times)];
         end
      end
      
      %% Get Functions
      function self = windowTimes(self)
         n = length(self);
         if n == 1
            nWindow = size(self.window,1);
            times = self.times;
            window = self.window;
            windowedTimes = cell(nWindow,1);
            windowedIndex = cell(nWindow,1);
            for i = 1:nWindow
               ind = (times>=window(i,1)) & (times<=window(i,2));
               windowedTimes{i,1} = times(ind);
%               windowedIndex{i,1} = find(ind);
            end
            self.windowedTimes = windowedTimes;
%            self.windowedIndex = windowedIndex;
         else
            % Handle array of objects??
         end
      end
      
      function self = offsetTimes(self,reset)
         if nargin == 1
            reset = false;
         end
         n = length(self);
         if n == 1
            %keyboard
            if reset 
               offset = -self.offset;
            else
               offset = self.offset;
            end
            windowedTimes = cell(length(offset),1);
            for i = 1:length(offset)
               %self.windowedTimes{i,1} = self.windowedTimes{i,1} + offset(i);
               windowedTimes{i,1} = self.windowedTimes{i,1} + offset(i);
               %self.windowedTimes{i,1} = randn(1,100);
            end
            self.windowedTimes = windowedTimes;
         else
            % Handle array of objects??
         end
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
      
      function count = get.count(self)
         % # of events within windows
         times = self.windowedTimes;
         for i = 1:length(times)
            count(i,1) = length(times{i});
         end
      end
      
      function rate = get.rate(self)
         % # of events within windows
         times = self.windowedTimes;
         for i = 1:length(times)
            rate(i,1) = length(times{i}) / (self.window(2,i)-self.window(1,i));
         end
      end

      function minTime = get.minTime(self)
         % Minimum event time within windows
         times = self.windowedTimes;
         for i = 1:length(times)
            minTime(i,1) = min(times{i});
         end
      end
      
      function maxTime = get.maxTime(self)
         % Maximum event time within windows
         times = self.windowedTimes;
         for i = 1:length(times)
            maxTime(i,1) = max(times{i});
         end
     end

      %% Functions

%       function self = reset(self)
%          % Reset times and windows to state when object was created
%          self = self.undoAlign();
%          
%          n = length(self);
%          for i = 1:n
%             self(i).window = self(i).window_;
%          end
%       end
      
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
         % For a full description of the possible parameters, see 
         % <a href="matlab:help('plotRaster')">plotRaster</a>

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
            %xlabel(['Time (' self.timeUnits ')']);
         end         
      end
      
      function [r,t,r_sem,count,reps] = getPsth(self,bw,varargin)
         % Get history-independent intensity representation
         % For a full description of the possible parameters, see 
         % <a href="matlab:help('getPsth')">getPsth</a>

         % TODO
         % When timeUnits functioning, need to reconcile units with bandwidth
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
      
      function self = transform(self)
         % Transformation like time-rescaling
         % should be a property transformFunction that contains a function
         % handle that accepts spikes and possibly other inputs
         % applied after windowing, so the most general form would be an
         % array of function handles, but this really seems excessive...
      end
      
      %% Operators
      function obj = plus(x,y)
         % Addition
         if isa(x,'pointProcess') && isa(y,'pointProcess')
            % not done yet
            % should merge the objects
         elseif isa(x,'pointProcess') && isnumeric(y)
            x.offset = y;
            obj = x;
         elseif isa(y,'pointProcess') && isnumeric(x)
            y.offset = x;
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
            x.offset = -y;
            obj = x;
         elseif isa(y,'pointProcess') && isnumeric(x)
            y.offset = -x;
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
            elseif any(x.marks ~= y.marks)
               bool = false;
               return;
            elseif x.tAbs ~= y.tAbs
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
   
   methods(Static, Access = public)
      function validOffset = checkOffset(offset,n)
         % TODO Validate sync, 
         if nargin == 1
            n = 1;
         end
         validOffset = checkOffset(offset,n);
      end
      
      function validWindow = checkWindow(window,n)
         if nargin == 1
            n = 1;
         end
         validWindow = checkWindow(window,n);
      end
   end
   
end

