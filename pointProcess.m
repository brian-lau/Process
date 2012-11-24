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
classdef pointProcess
%
   properties(GetAccess = public, SetAccess = private)
      % String identifier
      name
      
      % container.Map with information about process (requires Matlab 2008b)
      info
            
      % Vector of event times
      times
      
      % If marked point process, vector of associated magnitudes
      marks
   end
   
   properties(GetAccess = public, SetAccess = public)
      % Time representation 
      timeUnits = 'seconds';

      % This is likely just for info, not useful to convert
      markUnits = 'none';
 
      % [min max] time window of interest
      window
   end
   
   % These dependent properties all apply the window property
   properties (GetAccess = public, SetAccess = private, Dependent)
      % Interevent interval representation
      intervals
      
      % Counting process representation
      countingProcess
      
      % # of events within window
      count
      
      % count/window Hz?
      rate 
      
      % Minimum event time within window
      minTime
      
      % Minimum event time within window
      maxTime
   end
   
   properties(GetAccess = private, SetAccess = private)
      % Time shift that returns times back to state when object was created
      % perhaps this should be called tRelShift
      tAbsShift
   end
   
   properties(GetAccess = public, SetAccess = immutable)
      % Time that event times are relative to when object is constructed
      tAbs
   end
      
   properties(GetAccess = private, SetAccess = immutable)
      % Original [min max] time window of interest
      window_
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
         p.addParamValue('marks',[],@isnumeric); % NEED VALIDATOR
         p.addParamValue('window',[],@isnumeric); % NEED VALIDATOR
         p.addParamValue('tAbs',0,@isnumeric);
         p.parse(varargin{:});
         
         self.name = p.Results.name;
         self.times = sort(p.Results.times(:));
         
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
            else
               self.window = self.checkWindow(p.Results.window);
            end
         end
         
         % Tuck away the original window for resetting
         self.window_ = self.window;
         
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
         
         self.tAbsShift = 0;
      end
      
      %% Set functions
      function self = set.window(self,window)
         % Convenience method for setting the window property
         % Useful for error-checking public setting
         % Does not work for vector inputs, see setWindow()
         self.window = self.checkWindow(window);
      end
     
      function self = setWindow(self,window)
         % Set the window property
         % window can be [1 x 2], where all objects are set to the same window
         % window can be [nObjs x 2], where each object window is set individually
         n = length(self);

         % Reset to default windows
         if nargin == 1
            self = self.setInclusiveWindow();
            return
         end

         window = self.checkWindow(window,n);         
         for i = 1:n
            self(i).window = window(i,:);
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
      function windowedTimes = getTimes(self,window)
         % Apply window to times
         % Note that windowedTimes is a cell array
         n = length(self);
         
         % These window changes will NOT be persistent (not copied into object)
         if nargin == 2
            window = self.checkWindow(window,n);
         else
            window = self.checkWindow(cat(1,self.window),n);
         end
         
         for i = 1:n
            ind = (self(i).times>=window(i,1)) & (self(i).times<=window(i,2));
            windowedTimes{i,1} = self(i).times(ind);
         end
         
         if isrow(self)
            windowedTimes = windowedTimes';
         end
         
      end
      
      function intervals = get.intervals(self)
         % Interevent interval representation
         times = getTimes(self,self.window);
         intervals = diff(times{1});
      end
      
      function countingProcess = get.countingProcess(self)
         % Counting process representation
         if any(isnan(self.window))
            countingProcess = [NaN NaN];
         else
            window = self.window;
            times = getTimes(self,window);
            times = times{1};
            count = cumsum(ones(size(times)));
            tStart = max(-inf,unique(min(times)));
            countingProcess = [[tStart;times] , [0;count]];
         end
      end
      
      function count = get.count(self)
         % # of events within window
         if any(isnan(self.window))
            count = 0;
         else
            times = getTimes(self,self.window);
            count = length(times{1});
         end
      end
      
      function rate = get.rate(self)
         % # of events within window
         if any(isnan(self.window))
            rate = 0;
         else
            times = getTimes(self,self.window);
            rate = length(times{1}) / (self.window(2)-self.window(1));
         end
      end

      function minTime = get.minTime(self)
         % Minimum event time within window
         if any(isnan(self.window))
            minTime = NaN;
         else
            times = getTimes(self,self.window);
            minTime = min(times{1});
         end
      end
      
      function maxTime = get.maxTime(self)
         % Maximum event time within window
         if any(isnan(self.window))
            maxTime = NaN;
         else
            times = getTimes(self,self.window);
            maxTime = max(times{1});
         end
      end

      %% Functions
      function self = align(self,sync,varargin)
         % Align event times
         % sync can be a scalar, where it is applied to all objects
         % sync can be [nObjs x 1], where each object is aligned individually
         % NaN elements in sync skipped
         % The window property is also aligned
         
         % Automatically reset
         self = self.undoAlign();

         n = length(self);
         % Check sync dimension
         if numel(sync) == 1
            sync = repmat(sync,n,1);
         elseif ~(numel(sync)==n)
            error('Sync must have length 1 or nObj');
         end
         
         for i = 1:n
            if ~isnan(sync(i))
               % Always take all events to ensure we can recontruct original times
               [tempTimes,tempWindow] = alignTimes({self(i).times},sync(i),...
                  'window',[min(self(i).times) max(self(i).times)]);
               self(i).times = tempTimes{1};
               self(i).window = self(i).window - sync(i);
               %self(i).window = tempWindow; % same as setInclusiveWindow
               self(i).tAbsShift = sync(i);
            end
         end
      end
      
      function self = undoAlign(self)
         % Undo align
         n = length(self);
         for i = 1:n
            self(i).times = self(i).times + self(i).tAbsShift;
            self(i).window = self(i).window + self(i).tAbsShift;
            self(i).tAbsShift = 0;
         end
      end
      
      function self = reset(self)
         % Reset times and windows to state when object was created
         self = self.undoAlign();
         
         n = length(self);
         for i = 1:n
            self(i).window = self(i).window_;
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
         % For a full description of the possible parameters, see the help
         % for <a href="matlab:help('plotRaster')">plotRaster</a>

         % Intercept window parameter
         n = length(self);
         if n < 10
            ms = 6;
         else
            ms = 3;
         end
         p = inputParser;
         p.KeepUnmatched= true;
         p.FunctionName = 'pointProcess raster method';
         p.addParamValue('markerSize',ms,@isnumeric);
         p.addParamValue('grpBorder',false,@islogical);
         p.addParamValue('labelXAxis',false,@islogical);
         p.addParamValue('labelYAxis',false,@islogical);
         p.parse(varargin{:});
         params = p.Unmatched; % passed through to plotRaster
         
         % These window changes will NOT be persistent (not copied into object)
         if isfield(params,'window')
            window = self.checkWindow(params.window,n);
         else
            window = self.checkWindow(cat(1,self.window),n);
         end
         
         times = getTimes(self,window);
         if isempty(times)
            % need to return handle and yOffset if they exist? TODO
         end
         
         [h,yOffset] = plotRaster(times,p.Results,params);
         xlabel('Time');
         %xlabel(['Time (' self.timeUnits ')']);
      end
      
      function [r,t,r_sem,count,reps] = getPsth(self,bw,varargin)
         % Get intensity representation
         % When timeUnits functioning, need to reconcile units with bandwidth
         % here
         
         % Intercept window parameter
         p = inputParser;
         p.KeepUnmatched= true;
         p.FunctionName = 'pointProcess psth method';
         p.addRequired('bw', @isnumeric);
         p.parse(bw,varargin{:});
         params = p.Unmatched; % passed through to getPsth
         
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
         elseif isa(x,'pointProcess') && isnumeric(y)
            obj = align(x,-y);
         elseif isa(y,'pointProcess') && isnumeric(x)
            obj = align(y,-x);
         else
            error('Plus not defined for inputs');
         end
      end
      
      function obj = minus(x,y)
         % Subtraction
         if isa(x,'pointProcess') && isa(y,'pointProcess')
            % not done yet
            % should delete the times from object
         elseif isa(x,'pointProcess') && isnumeric(y)
            obj = align(x,y);
         elseif isa(y,'pointProcess') && isnumeric(x)
            obj = align(y,x);
         else
            error('Minus not defined for inputs');
         end
      end
   
      function bool = eq(x,y)
         % Equality
         % TODO
         % check units ?
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
   
   methods(Static, Access = private)
      function validWindow = checkWindow(window,n)
         % Validate window, and replicate if necessary
         if nargin == 1
            n = 1;
         end
         
         if numel(window) == 2
            window = window(:)';
            window = repmat(window,n,1);
         end
         if size(window,1) ~= n
            error('window must be [1 x 2] or [nObjs x 2]');
         end
         if any(window(:,1)>window(:,2))
            error('First element of window must be less than second');
         end
         
         validWindow = window;
      end
   end
   
end

