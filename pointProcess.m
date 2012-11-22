% point process class
% simulation
%
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

% probably need methods to get and set info & infoLabels?

classdef pointProcess
%
   properties(GetAccess = public, SetAccess = private)
      name;
      
      info;
      
      infoLabels;
      
      % Vector of event times
      times;
      
      % If marked point process, vector of associated magnitudes
      marks;
   end
   
   properties(GetAccess = public, SetAccess = public)
      % Time representation 
      timeUnits = 'seconds';

      % This is likely just for info, not useful to convert
      markUnits = 'none';
 
      % [min max] time window of interest
      window;
   end
   
   % These dependent properties all apply the window property
   properties (GetAccess = public, SetAccess = private, Dependent)
      % interevent interval representation
      intervals;
      
      % counting process representation
      countingProcess;
      
      % # of events within window
      count;
      
      % minimum event time within window
      minTime;
      
      % minimum event time within window
      maxTime;
   end
   
   properties(GetAccess = private, SetAccess = private)
      % Time shift that returns times back to state when object was created
      % perhaps this should be called tRelShift
      tAbsShift;
   end
      
   properties(GetAccess = private, SetAccess = immutable)
      % Time that event times are relative to when object is constructed
      tAbs;
      % Original [min max] time window of interest
      window_;
   end
   
   methods
      %% Constructor
      function self = pointProcess(varargin)
         % Constructor, arguments are taken as name/value pairs
         % name       - string identifier
         % info       - cell array of information about process
         % infoLabels - cell array of strings labelling info elements
         % times      - Vector of event times
         % marks      - Corresponding vector of magnitudes for "marked" process
         % window     - Defaults to window that includes all event times,
         %              If a smaller window is passed in, event times outside
         %              the window will be DISCARDED.
         % tAbs       - Time that event times are relative
         
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'pointProcess constructor';
         p.addParamValue('name',datestr(now,'yyyy-mm-dd HH:MM:SS:FFF'),@ischar);
         p.addParamValue('info',[],@iscell);
         p.addParamValue('infoLabels',[],@iscell);
         p.addParamValue('times',NaN,@isnumeric);
         p.addParamValue('marks',[],@isnumeric); % NEED VALIDATOR
         p.addParamValue('window',[],@isnumeric); % NEED VALIDATOR
         p.addParamValue('tAbs',0,@isnumeric);
         p.parse(varargin{:});
         
         self.name = p.Results.name;
         
         if ~isempty(p.Results.info)
            self.info = p.Results.info(:);
         else
            self.info = {};
         end
         
         if ~isempty(p.Results.infoLabels)
            infoLabels = p.Results.infoLabels(:);
            if length(infoLabels) == length(self.info)
               self.infoLabels = infoLabels;
            else
               error('Dimensions of infoLabels must match info');
            end
         else
            if isempty(p.Results.info)
               self.infoLabels = {};
            else
               for i = 1:length(self.info)
                  self.infoLabels{i,1} = ['dim' num2str(i)];
               end
            end
         end
         
         self.times = sort(p.Results.times(:));
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
               self.window = p.Results.window;
            end
         end
         self.window_ = self.window;
         
         % Window the event times and corresponding marks
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
         % Set the window property
         % Useful for error-checking public setting
         % This method does not work for vector inputs, see setWindow()
         if numel(window) ~= 2
            error('window must be a 2-element vector');
         end
         if window(1) > window(2)
            error('First element of window must be less than second');
         end
         self.window = window;
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
         % Set windows to earliest and latest event
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
               [tempTimes,tempWindow] = alignSpkTimes({self(i).times},sync(i),...
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
         xlabel(['Time (' self.timeUnits ')']);
         ylabel('N_t');
      end
      
      function [h,yOffset] = raster(self,varargin)
         % Raster plot
         % For a full description of the possible parameters, see the help
         % for plotRaster

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
            % need to return handle and yOffset if they exist?
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
         p.FunctionName = 'pointProcess raster method';
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

