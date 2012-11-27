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
% externalize getTimes, in fact, if we clean up alignTimes a bit, the
% method could just call this function. GetTimes is a misleading name,
% suggests that I am just returning times property. Should be
% getWindowedTimes or something more descriptive. Should allow sync, window
% inputs
%
% How to ensure we can always load the data? How to save? as struct and use
% savObj and loadObj methods
%   started by including version property
%
% what about multiple windows? should all the dependent properties become
% cell arrays??? that seems ugly. Does this mean that the only way to
% create a triggered PSTH is via collection object?
% A triggered analysis (multiple events) seems to be a basic requirement of
% pointProcess?
%
% One possibility is to enhance alignTimes to accept multiple window and
% sync inputs. Then properties like window, intervals, count, rate, etc
% would need to handle this. However, what if we also want the spike times
% within all these windows? currently we never see these, since the
% properties implicitly window the times and discard them. 
% Perhaps we need a property that transiently stores the windowed times?
% windowedTimes. This maybe even useful by itself, since all methods see
% things through the window anyways? Avoid calling getTimes?
%   If I do the above, should also be a method to "break" a multiply
%   windowed pointProcess into a collection?
%
% Another possibility is to define a method that handles multiple windows
% and returns a pointProcessCollection. This could get messy very quickly,
% for example, if there are huge numbers of spikes (reverse correlation),
% we are essentially replicating data. Handle class doesn't seem to solve
% this, because each element would have the same times but different
% windows. Maybe can get away using a dirty bit?
% Not sure the former method save much, for huge numbers of spikes, the
% dependent properties will spend a lot of time doing stuff? Move isis and
% counting process to be methods?
%
% Final possibility is to allow multiple windows, but only pass back the
% spike array in the method. That is, it doesn't alter the object. Then
% what's the point of the object???!!
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
      
      % If marked point process, vector of associated magnitudes
      marks
   end
   
   properties(GetAccess = public, SetAccess = public)
      % Time representation 
      timeUnits = 'seconds';

      % This is likely just for info, not useful to convert
      % perhaps unnecessary or nonsense if marks is itself an array of
      % objects?
      markUnits = 'none';
 
      % [min max] time window of interest
      window
   end

   % These dependent properties all apply the window property
   properties (GetAccess = public, SetAccess = private, Dependent = true, Transient = true)
      %
      windowedTimes
      %
      
%       % Interevent interval representation
%       intervals
      
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
   
   properties(GetAccess = private, SetAccess = private)
      % Time shift that returns times back to state when object was created
      % perhaps this should be called tRelShift
      tAbsShift
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
         self.window = self.checkWindow(window,size(window,1));
      end
     
      function self = setWindow(self,window)
         % Set the window property
         % window can be [1 x 2], where all objects are set to the same window
         % window can be [nObjs x 2], where each object window is set individually

         % Reset to default windows
         if nargin == 1
            self = self.setInclusiveWindow();
            return
         end

         n = length(self);
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
         n = length(self);
         
         if n == 1
            nWindow = size(window,1);
            window = self.checkWindow(window,nWindow);
            for i = 1:nWindow
               % TODO What about non-zero sync? This seems pretty useful
               windowedTimes(i,1) = alignTimes({self.times},'sync',0,...
                  'window',window(i,:),'alignWindow',true);
            end
         else
            
         end
      end
%       function windowedTimes = getTimes(self,window)
%          % Apply window to times
%          % Note that windowedTimes is a cell array
%          n = length(self);
%          keyboard
%          % These window changes will NOT be persistent (not copied into object)
%          if nargin == 2
%             window = self.checkWindow(window,n);
%          else
%             window = self.checkWindow(cat(1,self.window),n);
%          end
% 
%          for i = 1:n
%             % TODO call alignTimes here? What is the advantage? error-checking?
%             % extra parameters??? Just consistency? What about non-zero
%             % sync? This seems pretty useful
% %            ind = (self(i).times>=window(i,1)) & (self(i).times<=window(i,2));
% %            windowedTimes{i,1} = self(i).times(ind);
%              windowedTimes(i,1) = alignTimes({self(i).times},'sync',0,...
%                 'window',window(i,:),'alignWindow',true);
%          end
%          
%          if isrow(self)
%             windowedTimes = windowedTimes';
%          end         
%       end
      
      function windowedTimes = get.windowedTimes(self)
         windowedTimes = getTimes(self,self.window);
      end
      
%       function intervals = get.intervals(self)
%          % Interevent interval representation
% %         times = getTimes(self,self.window);
% %         intervals = diff(times{1});
%          
%          times = self.windowedTimes;
%          if iscell(times)
%             for i = 1:length(times)
%                intervals{i,1} = diff(times{i});
%             end
%          else
%             intervals = diff(times);
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
         % # of events within window
%          if any(isnan(self.window))
%             count = 0;
%          else
%             times = getTimes(self,self.window);
%             count = length(times{1});
%          end
         times = self.windowedTimes;
         for i = 1:length(times)
            count(i,1) = length(times{i});
         end
      end
      
      function rate = get.rate(self)
         % # of events within window
%          if any(isnan(self.window))
%             rate = 0;
%          else
%             times = getTimes(self,self.window);
%             rate = length(times{1}) / (self.window(2)-self.window(1));
%          end
         times = self.windowedTimes;
         for i = 1:length(times)
            rate(i,1) = length(times{i}) / (self.window(2,i)-self.window(1,i));
         end
      end

      function minTime = get.minTime(self)
%          % Minimum event time within window
%          if any(isnan(self.window))
%             minTime = NaN;
%          else
%             times = getTimes(self,self.window);
%             minTime = min(times{1});
%          end
         times = self.windowedTimes;
         for i = 1:length(times)
            minTime(i,1) = min(times{i});
         end
      end
      
      function maxTime = get.maxTime(self)
         % Maximum event time within window
%          if any(isnan(self.window))
%             maxTime = NaN;
%          else
%             times = getTimes(self,self.window);
%             maxTime = max(times{1});
%          end
         times = self.windowedTimes;
         for i = 1:length(times)
            maxTime(i,1) = max(times{i});
         end
     end

      %% Functions
      function self = align(self,sync,varargin)
         % Align event times
         % sync can be a scalar, where it is applied to all objects
         % sync can be [nObjs x 1], where each object is aligned individually
         % NaN elements in sync skipped
         % The window property is also aligned
         % For a full description, see 
         % <a href="matlab:help('alignTimes')">alignTimes</a>
         
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
               [tempTimes,tempWindow] = alignTimes({self(i).times},'sync',sync(i),...
                  'window',[min(self(i).times) max(self(i).times)],'alignWindow',true);
               self(i).times = tempTimes{1};
               self(i).window = tempWindow;
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
         % For a full description of the possible parameters, see 
         % <a href="matlab:help('plotRaster')">plotRaster</a>

         n = length(self);
         if n < 10
           ms = 6;
         else
           ms = 3;
         end
         p = inputParser;
         p.KeepUnmatched= true;
         p.FunctionName = 'pointProcess raster method';
         % Intercept some parameters to override defaults
         p.addParamValue('markerSize',ms,@isnumeric);
         p.addParamValue('grpBorder',false,@islogical);
         p.addParamValue('labelXAxis',false,@islogical);
         p.addParamValue('labelYAxis',false,@islogical);
         p.parse(varargin{:});
         % Passed through to plotRaster
         params = p.Unmatched;
         
         % These window changes will NOT be persistent (not copied into object)
         if isfield(params,'window')
            window = self.checkWindow(params.window,n);
         else
            window = self.checkWindow(cat(1,self.window),n);
         end
         
         times = getTimes(self,window);
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
      
      function self = rescale(self)
         % Time rescale
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
            % should delete the common times from object
         elseif isa(x,'pointProcess') && isnumeric(y)
            obj = align(x,y);
         elseif isa(y,'pointProcess') && isnumeric(x)
            obj = align(y,x);
         else
            error('Minus not defined for inputs');
         end
      end
   
      function bool = eq(x,y)
         % Equality (==, isequal)
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
   
   methods(Static, Access = public)
      function validOffset = checkOffset()
         % Validate sync, 
      end
      
      function validWindow = checkWindow(window,n)
         if nargin == 1
            n = 1;
         end
         validWindow = checkWindow(window,n);
      end
   end
   
end

