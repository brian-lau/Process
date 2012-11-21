% point process class
% simulation
% align - given a value, shift times relative to this
% reset (reset spike times back to original data)
%       origspikeTimes = spikeTimes + tAbs
% psth
% analyses
%  isi, cv, cv2, lvr
% conditional mean
% hazard
%
% append - add pointProcess to array

classdef pointProcess
%
   properties(GetAccess = public, SetAccess = private)
      % vector of event times
      times;
      
      % if marked point process, vector of associated magnitudes
      marks;
   end
   
   properties(GetAccess = public, SetAccess = public)
      % TODO
      % specify the units? methods for converting
      % valid units metric seconds (eg, milli, nano, )
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
      % time shift that returns times back to state when object was created
      % perhaps this should be called tRelShift
      tAbsShift;
   end
   
   methods
      %% Constructor
      function self = pointProcess(varargin)
         % if spikeTimes is cell array? make an object array
         % currently allocating object array with a single time vector puts
         % it in the last element
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'pointProcess constructor';
         p.addParamValue('times',NaN,@isnumeric);
         p.addParamValue('marks',[],@isnumeric); % NEED VALIDATOR
         p.addParamValue('name','',@ischar); % NEED VALIDATOR
         p.addParamValue('dt',0.001,@(x)(x>0)); % NEED VALIDATOR
         p.addParamValue('window',[],@isnumeric); % NEED VALIDATOR
         p.parse(varargin{:});
                  
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
         self.tAbsShift = 0;
      end
      
      %% Set functions
      %% Set the window property
      % Redundant, maybe useful to keep for error-checking public setting?
      % This method does not work for vector inputs, see setWindow()
      function self = set.window(self,window)
         if numel(window) ~= 2
            error('window must be a 2-element vector');
         end
         if window(1) > window(2)
            error('First element of window must be less than second');
         end
         self.window = window;
      end
      
      %% Set the window property
      % window can be [1 x 2], where all objects are set to the same window
      % window can be [nObjs x 2], where each object window is set individually
      function self = setWindow(self,window)
         n = length(self);

         % Reset to default windows
         if nargin == 1
            self = self.resetWindow();
            return
         end

         window = self.checkWindow(window,n);         
         for i = 1:n
            self(i).window = window(i,:);
         end         
      end
      
      %% Reset to default windows
      function self = resetWindow(self)
         n = length(self);
         for i = 1:n
            self(i).window = [min(self(i).times) max(self(i).times)];
         end
      end
      
      %% Get Functions
      %% Apply window to times
      % Note that windowedTimes is a cell array
      function windowedTimes = getTimes(self,window)
         n = length(self);
         
         % Validate if window is passed in
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
      
      %% Interevent interval representation
      function intervals = get.intervals(self)
         times = getTimes(self,self.window);
         intervals = diff(times{1});
      end
      
      %% Counting process representation
      function countingProcess = get.countingProcess(self)
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
      
      %% # of events within window
      function count = get.count(self)
         if any(isnan(self.window))
            count = 0;
         else
            times = getTimes(self,self.window);
            count = length(times{1});
         end
      end
      
      %% Minimum event time within window
      function minTime = get.minTime(self)
         if any(isnan(self.window))
            minTime = NaN;
         else
            times = getTimes(self,self.window);
            minTime = min(times{1});
         end
      end
      
      %% Maximum event time within window
      function maxTime = get.maxTime(self)
         if any(isnan(self.window))
            maxTime = NaN;
         else
            times = getTimes(self,self.window);
            maxTime = max(times{1});
         end
      end

      %% Functions
      %% Align event times
      % sync can be a scalar, where it is applied to all objects
      % sync can be [nObjs x 1], where each object is aligned individually
      % NaN elements in sync skipped
      % currently this resets the windows as well. Change?
      function self = align(self,sync,varargin)
         % Automatically reset
         self = self.reset();
         
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
               %self(i).window = tempWindow; % reset windows
               self(i).tAbsShift = sync(i);
            end
         end
      end
      
      %% Return times and windows to state when object was created
      function self = reset(self)
         n = length(self);
         for i = 1:n
            self(i).times = self(i).times + self(i).tAbsShift;
            self(i).window = self(i).window + self(i).tAbsShift;
            self(i).tAbsShift = 0;
         end        
      end
      
      %% Plot times & counting process
      function h = plot(self,varargin)
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
            'labelYaxis',false,varargin{:});
         stairs(countingProcess(:,1),countingProcess(:,2));

         axis tight;
         ylabel('N_t');
      end
      
      %% Raster plot
      function [h,yOffset] = raster(self,varargin)
         % Intercept window parameter
         p = inputParser;
         p.KeepUnmatched= true;
         p.FunctionName = 'pointProcess raster method';
         p.parse(varargin{:});
         params = p.Unmatched; % passed through to plotRaster
         
         n = length(self);

         % Validate if window is passed in
         % These window changes will NOT be persistent (not copied into object)
         if isfield(params,'window')
            window = self.checkWindow(params.window,n);
         else
            window = self.checkWindow(cat(1,self.window),n);
         end

         times = getTimes(self,window);
         [h,yOffset] = plotRaster(times,params);
      end
      
      %% Get intensity representation
      function [r,t,r_sem,count,reps] = getPsth(self,bw,varargin)
         % Intercept window parameter
         p = inputParser;
         p.KeepUnmatched= true;
         p.FunctionName = 'pointProcess raster method';
         p.addRequired('bw', @isnumeric);
         p.parse(bw,varargin{:});
         params = p.Unmatched; % passed through to getPsth
         
         n = length(self);

         % Validate if window is passed in
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
      %% Addition
      function obj = plus(x,y)
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
      
      %% Subtraction
      function obj = minus(x,y)
         if isa(x,'pointProcess') && isa(y,'pointProcess')
            % not done yet
            % should delete the times from object
         elseif isa(x,'pointProcess') && isnumeric(y)
            obj = align(x,-y);
         elseif isa(y,'pointProcess') && isnumeric(x)
            obj = align(y,-x);
         else
            error('Minus not defined for inputs');
         end
      end
   
   end
   
   methods(Static, Access = private)
      %% Validate window, and replicate if necessary
      function validWindow = checkWindow(window,n)
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

