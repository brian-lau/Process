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
      
      % if marked point process, associated magnitudes
      marks;
   end
   
   properties(GetAccess = public, SetAccess = public)
      % specify the units?
      timeUnits;
      markUnits;
 
      % [min max] time window of interest
      window;
   end
   
   % These dependent properties all apply the window property
   properties (GetAccess = public, SetAccess = private, Dependent)
      % interevent interval representation
      isis;
      
      % counting process representation
      countingProcess;
      
      % minimum event time within window
      minTime
      
      % minimum event time within window
      maxTime
   end
   
   properties(GetAccess = private, SetAccess = private)
      % time shift that returns times back to state when object was created
      tAbsShift;
   end
   
   methods
      %% Constructor
      function self = pointProcess(varargin)
         % if spikeTimes is cell array? make an object array
         % currently allocating object array with a single time vector puts
         % it in the last element
         % also contruct using isis or counting process? won't work now
         % since these are dependent...
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
      % Set the window property
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
      
      % Set the window property
      % window can be [1 x 2], where all objects are set to the same window
      % window can be [nObjs x 2], where each object window is set individually
      function self = setWindow(self,window)
         n = length(self);

         % Reset to default windows
         if nargin == 1
            for i = 1:n
               self(i).window = [min(self(i).times) max(self(i).times)];
            end
            return
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
         for i = 1:n
            self(i).window = window(i,:);
         end         
      end
      
      %% Get Functions
      % Apply window to times
      function windowedTimes = getTimes(self,window)
         % TODO
         % vector input
         if nargin < 2
            window = self.window;
         end
         ind = (self.times>=window(1)) & (self.times<=window(2));
         windowedTimes = self.times(ind);
      end
      
      % Interevent interval representation
      function isis = get.isis(self)
         isis = diff(getTimes(self,self.window));
      end
      
      % Counting process representation
      function countingProcess = get.countingProcess(self)
         window = self.window;
         times = getTimes(self,window);
         count = cumsum(ones(size(times)));
         tStart = max(-inf,unique(min(times)));
         countingProcess = [[tStart;times] , [0;count]];
      end
      
      % Minimum event time within window
      function minTime = get.minTime(self)
         minTime = min(getTimes(self,self.window));
      end
      
      % Maximum event time within window
      function maxTime = get.maxTime(self)
         maxTime = max(getTimes(self,self.window));
      end

      %% Functions
      % Align event times
      % sync can be a scalar, where it is applied to all objects
      % sync can be [nObjs x 1], where each object is aligned individually
      % NaN elements in sync skipped
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
               self(i).window = tempWindow;
               self(i).tAbsShift = sync(i);
            end
         end
      end
      
      % Return times and windows to state when object was created
      function self = reset(self)
         n = length(self);
         for i = 1:n
            self(i).times = self(i).times + self(i).tAbsShift;
            self(i).window = self(i).window + self(i).tAbsShift;
            self(i).tAbsShift = 0;
         end        
      end
      
      % Plot times & counting process
      function h = plot(self,varargin)
         % TODO 
         % vector input
         times = getTimes(self,self.window);
         if isempty(times)
            fprintf('No times in window.\n');
            return;
         end
         countingProcess = self.countingProcess;

         h = plotRaster({times},'grpBorder',false,...
            'window',self.window,'yOffset',0,'markerStyle','x','markersize',6,...
            'labelYaxis',false,varargin{:});
         stairs(countingProcess(:,1),countingProcess(:,2));

         axis tight;
         ylabel('N_t');
      end
      
      % Raster plot
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
            window = params.window;
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
         else
            window = cat(1,self.window);
         end

         for i = 1:n
            times{i,1} = getTimes(self(i),window(i,:));
         end
         
         [h,yOffset] = plotRaster(times,params);
      end
      
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
            window = params.window;
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
         else
            window = cat(1,self.window);
         end

         for i = 1:n
            times{i,1} = getTimes(self(i),window(i,:));
         end
         
         [r,t,r_sem,count,reps] = getPsth(times,p.Results.bw,params);
      end
      
      %% Operators
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
   
end