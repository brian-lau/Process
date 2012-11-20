% pointprocess collection

% spatial location 
%

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
      % interspike interval representation
      isis;
      
      % counting process representation
      countingProcess;
      
      % minimum event time within window
      minTime
      
      % minimum event time within window
      maxTime
   end
   
   properties(GetAccess = private, SetAccess = private)
      % time shift needed to bring spikeTimes back to original
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
      function self = set.window(self,window)
         [m,n] = size(window);
         if (m*n) ~= 2
            error('window must be a 2-element vector');
         end
         if window(1) > window(2)
            error('First element of window must be less than second');
         end
         self.window = window;
      end
      
      %% Get Functions
      function windowedTimes = getTimes(self,window)
         if nargin < 2
            window = self.window;
         end
         ind = (self.times>=window(1)) & (self.times<=window(2));
         windowedTimes = self.times(ind);
      end
      
      function isis = get.isis(self)
         isis = diff(getTimes(self,self.window));
      end
      
      function countingProcess = get.countingProcess(self)
         window = self.window;
         times = getTimes(self,window);
         count = cumsum(ones(size(times)));
         tStart = max(-inf,unique(min(times)));
         countingProcess = [[tStart;times] , [0;count]];
      end
      
      function minTime = get.minTime(self)
         minTime = min(getTimes(self,self.window));
      end
      
      function maxTime = get.maxTime(self)
         maxTime = max(getTimes(self,self.window));
      end

      %% Functions
      %
      function self = align(self,sync,varargin)
         % automatically call reset
         %self = self(:);
%          n = length(self);
%          for i = 1:n
%             times{i,1} = getTimes(self(i),self(i).window);
%          end
%          window = cat(1,self.window);
%          
        % keyboard
         % check sync 
         
         %[alignedTimes,alignedWindow] = alignSpkTimes(times,sync,'window',window);         
         
         n = length(self);
         for i = 1:n
            [tempTimes,tempWindow] = alignSpkTimes({self(i).times},sync(i),...
               'window',[min(self(i).times) max(self(i).times)]);
            self(i).times = tempTimes{1};
            self(i).window = tempWindow;
            self(i).tAbsShift = sync(i);
         end
      end
      
      %
      function self = reset(self)
         n = length(self);
         for i = 1:n
            self(i).times = self(i).times + self(i).tAbsShift;
            self(i).window = self(i).window + self(i).tAbsShift;
            self(i).tAbsShift = 0;
         end        
      end
      
      % Plot times & counting process
      function [h,yOffset] = plot(self,varargin)
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
         self = self(:);
         n = length(self);
         for i = 1:n
            times{i,1} = getTimes(self(i),self(i).window);
         end
         [h,yOffset] = plotRaster(times,varargin{:});
      end
   end
   
end