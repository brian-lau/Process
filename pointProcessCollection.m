% pointprocess collection

% What defines a collection? A good definition is that all elements of the collection
% Have the same timebase (eg., a trial), so that all operations acting on these
% elements can assume this. Eg., when defining a method align, we could try to acount
% for the possibility that each element has a different sync input. This seems very
% messy. Rather, if we assume the same time base, then generally we want to sync
% every element in a collection to the same event

% Design goals
%  should provide translation to
%   1) nStat toolbox
%

% locations should probably be a cell array in case one wants to specify
% non-numeric labels
% the reason it is separate from names is to provide for the possibility of
% multiple data from the same location
%

% simulation
% align - given a value, shift times relative to this
% psth
% reset (reset spike times back to original data)
%       origspikeTimes = spikeTimes + tAbs

% NEED VALIDATOR FOR UNIQUE NAMES?? or should we be able to put many
% instances of the same name? eg., trials into collection, there is no
% sense of ordering here?

% force everything to be a row or column
% probably need to verify that names are all of the same type (strs or #)
classdef pointProcessCollection
%
   properties(GetAccess = public, SetAccess = private)
      % Cell array of names
      names
      
      % Array of pointProcess objects
      array
      
      % Array of location information
      locations
      
      % Labels for the location information
      locDimNames
      
      % Boolean mask
      mask
   end
   
   % These dependent properties all apply the mask property
   properties (GetAccess = public, SetAccess = private, Dependent)
      % Minimum event time within window
      minTime
 
      % Maximum event time within window
      maxTime
   end

   properties(GetAccess = private, SetAccess = private)
      % Absolute time that all objects in collection are referenced to
      tAbs;
   end
   
   methods
      %% Constructor
      function self = pointProcessCollection(varargin)
         
         % allow array to be passed in without name
         if (nargin>1) && isa(varargin{1},'pointProcess')
            self = pointProcessCollection('array',varargin{1},varargin{2:end});
            return;
         end
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'pointProcessCollection constructor';
         p.addParamValue('array',pointProcess,@(x)isa(x,'pointProcess')); % NEED VALIDATOR
         p.addParamValue('names',[],@iscell); % NEED VALIDATOR
         p.addParamValue('locations',[],@isnumeric); % NEED VALIDATOR
         p.addParamValue('locDimNames',[],@iscell); % NEED VALIDATOR
         p.addParamValue('mask',[],@islogical); % NEED VALIDATOR
         p.parse(varargin{:});

         if nargin == 0
            self.array = pointProcess;
         else
            self.array = p.Results.array(:)';
         end
         %[m,n] = size(p.Results.array);
         n = length(p.Results.array);
         if isempty(p.Results.names)
            self.names = cell(1,n);
         else
            if length(p.Results.names) == n
               self.names = p.Results.names;
            else
               error('Bad names size');
            end
         end
         if isempty(p.Results.locations)
            self.locations = cell(1,n);
         else
            if size(p.Results.locations,2) == n
               self.locations = p.Results.locations;
            else
               error('Bad locations size');
            end
         end
         m = size(self.locations,1);
         if isempty(p.Results.locDimNames)
            self.locDimNames = cell(m,1);
         else
            if sum(size(p.Results.locDimNames)==[m 1]) == 2
               self.locDimNames = p.Results.locDimNames;
            else
               error('Bad locDimNames size');
            end
         end
         if isempty(p.Results.mask)
            self.mask = true(1,n);
         else
            if length(p.Results.mask) == n
               self.mask = p.Results.mask;
            else
               error('Bad mask size');
            end
         end
      end
      
      %% Set functions
      % Set the window property
      % window can be [1 x 2], where all objects are set to the same window
      % window can be [nObjs x 2], where each object window is set individually
      function self = setWindow(self,window)
         n = length(self);

         if nargin == 1
            for i = 1:n
               self(i).array = self(i).array.setWindow();
            end
            return;
         end
         
         % check window dimensions
         if numel(window) == 2
            window = window(:)';
            window = repmat(window,n,1);
         end
         if size(window,1) ~= n
            error('window must be [1 x 2] or [nObjs x 2]');
         end
         
         for i = 1:n
            self(i).array = self(i).array.setWindow(window(i,:));
         end
      end

      %% Get Functions
      function minTime = get.minTime(self)
         % mask
         minTime = min(cat(1,self.array.minTime));
      end
      
      function maxTime = get.maxTime(self)
         maxTime = max(cat(1,self.array.minTime));
      end

      %% Functions
      % Align event times
      % sync can be a scalar, where it is applied to all objects
      % sync can be [nObjs x 1], where each object is aligned individually
      function self = align(self,sync,varargin)
         n = length(self);
         
         % check sync dimensions
         if numel(sync) == 1
            sync = repmat(sync,n,1);
         elseif ~(numel(sync)==n)
            error('Sync must have length 1 or numel(obj)');
         end
         
         for i = 1:n
            self(i).array = self(i).array.align(sync(i));
         end
      end
      
      % Return pointProcess objects to state when they were created
      function self = reset(self)
         n = length(self);
         for i = 1:n
            self(i).array = self(i).array.reset();
         end
      end
      
      %
      function [r,t,r_sem,count,reps] = getPsth(self,bw,varargin)
         % Input can be vector of pointProcessCollections, so we concatonate
         array = cat(2,self.array);
         [grp,ind] = self.getGrpInd(cat(2,self.names),cat(2,self.mask));
         
         %keyboard
         % Since this loops, each call to getPsth will use a default window
         % need a window to return these as matrix, otherwise cell array?
         % we should
         %  default to window including all event times passing mask
         %  these will naturally all be seen through the individual
         %  pointProcess windows
         %  
         %  passing in explicit window works as well, but this is applied
         %  on top of any windows in the individual pointProcess windows
         %  we should call setWindow() first!
         %  What's the cleanest way to do this while still allowing all the
         %  varargins to go through? inputParser...
         %
         %  if we try to enforce that r is a matrix, how can we pass back
         %  reps?
         %
         %  another approach would be to extract a cell array in the old
         %  format expected by getPsth?
         %
         %  yet another approach is to define a method for pointProcess,
         %  and then call that? like raster method?
         count = 1;
         for i = find(grp)
            [r(:,count),t,r_sem(:,count)] = getPsth({array(ind{i}).times}',bw,varargin{:});
            count = count + 1;
         end
         
      end
      
      % Alias to raster until I think of better plot override
      function [h,yOffset] = plot(self,varargin)
         [h,yOffset] = raster(self,varargin{:});
      end
      
      % Inputs can be any of those accepted by plotRaster, except
      % 
      % Like getPsth method, passing in a window goes through to raster
      % However, each object in collection has its own window
      % should inputParse and apply explicitly set window. Perhaps it is
      % best to do this in the pointProcess method (do the same for
      % getPsth)
      function [h,yOffset] = raster(self,varargin)
         % TODO
         % intercept handle and yOffset
         % how to handle colors? perhaps intercept?
         % Input can be vector of pointProcessCollections, so we concatonate
         array = cat(2,self.array);
         [grp,ind] = self.getGrpInd(cat(2,self.names),cat(2,self.mask));
         
         c = distinguishable_colors(sum(grp));
         
         count = 1; h = NaN; yOffset = 1;
         for i = find(grp)
            [h,yOffset] = array(ind{i}).raster('handle',h,'yOffset',...
               yOffset,'grpColor',c(count,:),varargin{:});
            count = count + 1;
         end
      end
   end
   
   methods(Static, Access = private)
      % Return index into all elements of collection that pass mask
      % names : cell array of names from collection
      % mask  : corresponding boolean mask from collection
      %
      % grp   : boolean indicating whether array object contains data
      % ind   : cell array with the corresponding indices
      %
      % TODO
      % error checking and boundary conditions
      % need to modify of uniqueness is defined by names & locations
      function [grp,ind] = getGrpInd(names,mask)
         % Check dimensions
         uNames = unique(names);
         for i = 1:length(uNames)
            ind{i} = find(strcmp(names(mask),uNames{i}));
            if ~isempty(ind{i})
               grp(i) = true;
            end
         end
      end
   end
end