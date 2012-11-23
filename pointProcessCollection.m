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

% when we have arrays of collections
% we want to be able to :
% get psth with ability to mask names and array elements (trials)
% get event counts in the same manner
%
% do all of the above after aligning
% do all of the above in specific windows applied to all trials
% do all of the above in windows applied to each trial

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

% SHOULD figure out how to pass in mask as a parameter. maybe the case that
% you can't do it when there is more than one unique tAbs. The reason is I
% would like to store an immutible mask_ so that I can restore it to when
% the object was created?
%
% Since an array of Collections is constructed ordered in time, we will
% need methods to add (insert) and delete collections
%
% Need general methods:
% setMaskByName
% setMaskByAbsTime
% setMaskByInfo
% what is the mask logic? AND, OR? or should this be specified as param
%
classdef pointProcessCollection
   %
   properties(GetAccess = public, SetAccess = private)
      % Cell array of names
      names;
      
      % Array of pointProcess objects
      array;
   end
   
   properties(GetAccess = public, SetAccess = public)
      % Boolean mask
      mask;
   end
   
   % These dependent properties all apply the mask property
   properties (GetAccess = public, SetAccess = private, Dependent)
      % # of pointProcess objects in collection
      count;
      
      % Minimum event time within window
      minTime;
      
      % Maximum event time within window
      maxTime;
   end
   
   properties(GetAccess = public, SetAccess = private)
      % Absolute time that all objects in collection are referenced to
      tAbs;
   end
   
   methods
      %% Constructor
      function self = pointProcessCollection(varargin)
         % Constructor, arguments are taken as name/value pairs
         % TODO
         % enforce common timeUnit in array
         
         if nargin == 0
            self = pointProcessCollection('array',pointProcess);
            return;
         end
         
         % Allow array to be passed in without name
         if (nargin>=1) && isa(varargin{1},'pointProcess')
            self = pointProcessCollection('array',varargin{1},varargin{2:end});
            return;
         end
         
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'pointProcessCollection constructor';
         p.addParamValue('array',pointProcess,@(x)isa(x,'pointProcess'));
         p.addParamValue('mask',[],@islogical);
         p.parse(varargin{:});
         
         array = p.Results.array(:)';
         [tAbs,ind] = sort([array.tAbs]);
         names = {array.name};
         
         n = length(array);
         self.array = array(ind);
         self.names = names(ind);
         if numel(p.Results.mask) == n
            self.mask = p.Results.mask(:)';
         else
            self.mask = true(1,n);
         end            
         self.tAbs = tAbs;
         
      end
      
      %% Set functions
      % Set the window property
      % window can be [1 x 2], where all objects are set to the same window
      % window can be [nObjs x 2], where each object window is set individually
      function self = setWindow(self,window)
         n = length(self);
         
         if nargin == 1
            for i = 1:n
               % Default to inclusive window for each element
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
      function count = get.count(self)
         % # of pointProcess objects in collection
         count = sum(self.mask);
      end
      function minTime = get.minTime(self)

         validTimes = self.array(self.mask).minTime;
         minTime = min(validTimes);
      end
      
      function maxTime = get.maxTime(self)
         validTimes = self.array(self.mask).maxTime;
         maxTime = max(validTimes);
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
         % Should also reset mask?? Is that useful?
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
      
      function [h,yOffset] = plot(self,varargin)
         % Alias to raster until I think of better plot override
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
         % Raster plot
         % For a full description of the possible parameters, see the help
         % for plotRaster
        
         % TODO
         % how to handle colors? perhaps intercept?
         
         % Should intercept window, propagate to all objects in all
         % collections, same for psth, otherwise the pointProcess window is
         % used
         
         % Should also be the option to plot ordered by collection first
         % or by names first. ie all trials for one name grouped, followed
         % by another, OR all names grouped (but different colors) in a collection,
         % followed by another collection
         
         % grpByName - group by names, ie all trials for one name grouped,
         %             followed by another, etc. This is alphabetical.
         %             Otherwise gprbyTime, all names grouped by collection
         % 
         

         % Intercept window parameter
         p = inputParser;
         p.KeepUnmatched= true;
         p.FunctionName = 'pointProcessCollection raster method';
         p.addParamValue('grpByName',true,@islogical);
         p.addParamValue('handle',NaN,@ishandle);
         p.addParamValue('yOffset',1,@isnumeric);
         p.parse(varargin{:});
         params = p.Unmatched; % passed through to pointProcess.raster

         if p.Results.grpByName
            [ind,nGrps] = self.getGrpByName();
         else
            [ind,nGrps] = self.getGrpByTime();
         end
         
         % Input can be vector of pointProcessCollections, so we concatonate
         array = cat(2,self.array);
         
         % Default colormap
         c = distinguishable_colors(nGrps);
         
         h = p.Results.handle;
         yOffset = p.Results.yOffset;
         count = 1;
         for i = 1:nGrps
            [h,yOffset] = array(ind{i}).raster('handle',h,'yOffset',...
               yOffset,'grpColor',c(count,:),params);
            count = count + 1;
         end
      end
   end
   
   methods(Access = private)
      function validWindow = checkWindow(window,n)
         % Should be analogous to method in pointProcess
      end
      
      function [ind,nGrps] = getGrpByName(self)
         % Return index into all elements of collection that pass mask
         %
         % ind   : cell array with the corresponding indices
         %
         % TODO
         % error checking and boundary conditions
         % need to modify of uniqueness is defined by names & locations
         % Check dimensions
         

         uNames = unique(self.names);
         count = 1;
         for i = 1:length(uNames)
            temp = find(strcmp(self.names(self.mask),uNames{i}));
            if ~isempty(temp)
               ind{count} = temp;
               count = count + 1;
            end
         end
         if ~exist('ind','var')
            nGrps = 0;
            ind = {};
         else
            nGrps = length(ind);
         end         
      end
      
      function [ind,nGrps] = getGrpByTime(self)
         % Return index into all elements of collection that pass mask
         %
         % ind   : cell array with the corresponding indices
         %
         % TODO
         % error checking and boundary conditions
         % Check dimensions
         
         uTAbs = unique(self.tAbs);
         count = 1;
         for i = 1:length(uTAbs)
            temp = find(self.tAbs(self.mask)==uTAbs(i));
            if ~isempty(temp)
               ind{count} = temp;
               count = count + 1;
            end
         end
         if ~exist('ind','var')
            nGrps = 0;
            ind = {};
         else
            nGrps = length(ind);
         end         
      end
      
   end
end