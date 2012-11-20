% pointprocess collection

% spatial location 
%

% simulation
% align - given a value, shift times relative to this
% psth
% reset (reset spike times back to original data)
%       origspikeTimes = spikeTimes + tAbs

% NEED VALIDATOR FOR UNIQUE NAMES?? or should we be able to put many
% instances of the same name? eg., trials into collection, there is no
% sense of ordering here?

% probably should force everything to be a row or column
%
classdef pointProcessCollection
%
   properties(GetAccess = public, SetAccess = private)
      
      names
      
      array
      
      locations
      
      locDimNames
      
      mask
   end
   
   properties (GetAccess = public, SetAccess = private, Dependent)
      minTime %Time first spike occurs in the collection
      maxTime %TIme last spike occurs in the collection      
   end
   
   methods
      %% Constructor
      function self = pointProcessCollection(varargin)
         
         % allow array to be passed in without name
         if isa(varargin{1},'pointProcess')
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
            self.array = p.Results.array;
         end
         [m,n] = size(p.Results.array);
         if isempty(p.Results.names)
            self.names = cell(m,n);
         else
            if sum(size(p.Results.names)==[m n]) == 2
               self.names = p.Results.names;
            else
               error('Bad names size');
            end
         end
         if isempty(p.Results.locations)
            self.locations = nan(m,n);
         else
            if sum(size(p.Results.locations)==[m n]) == 2
               self.locations = p.Results.locations;
            else
               error('Bad locations size');
            end
            self.locDimNames = {'not done'};
         end
         if isempty(p.Results.mask)
            self.mask = true(m,n);
         else
            if sum(size(p.Results.mask)==[m n]) == 2
               self.mask = p.Results.mask;
            else
               error('Bad mask size');
            end
         end
      end
      
      %% Get Functions

      %% Functions
      function [h,yOffset] = plot(self,varargin)
         [h,yOffset] = raster(self,varargin{:});
      end
      
      function [h,yOffset] = raster(self,varargin)
         % Input can be vector of pointProcessCollections, so we concatonate
         names = cat(2,self.names);
         uNames = unique(names);
         array = cat(2,self.array);
         mask = cat(2,self.mask);
         
%          % plotRaster requires a 2-D cell array of times, NaN-padded where
%          % there is no data. First determine the dimensions of this cell array.
%          % # of unique groups number to plot = uNames(mask)
%          for i = 1:length(uNames)
%             ind{i} = find(strcmp(names(mask),uNames{i}));
%             if ~isempty(ind{i})
%                n(i) = length(ind{i});
%                grp(i) = true;
%             end
%          end
%          maxN = unique(max(n));
%          nGrps = sum(grp);
% 
%          % Now that we know the dimensions, we can preallocate cell array
%          cellTimes = mat2cell(nan(maxN,nGrps),ones(1,maxN),ones(1,nGrps));
%          for i = 1:length(uNames)
%             ind2 = ind{i};
%             count = 1;
%             for j = ind2
%                if mask(j)
%                   cellTimes{count,i} = array(j).times;
%                   count = count + 1;
%                end
%             end
%          end
%          
%          [h,yOffset] = plotRaster(cellTimes,...
%             varargin{:});

         % Create index for each unique name across all collections
         for i = 1:length(uNames)
            ind{i} = find(strcmp(names(mask),uNames{i}));
            if ~isempty(ind{i})
               grp(i) = true;
            end
         end
         nGrps = sum(grp);
         
         c = distinguishable_colors(nGrps);
         count = 1;
         h = NaN;
         yOffset = 1;
         for i = find(grp)
            [h,yOffset] = array(ind{i}).raster('handle',h,'yOffset',...
               yOffset,'grpColor',c(count,:),varargin{:});
            count = count + 1;
         end
      end
      
   end
end