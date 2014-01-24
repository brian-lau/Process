% REQUIREMENTS
% R2008b - containers.Maps
% R2010a - containers.Maps constructor to specify key and value type
% R2011a - matlab.mixin.Copyable for copying handle objects
%
% TODO
% move checkWindows/checkOffset into static methods or even package
classdef(CaseInsensitiveProperties = true) process < hgsetget & matlab.mixin.Copyable
   properties
      info@containers.Map % Information about process
      userData
   end
   
   properties(SetAccess = protected)
      timeUnit % Time representation (placeholder)
      clock % Clock info (drift-correction)
   end

   properties(AbortSet)
      tStart % Start time of process
      tEnd   % End time of process
      window % [min max] time window of interest
      offset % Offset of event/sample times relative to window
   end
   
   properties
      labels
   end
   
   % Window-dependent, but only calculated on window change
   % http://blogs.mathworks.com/loren/2012/03/26/considering-performance-in-object-oriented-matlab-code/
   properties(SetAccess = protected, Transient = true)
      % Event/sample times
      % Note that any offset is applied *after* windowing, so times can be
      % outside of the windows property
      times

      % Attribute/value associated with each time
      values
      
      % Boolean if window(s) lies within tStart and tEnd
      isValidWindow
   end
            
   properties(SetAccess = protected, Hidden = true)
      index % Indices into times/values in window

      times_ % Original event/sample times
      values_ % Original attribute/values
      % Original [min max] time window of interest
      % TODO should we allow initial process be multiply windowed???
      window_
      offset_ % Original offset
   end
      
   properties(SetAccess = protected)
      version = '0.0.0'
   end
   
   methods(Abstract)
      reset(self)
      chop(self,shiftToWindow)
      %windowfun(self,fun)
      %windowFun(self,fun,nOpt,varargin) % apply applyFunc func?
      windowFun(self,fun) % apply applyFunc func?
      %copy?
      %plot
      setInclusiveWindow(self)
      
      % head
      % tail
   end
   
   methods(Abstract, Access = protected)
      applyWindow(self)
      applyOffset(self,undo)
   end

   methods
      function set.tStart(self,tStart)
         if isnan(tStart)
            self.tStart = 0;
         elseif isscalar(tStart) && isnumeric(tStart)
            self.tStart = tStart;
         end
      end
      
      function set.tEnd(self,tEnd)
         % TODO, validate against tStart
         % what is the point of tStart and tEnd??? should they be public?
         if isscalar(tEnd) && isnumeric(tEnd)
            self.tEnd = tEnd;
         end
      end
      
      function set.window(self,window)
         % Set the window property. Does not work for arrays of objects.
         %
         % SEE ALSO
         % setWindow, applyWindow
         self.window = self.checkWindow(window,size(window,1));
         % Reset offset, which always follows window
         self.offset = 'windowIsReset';
         % Expensive, only call when windows are changed (AbortSet=true)
         applyWindow(self);
      end
      
      function self = setWindow(self,window)
         % Set the window property. Works for array object input, where
         % window must either be
         %   [1 x 2] vector applied to all elements of the object array
         %   {nObjs x 1} cell vector containing windows for each element of
         %       the object array
         %
         % SEE ALSO
         % window, applyWindow
         n = numel(self);
         if n == 1
            % single or multiple windows
            if ~isnumeric(window)
               error('process:setWindow:InputFormat',...
                  'Window for a scalar process must be a numeric [nWin x 2] array.');
            end
            self.window = self.checkWindow(window,size(window,1));
         else
            if isnumeric(window)
               % single window or windows, same for each process
               window = self.checkWindow(window);
               [self.window] = deal(window);
            elseif iscell(window)
               % Different windows for each process
               window = self.checkWindow(window,n);
               [self.window] = deal(window{:});
            else
               error('process:setWindow:InputFormat',...
                  'Window badly formatted.');
            end
         end
      end
      
      function set.offset(self,offset)
         % Set the offset property. Does not work for arrays of objects.
         %
         % SEE ALSO
         % setOffset, applyOffset
         if strcmp(offset,'windowIsReset')
            self.offset = zeros(size(self.window,1),1);
         else
            newOffset = self.checkOffset(offset,size(self.window,1));
            % Reset offset, which is always follows window
            applyOffset(self,true);
            self.offset = newOffset;
            % Only call when offsets are changed
            applyOffset(self);
         end
      end
      
      function self = setOffset(self,offset)
         % Set the offset property. Works for array object input, where
         % offset must either be
         %   scalar applied to all elements of the object array
         %   {nObjs x 1} cell vector containing offsets for each element of
         %       the object array
         %
         % SEE ALSO
         % offset, applyOffset
         n = numel(self);
         if n == 1
            % single or multiple offsets
            if ~isnumeric(offset)
               error('process:setOffset:InputFormat',...
                  'Offset for a scalar process must be a numeric [nWin x 1] array.');
            end
            self.offset = self.checkOffset(offset,size(self.window,1));
         else
            if isscalar(offset) && isnumeric(offset)
               % single offset or offsets, same for each process
               offset = self.checkOffset(offset);
               [self.offset] = deal(offset);
            elseif isvector(offset) 
               % Different offset for each process
               offset = self.checkOffset(num2cell(offset),n);
               [self.offset] = deal(offset{:});               
            elseif iscell(offset)
               % Different offset for each process
               offset = self.checkWindow(offset,n);
               [self.offset] = deal(offset{:});
            else
               error('process:setOffset:InputFormat','Bad offset');
            end
         end
      end
      
%       function self = setInclusiveWindow(self)
%          % Set windows to earliest and latest event times
%          %
%          % SEE ALSO
%          % window, setWindow, applyWindow
%          
%          % FIXME, what happens for sampledProcess??? if we resample???
%          for i = 1:numel(self)
%             self(i).window = [min(self(i).times_) max(self(i).times_)];
%          end
%       end
      
      function keys = infoKeys(self,flatBool)
         % Return array of keys in INFO dictionary
         %
         % If flatBool is true (default false), the returned cell array
         % will be collapsed across all pointProcess elements passed in
         if nargin < 2
            flatBool = false;
         end
         
         keys = arrayfun(@(x) x.info.keys,self,'uniformoutput',false);
         if flatBool
            keys = flatten(keys);
         end
      end
      
      function bool = infoHasKey(self,key)
         % Boolean for whether INFO dictionary has key
         bool = arrayfun(@(x,y) x.info.isKey(y),self,repmat({key},size(self)));
      end
            
      function bool = infoHasValue(self,value,varargin)
         % Boolean for whether INFO dictionary has value
         %
         % It is possible to restrict to keys by passing in additional args
         % self.doesInfoHaveValue(value,'keys',{cell array of keys})
         bool = self.mapHasValue({self.info},value,varargin{:});
      end   
   end
   
   methods(Static, Access = protected)
      function [bool,keys] = mapHasValue(map,value,varargin)
         % Check whether dictionary contains value
         %
         % OUTPUT
         % bool - boolean indicating whether value exists in map
         % keys - corresponding cell array of keys for which bool is true
         %
         % It is possible to restrict to keys by passing in additional args
         % self.doesHashmapHaveValue(value,'keys',{cell array of keys})
         %
         % SEE ALSO
         % mapfun
         
         % TODO, checking for cell array input?
         
         [temp,keys] = mapfun(@(x,y) isequal(x,y),map,{value},varargin{:});
         bool = cellfun(@(x) any(x),temp);
         if nargout == 2
            for i = 1:numel(temp)
               keys{i} = keys{i}(logical(temp{i}));
            end
         end
      end
      
      function validWindow = checkWindow(window,n)
         % Validate window, and replicate if necessary
         %
         % % single window
         % [start end]
         %
         % % one window for each of n elements
         % [start(1,1) end(1,1)
         %    start(2,1) end(2,1)
         %    start(n,1) end(n,1)
         %    ]
         %
         % % aribitrary windows for each of n elements
         % {
         %   [start(1,1) end(1,1)   [start(1,2) end(1,2)   [start(1,n) end(1,n)]
         %    start(2,1) end(2,1)]   start(2,2) end(2,2)]
         %  }
         %
         % For example, to use the same set of windows for n elements,
         % checkWindow({[-6 0;0 6;-6 6]},n)
         % checkWindow({[0 1;1 2] [2 3]},2)
         if nargin == 1
            n = 1;
         end
         
         if iscell(window)
            % Same windows for each element
            if numel(window) == 1
               window(1:n) = window;
            end
            % Different windows for each element
            if numel(window) == n
               for i = 1:n
                  validWindow{1,i} = process.checkWindow(window{i},size(window{i},1));
               end
            else
               error('process:checkWindow:InputFormat',...
                  'Cell array window must be {[nx2]} or [nObjs x 2]');
            end
         else
            if numel(window) == 2
               window = window(:)';
               if n > 1
                  window = repmat(window,n,1);
               end
            end
            if any(window(:,1)>=window(:,2))
               error('process:checkWindow:InputValue',...
                  'First element of window must be less than second');
            end
            validWindow = window;
         end
      end
      
      function validOffset = checkOffset(offset,n)
         % Validate offset, and replicate if necessary
         %
         % % single offset
         % [offset]
         %
         % % one offset for each of n elements
         % [offset(1)
         %  offset(2)
         %  offset(n)
         %    ]
         %
         % % aribitrary offsets for each of n elements
         % {
         %   [offset(1,1)   [offset(1,2)   [start(1,n) end(1,n)]
         %    offset(2,1)]   offset(2,2)]   start(2,2) end(2,2)]
         %  }
         %
         % For example, to use the same set of windows for n elements,
         % checkWindow({[-6 0;0 6;-6 6]},n)
         if nargin == 1
            n = 1;
         end
         
         if iscell(offset)
            % Same offsets for each element
            if numel(offset) == 1
               offset(1:n) = offset;
            end
            % Different offsets for each element
            if numel(offset) == n
               for i = 1:n
                  validOffset{1,i} = process.checkOffset(offset{i},length(offset{i}));
               end
            else
               error('process:checkOffset:InputFormat',...
                  'Cell array offset must be {[nx1]} or [nObjs x 1]');
            end
         else
            if numel(offset) == 1
               if n > 1
                  offset = repmat(offset,n,1);
               end
            end
            if numel(offset) ~= n
               error('process:checkOffset:InputFormat',...
                  'Incorrect number of offsets.');
            end
            validOffset = offset(:);
         end
      end
   end % methods(Static)   
end
