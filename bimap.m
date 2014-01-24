% http://www.boost.org/doc/libs/1_42_0/libs/bimap/doc/html/boost_bimap/one_minute_tutorial.html
% https://google-collections.googlecode.com/svn/trunk/javadoc/com/google/common/collect/BiMap.html
% https://svn.mpl.ird.fr/us191/oceano/trunk/matlab/us191/+us191/@Map/Map.m

% TODO
% handle empty inputs

classdef bimap < handle
   properties
      left
      right
   end
   
   properties(GetAccess=private, SetAccess=private, Hidden=true)
      useID = false;
      right_
   end
   
   methods
      function self = bimap(varargin)
         
         % In case we use a different or subclassed map
         mapKind = @containers.Map;
         
         left = mapKind(varargin{:});
         lKeys = left.keys;
         lVals = left.values;
         kTypes = {'char' 'double' 'single' 'int32' 'uint32' 'int64' 'uint64'};
         vTypes = {'char' 'double' 'single' 'int8' 'uint8' 'int16' 'uint16'...
            'int32' 'uint32' 'int64' 'uint64' 'logical'};
         
         if strcmp(left.ValueType,'any')
            % Limited functionality for structs or objects as right keys.
            % This requires all right keys (left values) to be of the same
            % class. Also, these elements must contain an id field/property
            % that is a number that uniquely identifies structs or objects
            % that are considered different.
            for i = 1:length(lVals)
               lValClass{i} = class(lVals{i});
            end
            lValClass = unique(lValClass);
            if numel(lValClass) ~= 1
               error('All left values must be the same class');
            end
            try
               temp = cat(2,lVals{:});
            catch
               error('Left values must allow concatonation');
            end
            try
               id = num2cell([temp.id]);
            catch
               error('Left values must contain ID field or property');
            end
            
            % Create an internal mapping from id->struct, and reassign lVals
            self.right_ = mapKind(id,lVals);
            self.useID = true;
            lVals = id;
         else
            self.useID = false;
            self.right_ = [];
         end

         % containers.Map supports more value types than key types, so we
         % will have to cast these in the right map
         vTypesToCast = setdiff(vTypes,kTypes);
         
         %% Build right map
         % The left value type is compatible with being a right key
         if ismember(left.ValueType,vTypesToCast)
            % TODO cast to double?
         end
         
         if strcmp(left.ValueType,'char')
            [rKeys,I,J] = unique(lVals);
         else
            [rKeys,I,J] = unique(cat(2,lVals{:}));
         end
         if numel(rKeys) == numel(lKeys)
            % A unique mapping, just need to sort accordingly
            if ~iscell(rKeys) && (numel(rKeys)==1)
               % Wierd MATLAB bug?
               rKeys = {rKeys};
            end
            right = mapKind(rKeys,lKeys(I));
         else
            % Right keys map to multiple left keys. The right value is
            % defined as the array of left keys that map to it.
            % Left keys are either 'char' or 'numeric'
            if strcmp(left.KeyType,'char')
               % Put chars into a cell array
               for i = 1:numel(rKeys)
                  ind = i==J;
                  rValues{i} = cat(2,lKeys(ind));
               end
            else
               % Put numerics into a vector
               for i = 1:numel(rKeys)
                  ind = i==J;
                  rValues{i} = cat(2,lKeys{ind});
               end
            end
            right = mapKind(rKeys,rValues);
         end
         
         self.left = left;
         self.right = right;
      end % constructor

%       % put method, call containers.Map
%       % -------------------------------
%       function put(self, theKey, result)
%          self.map(theKey) = result;
%       end
      
      % get method from containers.Map
      % -------------------------------
      function result = get(self,keys)
         % Return value corresponding to left key
         % A cell array of keys will return a cell array of values
         try
            if iscell(keys)% && (numel(keys)>1)
               result = self.left.values(keys);
            else
               result = self.left(keys);
            end
         catch
            error('Key not found');
         end
      end
      
      function result = getR(self,keys)
         % Return value corresponding to right key
         % A cell array of keys will return a cell array of values
         if self.useID
            if numel(keys) > 1
               result = self.right.values(...
                  cellfun(@(x) x.id,keys,'UniformOutput',false));
            else
               result = self.right(keys.id);
            end
         else
            if iscell(keys)% && (numel(keys)>1)
               result = self.right.values(keys);
            else
               result = self.right(keys);
            end
         end
      end
      
      function result = values(self,keys)
         % Return values corresponding to left keys
         if nargin == 1
            result = values(self.left);
         else
            result = get(self,keys);
         end
      end
      
      function result = valuesR(self,keys)
         % Return value corresponding to right key
         if nargin == 1
            result = values(self.right);
         else
            result = getR(self,keys);
         end
      end
     
      function result = keys(self)
         % Return all the left keys
         result = keys(self.left);
      end
      
      function result = keysR(self)
         % Return all the right keys
         if self.useID
            result = self.right_.values;
         else
            result = keys(self.right);
         end
      end
      
      function bool = isKey(self,keys)
         % Boolean indicating whether left keys exist
         bool = isKey(self.left,keys);
      end
      
      function bool = isKeyR(self,keys)
         % Boolean indicating whether right keys exist
         if self.useID
            try
               if numel(keys) > 1
                  bool = isKey(self.right,...
                     cellfun(@(x) x.id,keys,'UniformOutput',false));
               else
                  bool = isKey(self.right,keys.id);
               end
            catch err
               if strcmp(err.identifier,'MATLAB:nonStrucReference')
                  bool = false(size(keys));
               else
                  rethrow(err);
               end
            end
         else
            bool = isKey(self.right,keys);
         end
      end
      
      function bool = isKeyR_slow(self,values)
         % Boolean indicating whether right keys exist
         % Purely for testing since this is O(n)... 
         %
         % Check values for all left keys
         % Note that isequal does funny things when comparing chars to
         % numbers, chars are converted to integers
         keys = self.left.keys;
         vals = self.left.values(keys);
         for i = 1:numel(values)
            bool(i) = any(...
               cellfun(@(x,y) isequal(x,y),vals,repmat(values(i),size(vals)))...
               );
         end
      end
      
      function theLength = length(self)
         % TODO rename this to count
         theLength = length(self.left);
      end
      
      function theLength = lengthR(self)
         % TODO rename this to countR
         theLength = length(self.right);
      end
            
      function remove(self,varargin)
         % Remove left key/value pair (right map updated correspondingly)
         %
         % SEE ALSO
         % removeR
         lKey = varargin{:};
         % Find the left value (right key)
         rKey = get(self,lKey);
         keyboard
         % Remove the desired left key, and make a new right value
         if strcmp(self.left.KeyType,'char')%isnumeric(rVal)
            % Remove string rather than chars of the string
            if iscell(rKey)
               rVal = getR(self,rKey);
            else
               rVal = getR(self,{rKey});
            end
            ind = strcmp(lKey,rVal);
            rValNew = rVal;
            rValNew(ind) = [];
         else
            % The left key (right value) to delete will be in this right value
            rVal = getR(self,rKey);
            ind = rVal == lKey;
            rValNew = rVal;
            rValNew(ind) = [];
         end
         
         % Remove the left key/value pair
         remove(self.left,lKey);
         
         % Reassign the right key/value pair
         if self.useID
            self.right(rKey.id) = rValNew;
         else
            self.right(rKey) = rValNew;
         end
         % TODO, if the right value ends up empty, do we delete the key?
         % probably, to match the behavior of containers.Map
         % If so, we have to adjust self.right_
      end
      
      function removeR(self,varargin)
         % Remove right key/value pair (left map updated correspondingly)
         % Removing right key. also not so bad, but you have to
         % 1) remove the left keys = right values
         % 2) remove the right keys
         % 3) remove the right_ keys with corresponding id
         
         lKey = varargin{:};
         % Find the left value (right key)
         rKey = get(self,lKey);
         
         % The left key (right value) to delete will be in this right value
         rVal = getR(self,rKey);
         % Remove the desired left key, and make a new right value
         if isnumeric(rVal)
            ind = rVal == lKey;
         else
            ind = strcmp(lKey,rVal);
         end
         rValNew = rVal;
         rValNew(ind) = [];
         
         % Remove the left key/value pair
         remove(self.left,lKey);
         % Reassign the right key/value pair
         if self.useID
            self.right(rKey.id) = rValNew;
         else
            self.right(rKey) = rValNew;
         end
         % TODO, if the right value ends up empty, do we delete the key?
         % probably, to match the behavior of containers.Map
         % If so, we have to adjust self.right_
      end % removeR
      
      %% Overloading subsref
      function result = subsref(self,S)
         % S is a struct array with two fields, type and subs
         % SEE ALSO
         % subsref
         switch (length(S))
            % map(ARG)
            % map.ARG
            case 1
               switch S.type
                  case '.'
                     switch S.subs
                        case 'keys'
                           result = keys(self); return;
                        case 'keysR'
                           result = keysR(self); return;
                        case 'values'
                           result = values(self); return;
                        case 'valuesR'
                           result = valuesR(self); return;
                        case 'length'
                           result = length(self); return;
                        case 'lengthR'
                           result = lengthR(self); return;
                     end
                  case '()'
                     % Access the left map by key
                     % No equivalent for right key. Access the right key 
                     % using getR method
                     result = values(self,S.subs{:}); return;
               end
            case 2
               switch S(1).type
                  % map.ARG1(ARG2)
                  case '.'
                     key = S(2).subs{:};
                     switch S(1).subs
                        case 'get'
                           result = get(self,key); return;
                        case 'getR'
                           result = getR(self,key); return;
                        case 'remove'
                            remove(self,key); return;
                        case 'isKey'
                           result = isKey(self,key); return;
                        case 'isKeyR'
                           result = isKeyR(self,key); return;
                        case 'isKeyR_slow'
                           result = isKeyR_slow(self,key); return;
                        case 'values'
                           result = values(self,key); return;
                        case 'valuesR'
                           result = valuesR(self,key); return
                        otherwise
                           error('Unknown method %s for bimap object', S(1).subs);
                     end
                  case '()'
                     % Access a field of left map value
                     % map(ARG1).ARG2
                     result = get(self,S(1).subs{:});
                     switch S(2).type
                        case '.'
                           try
                              if iscell(result)
                                 % Cell array of keys
                                 temp = cat(2,result{:});
                                 result = [temp.(S(2).subs)];
                              else
                                 result = result.(S(2).subs);
                              end
                           catch
                              error('''%s'' is not a field structure for key-value', ...
                                 S(2).subs);
                           end
                     end
               end
            case 3
               % m.get(ARG1).ARG2
               % get(m,ARG1).ARG2  not allowed
               % getR not implemented since the bimap is asymmetric, right
               % values (left keys) cannot be structs or objects
               switch S(1).type
                  case '.'
                     switch S(1).subs
                        case 'get'
                           key = S(2).subs{:};
                           result = get(self,key);
                           try
                              if iscell(result)
                                 % Cell array of keys
                                 temp = cat(2,result{:});
                                 result = [temp.(S(3).subs)];
                              else
                                 result = result.(S(3).subs);
                              end
                           catch
                              error('''%s'' is not a field structure for key-value', ...
                                 S(3).subs);
                           end
                     end
               end
         end % switch length(S)
      end % subsref
   end
   
%     function display(self)      
%       fprintf(['\n  <a href="matlab:help us191.Map">us191.Map</a> ' ...
%         '<a href="matlab:help handle">handle</a>\n']);
%       
%       fprintf('\n  Properties:\n');
%       fprintf('\t      Count: %d [%-10s]\n', ...
%         self.map.Count, class(self.map));
%       fprintf('\t MagicField: [%s]\n', self.MagicField);
%       fprintf('\t    KeyType: [%s]\n', self.map.KeyType);
%       fprintf('\t  ValueType: [%s]\n', self.map.ValueType);
%       
%       fprintf('\n');
%     end
end