classdef bimap < handle
   properties
      left
      right

      useID = false;
      right_
   end
   
   methods
      function self = bimap(varargin)
         
         left = containers.Map(varargin{:});
         %left = mymap(varargin{:});
         lKeys = left.keys;
         lVals = left.values;
         kTypes = {'char' 'double' 'single' 'int32' 'uint32' 'int64' 'uint64'};
         vTypes = {'char' 'double' 'single' 'int8' 'uint8' 'int16' 'uint16'...
            'int32' 'uint32' 'int64' 'uint64' 'logical'};
         
         if strcmp(left.ValueType,'any')
            % Attempt would be to allow structs or objects as right keys.
            % This requires right keys (left values) to all be of the same
            % class. These elements must contain an id field or property
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
            self.right_ = containers.Map(id,lVals);
            %self.right_ = mymap(id,lVals);
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
            % TODO cast to double
         end
         
         if strcmp(left.ValueType,'char')
            [rKeys,I,J] = unique(lVals);
         else
            [rKeys,I,J] = unique(cat(2,lVals{:}));
         end
         if numel(rKeys) == numel(lKeys)
            % A unique mapping, just need to sort accordingly
            %right = mymap(rKeys,lKeys(I));
            right = containers.Map(rKeys,lKeys(I));
            %cellfun(@(x,y) fprintf('%s : %g\n',x,y),rKeys,rVals)
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
            right = containers.Map(rKeys,rValues);
            %right = mymap(rKeys,rValues);
         end
         
         self.left = left;
         self.right = right;

      end % constructor

%       % put method, call containers.Map
%       % -------------------------------
%       function put(self, theKey, theValue)
%          self.map(theKey) = theValue;
%       end
      
      % get method from containers.Map
      % -------------------------------
      function theValue = get(self,key)
         try
            if numel(key) > 1
               theValue = self.left.values(key);
            else
               theValue = self.left(key);
            end
         catch
            error('Key not found');
         end
      end
      
      function theValue = getR(self,key)
         if self.useID
            if numel(key) > 1
               theValue = self.right.values(...
                  cellfun(@(x) x.id,key,'UniformOutput',false));
            else
               theValue = self.right(key.id);
            end
         else
            if numel(key) > 1
               theValue = self.right.values(key);
            else
               theValue = self.right(key);
            end
         end
      end
      
      function theValue = keys(self)
         theValue = keys(self.left);
      end
      
      function theValue = keysR(self)
         if self.useID
            theValue = self.right_.values;
         else
            theValue = keys(self.right);
         end
      end
      
      function bool = isKey(self,keys)
         bool = isKey(self.left,keys);
      end
      
      function bool = isKeyR(self,keys)
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
      
      function theLength = length(self)
         theLength = length(self.left);
      end
      
      function theLength = lengthR(self)
         theLength = length(self.right);
      end
      
      function theValues = values(self,varargin)
         if nargin == 1
            theValues = values(self.left);
         else
            if iscell(varargin{1})
               theValues = values(self.left,varargin{:});
            else
               theValues = self.left(varargin{:});
            end
         end
      end
      
      function theValues = valuesR(self,varargin)
         if self.useID
            if isempty(varargin)
               theValues = values(self.right);
            else
               if numel(varargin{1}) > 1
                  theValues = values(self.right,cellfun(@(x) x.id,varargin{:},'UniformOutput',false));
               else
                  % TODO catch error if key is not structure
                  theValues = self.right(varargin{1}.id);
               end
            end
         else
            theValues = values(self.right, varargin{:});
         end
      end
      
      function remove(self,varargin)
         % Remove left key/value pair (right map updated correspondingly)
         
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
      end
      
      function removeR(self,varargin)
         % Remove right key/value pair (left map updated correspondingly)
         
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
      end
%       function removeR(self, varargin)
%          remove(self.right, varargin{:});
%          remove(self.left, varargin{:});
%          remove(self.right_, varargin{:});
%       end
%                         case {'remove'}
% remove must be treated carefully. removing from left requires rebuilding
% right? Perhaps not, since left keys are unique, you can search through
% the right values, and break when we find it.
% 1) remove the left key/value pair
% 2) search the right values...
% Removing right key. also not so bad, but you have to
% 1) remove the left keys = right values
% 2) remove the right keys
% 3) remove the right_ keys with corresponding id
%                            remove(self.left, key);
%                            return
%       function bool = containsValue(self,values,keys)
%          % This is O(n)... 
%          if nargin < 3
%             % Check values for all keys
%             keys = self.keys;
%          end
%          vals = self.values(keys);
%          for i = 1:numel(values)
%             bool(i) = any(...
%                cellfun(@(x,y) isequal(x,y),vals,repmat(values(i),size(vals)))...
%                );
%          end
%       end

      % subsref, MATLAB uses the built-in subsref function to interpret indexed
      % references to objects. To modify the indexed reference behavior of
      % objects, overload subsref in the class.
      % ex:
      % m = us191.Map('TEMP', struct('data__',21.56,'long_name','Temperature'))
      % m('PSAL') = struct('data__', 35.43, 'long_name', 'Salinity')
      % ---------------------------------------------------------------------
      function theValue = subsref(self, theStruct)
         % theStruct is a struct array with two fields, type and subs
         % ----------------------------------------------------------
         switch (length(theStruct))
            % map(ARG)
            % map.ARG
            case 1
               switch theStruct.type
                  case '.'
                     switch theStruct.subs
                        case 'keys'
                           theValue = keys(self); return;
                        case 'keysR'
                           theValue = keysR(self); return;
                        case 'values'
                           theValue = values(self); return;
                        case 'valuesR'
                           theValue = valuesR(self); return;
                        case 'length'
                           theValue = length(self); return;
                        case 'lengthR'
                           theValue = lengthR(self); return;
                     end
                  case '()'
                     % Access the left map by key
                     % Access the right key using getR method
                     theValue = values(self,theStruct.subs{:}); return;
               end
               
            % m('TEMP').long_name
            % m.TEMP.long_name
            % m.get('TEMP')
            % get(m,'TEMP')
            % m.isKey('TEMP')
            % ------------------
            case 2
               switch theStruct(1).type
                  % m.TEMP.long_name
                  % m.get('TEMP')
                  case '.'
                     key = theStruct(2).subs{:};
                     switch theStruct(1).subs
                        case 'get'
                           theValue = get(self,key);
                           return
                        case 'getR'
                           theValue = getR(self,key);
                           return
%                         case {'remove'}
% remove must be treated carefully. removing from left requires rebuilding
% right? Perhaps not, since left keys are unique, you can search through
% the right values, and break when we find it.
% Removing right key. also not so bad, but you have to
% 1) remove the left keys = right values
% 2) remove the right keys
% 3) remove the right_ keys with corresponding id
%                            remove(self.left, key);
%                            return
                        case 'isKey'
                           theValue = isKey(self,key);
                           return
                        case 'isKeyR'
                           theValue = isKeyR(self,key);
                           return
                        case 'values'
                           theValue = values(self,key);
                           return
                        case 'valuesR'
                           theValue = valuesR(self,key);
                           return
%                         otherwise % not sure I can get here???
%                            % Access a field of left map value
%                            theValue = get(self,key);
%                            if isfield(theValue, theStruct(2).subs)
%                               theValue = theValue.(theStruct(2).subs);
%                            else
%                               error('unknow method %s for bimap object', theStruct(2).subs);
%                            end
                     end
                  case '()'
                     % Access a field of left map value
                     % m(ARG1).ARG2
                     theValue = get(self,theStruct(1).subs{:});
                     switch theStruct(2).type
                        case '.'
                           try
                              if iscell(theValue)
                                 % Cell array of keys
                                 temp = cat(2,theValue{:});
                                 theValue = [temp.(theStruct(2).subs)];
                              else
                                 theValue = theValue.(theStruct(2).subs);
                              end
                           catch
                              error('''%s'' is not a field structure for key-value', ...
                                 theStruct(2).subs);
                           end
                     end
               end
            case 3
               % m.get(ARG1).ARG2
               % get(m,ARG1).ARG2  not allowed
               % getR not implemented since the bimap is asymmetric, right
               % values (left keys) cannot be structs or objects
               % -------------------------------
               switch theStruct(1).type
                  case '.'
                     switch theStruct(1).subs
                        case 'get'
                           key = theStruct(2).subs{:};
                           theValue = get(self,key);
                           try
                              if iscell(theValue)
                                 % Cell array of keys
                                 temp = cat(2,theValue{:});
                                 theValue = [temp.(theStruct(3).subs)];
                              else
                                 theValue = theValue.(theStruct(3).subs);
                              end
                           catch
                              error('''%s'' is not a field structure for key-value', ...
                                 theStruct(3).subs);
                           end
                     end
               end
         end % end of switch length(theStruct)
      end % end of subsref
   end
   
end