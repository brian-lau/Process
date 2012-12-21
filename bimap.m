classdef bimap < handle
   properties
      left
      right

      useID = false;
      right_
   end
   
   methods
      function self = bimap(varargin)
         
         %left = containers.Map(varargin{:});
         left = mymap(varargin{:});
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
            self.right_ = mymap(id,lVals);
            self.useID = true;
            lVals = id;
         else
            self.useID = false;
            self.right_ = [];
         end

         % containers.Map supports more value types than key types, so we
         % will have to cast these in the right map
         vTypesToCast = setdiff(vTypes,kTypes);
         
         %% Build reverse map
         % The left value type is compatible with being a right key
         if ismember(left.ValueType,vTypesToCast)
            % cast to double
         end
         
         if strcmp(left.ValueType,'char')
            [rKeys,I,J] = unique(lVals);
         else % numeric left value
            [rKeys,I,J] = unique(cat(2,lVals{:}));
         end
         if numel(rKeys) == numel(lKeys)
            % A unique mapping, just need to sort accordingly
            right = mymap(rKeys,lKeys(I));
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
            right = mymap(rKeys,rValues);
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
      function theValue = get(self, theKey)
         theValue = self.left(theKey);
      end
      function theValue = getR(self, theKey)
         theValue = self.right(theKey);
      end
      function tf = isKey(self, keys)
         tf = isKey(self.left, keys);
      end
      function tf = isKeyR(self, keys)
         tf = isKey(self.right, keys);
      end
      function theLength = length(self)
         theLength = length(self.left);
      end
      function theLength = lengthR(self)
         theLength = length(self.right);
      end
      function theValues = values(self, varargin)
         theValues = values(self.left, varargin{:});
      end
      function theValues = valuesR(self, varargin)
         theValues = values(self.right, varargin{:});
      end
%       function remove(self, varargin)
%          remove(self.map, varargin{:});
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
            % m('TEMP')
            % m.TEMP
            case 1
               switch theStruct.type
                  case '.'
                     switch theStruct.subs
                        case 'keys'
                           theValue = keys(self.left);
                           return
                        case 'keysR'
                           if self.useID
                              theValue = self.right_.values;
                           else
                              theValue = keys(self.right);
                           end
                           return
                        case 'values'
                           theValue = values(self.left);
                           return
                        case 'valuesR'
                           theValue = values(self.right);
                           return
                        case 'length'
                           theValue = length(self.left);
                           return
                        case 'lengthR'
                           theValue = length(self.right);
                           return
                     end
                  case '()'
                     % Access the left map by key
                     key = theStruct.subs{:};
                     if iscell(theStruct.subs{1})
                        theValue = self.left.values(key);
                     else
                        theValue = self.left(key);
                     end
                     return
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
                     if iscell(theStruct(2).subs{1})
                        key = theStruct(2).subs{:};
                     else
                        key = theStruct(2).subs;
                     end %
                     switch theStruct(1).subs
                        case 'get'
                           theValue = self.left(key);
                           return
                        case 'getR'
                           theValue = self.right(key);
                           return
                           %                         case {'remove'}
                           %                            remove(self.left, key);
                           %                            return
                        case 'isKey'
                           theValue = isKey(self.left,key);
                           return
                        case 'isKeyR'
                           if self.useID
                              theValue = isKey(self.right,key{1}.id);
                           else
                              theValue = isKey(self.right,key);
                           end
                           return
                        case 'values'
                           theValue = values(self.left,key);
                           return
                        case 'valuesR'
                           theValue = values(self.right,key);
                           return
                        otherwise
                           % Don't understand what this is for
                           theValue = self.map(key);
                           if isfield(theValue, theStruct(2).subs)
                              theValue = self.map(key).(theStruct(2).subs);
                           else
                              error('unknow method %s for Map object', theStruct(1).subs);
                           end
                     end
                     
                     % m('TEMP').long_name
                  case {'()'}
                     if iscell(theStruct(1).subs)
                        key = theStruct(1).subs{:};
                     else
                        key = theStruct(1).subs;
                     end
                     theValue = self.left(key);
                     switch theStruct(2).type
                        case '.'
                           if isfield(theValue, theStruct(2).subs)
                              theValue = self.map(key).(theStruct(2).subs);
                           else
                              error('''%s'' is not a field structure for key ''%s''', ...
                                 theStruct(2).subs, key);
                           end
                     end
               end
               
               % m.get('TEMP').name
               % get(m,'TEMP').name  not allowed
               % -------------------------------
            case 3 % only for m.get('TEMP').data rule
               switch theStruct(1).type
                  case {'.'}
                     switch theStruct(1).subs
                        case 'get'
                           if iscell(theStruct(2).subs)
                              key = theStruct(2).subs{:};
                           else
                              key = theStruct(2).subs;
                           end
                           theValue = self.map(key);
                           if isfield(theValue, theStruct(3).subs)
                              theValue = self.map(key).(theStruct(3).subs);
                           else
                              error('(%s) is not struct member for key (%s)', ...
                                 theStruct(3).subs, key);
                           end
                     end
               end
               
         end % end of switch length(theStruct)
      end % end of subsref
   end
   
end