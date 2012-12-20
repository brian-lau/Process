classdef bimap < handle
   properties
      left
      right
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
            error('bimap doesn''t work for this');
         end
         
         % containers.Map supports more value types than key types, so we
         % will have to cast these in the right map
         vTypesToCast = setdiff(vTypes,kTypes);
         
         %% Build reverse map
         if ismember(left.ValueType,vTypesToCast)
            if strcmp(left.ValueType,'any')
               error('bimap doesn''t work for this');
            else
               % cast 
            end
            
            % Decided not to try and deal with 'any' left valuetype. No
            % clear way to construct mapping. User will have to do their
            % own mapping to benefit
%             % All left values must be the same class
%             for i = 1:length(lVals)
%                lValClass{i} = class(lVals{i});
%             end
%             lValClass = unique(lValClass);
%             if numel(lValClass) ~= 1
%                error('All left values must be the same class');
%             end
%             [rKeys,I,J] = unique(cat(2,lVals));
%             %[rKeys,I,J] = unique(cat(2,lVals{:}));
%             rKeys = arrayfun(@(x) char(x),rKeys,'UniformOutput',false);
         else
            % The left value type is compatible with being a right key
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
               % Right keys map to multiple left keys
               if strcmp(left.KeyType,'char')
                  for i = 1:numel(rKeys)
                     ind = i==J;
                     rValues{i} = cat(2,lKeys(ind));
                  end
               else
                  for i = 1:numel(rKeys)
                     ind = i==J;
                     rValues{i} = cat(2,lKeys{ind});
                  end
               end
               right = mymap(rKeys,rValues);
            end
         end         
         
         self.left = left;
         self.right = right;
      end
      
   end
   
end