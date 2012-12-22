
classdef mymap < containers.Map
   properties
   end
   
   methods
      function self = mymap(varargin)
         self = self@containers.Map(varargin{:});
      end
      
      function put(self,keys,values)
         % bizarre, cannot call like self.put, must use put(self...)???
         newMap = mymap(keys,values);
         assignin('caller',inputname(1),[self;newMap]);
      end
      
      function bool = containsKey(self,keys)
         bool = isKey(self,keys);
      end
      
      function bool = containsValue(self,values,keys)
         % This is O(n)... 
         if nargin < 3
            % Check values for all keys
            keys = self.keys;
         end
         vals = self.values(keys);
         for i = 1:numel(values)
            bool(i) = any(...
               cellfun(@(x,y) isequal(x,y),vals,repmat(values(i),size(vals)))...
               );
         end
      end
   end
   
end