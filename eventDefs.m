% Make a parent, abstract class

% Requirements,
%  R2010b, enumerations
classdef eventDefs
   properties % immutable
      %
      id
      % 
      data
   end
   methods
      function self = eventDefs(w)
         self.id = w;
      end
      
      function c = array2cell(self)
         % Convert array of eventDefs to a cell array
         c = cell(size(self));
         for i = 1:length(self)
            c{i} = self(i);
         end
         %c = arrayfun(@(x) x,self,'UniformOutput',false);
      end
      
      function [self,I,J] = unique(self)
         names = arrayfun(@(x) char(x),self,'UniformOutput',false);
         [~,I,J] = unique(names);
         self = self(I);
      end
      
%       function c = char(self)
%          keyboard
%          if numel(self) > 1
%             for i = 1:numel(self)
%                c{i} = char(self(i));
%             end
%          else
%             c = char(self);
%          end
%       end
   end
   enumeration
      %
      spike               (0)
      
      %
      trialStart          (1)
      trialEnd            (2)
      
      %
      onCue               (3)
      offCue              (4)
      onReward            (5)
      offReward           (6)
      
      %
      cond1               (7)
      cond2               (8)
      cond3               (9)
      cond4               (10)
   end
end