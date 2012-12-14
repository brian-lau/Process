classdef eventDefs
   properties
      %
      word
      % 
      data
   end
   methods
      function self = eventDefs(w)
         self.word = w;
      end
      function c = array2cell(self)
         % Convert array of eventDefs to a cell array
         c = arrayfun(@(x) x,self,'UniformOutput',false);
      end
   end
   enumeration
      %
      spike               (0)
      
      %
      trialStart          (1)
      trialEnd            (2)
      
      %
      onCue               (2)
      offCue              (4)
      onReward            (5)
      offReward           (6)
      
      %
      condAttend          (7)
      condNoAttend        (8)
   end
end