classdef eventDefs
   properties
      word
   end
   methods
      function self = eventDefs(w)
         self.word = w;
      end
      function c = array2cell(self)
         c = arrayfun(@(x) x,self,'UniformOutput',false);
      end
   end
   enumeration
      %
      Spike        (0)
      
      %
      StartTrial   (1)
      EndTrial     (2)
      
      %
      CueOn        (2)
      CueOff       (4)
      
      %
      Condition1   (5)
      Condition2   (6)
   end
end