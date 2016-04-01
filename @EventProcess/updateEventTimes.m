% Adjust times stored in Events
function updateEventTimes(self,varargin)

for i = 1:size(self.times,1) % channels
   for j = 1:size(self.times,2) % windows
      temp = self.values{i,j};
      times = self.times{i,j};
      for k = 1:numel(temp)
         temp(k).tStart = times(k,1);
         temp(k).tEnd = times(k,2);
      end
      self.values{i,j} = temp;
   end
end