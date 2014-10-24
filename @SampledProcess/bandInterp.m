function self = bandInterp(self,freqs,freqrange,chunksize)

for i = 1:numel(self)
   for j = 1:size(self(i).window,1)
      for k = 1:size(self.values{j},2)
         self(i).values{j}(:,k) = chunkwiseDeline(self(i).values{j}(:,k),...
            self(i).Fs,freqs,freqrange,chunksize);
      end
   end
end

