function plot(self)
figure; hold on
for i = 1:numel(self)
   subplot(numel(self),1,i);
   plot(self(i).times{1},self(i).values{1});
   %strips(self.times{i},self.values{i});
end