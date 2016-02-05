% labels
% min
% excludeNaNs

% checktimes?
% quality check

function [obj,n] = mean(self)

uLabels = unique(cat(2,self.labels));
[s,l] = extract(self);
s = cat(3,s.values);
l = cat(2,l{:});
values = zeros(size(s,1),size(s,2),numel(uLabels));
for i = 1:numel(uLabels)
   ind = strcmp(l,uLabels{i});
   values(:,:,i) = nanmean(s(:,:,ind),3);
   n(i) = sum(ind);
end

obj = SpectralProcess(values,...
   'f',self(1).f,...
   'params',self(1).params,...
   'tBlock',self(1).tBlock,...
   'tStep',self(1).tStep,...
   'labels',uLabels,...
   'tStart',self(1).tStart,...
   'tEnd',self(1).tEnd,...
   'offset',self(1).offset,...
   'window',self(1).window...
   );
obj.cumulOffset = self(1).cumulOffset;
