function self = setInclusiveWindow(self)
% Set windows to earliest and latest event times
%
% SEE ALSO
% window, setWindow, applyWindow
for i = 1:numel(self)
   tempMin = cellfun(@min,self(i).times_,'uni',0);
   tempMin = min(vertcat(tempMin{:}));
   tempMax = cellfun(@max,self(i).times_,'uni',0);
   tempMax = max(vertcat(tempMax{:}));
   self(i).window = [tempMin tempMax];
end
