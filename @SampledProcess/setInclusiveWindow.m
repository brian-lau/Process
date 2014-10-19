function self = setInclusiveWindow(self)
% Set windows to earliest and latest event times
%
% SEE ALSO
% window, setWindow, applyWindow
for i = 1:numel(self)
   self(i).window = [min(self(i).times_) max(self(i).times_)];
end

