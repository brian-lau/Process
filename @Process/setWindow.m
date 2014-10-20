function self = setWindow(self,window)
% Set the window property. Works for array object input, where
% window must either be
%   [1 x 2] vector applied to all elements of the object array
%   {nObjs x 1} cell vector containing windows for each element of
%       the object array
%
% SEE ALSO
% window, applyWindow
n = numel(self);
if n == 1
   % single or multiple windows
   if ~isnumeric(window)
      error('process:setWindow:InputFormat',...
         'Window for a scalar process must be a numeric [nWin x 2] array.');
   end
   self.window = self.checkWindow(window,size(window,1));
else
   if isnumeric(window)
      % Single window or windows, same for each process
      window = self.checkWindow(window);
      [self.window] = deal(window);
   elseif iscell(window)
      % Different windows for each process
      window = self.checkWindow(window,n);
      [self.window] = deal(window{:});
   else
      error('process:setWindow:InputFormat',...
         'Window badly formatted.');
   end
end