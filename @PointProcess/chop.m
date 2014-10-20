function obj = chop(self,shiftToWindow)
% TODO
% can we rechop?
%     yes, not sure its useful, but i guess it should work.
%     eg., chop first by trials, then chop relative to an event
%     within each trial?
%
% need to handle case where there is an offset?, or perhaps there
% should be a convention?
if nargin == 1
   shiftToWindow = true;
end

if numel(self) > 1
   error('PointProcess:chop:InputCount',...
      'You can only chop a scalar PointProcess.');
end

nWindow = size(self.window,1);
% FIXME, http://www.mathworks.com/support/bugreports/893538
% May need looped allocation if there is a circular reference.
obj(nWindow) = PointProcess();
oldOffset = self.offset;
self.offset = 0;
for i = 1:nWindow
   obj(i).info = copyInfo(self);
   
   if shiftToWindow
      shift = self.window(i,1);
   else
      shift = 0;
   end
   
   obj(i).times_ = cellfun(@(x) x - shift,self.times(i,:),'uni',0);
   obj(i).values_ = self.values(i,:);
   
   obj(i).tStart = self.window(i,1) - shift;
   obj(i).tEnd = self.window(i,2) - shift;
   obj(i).window = self.window(i,:) - shift;
   obj(i).offset = oldOffset(i);
   
   % Need to set offset_ and window_
   obj(i).window_ = obj(i).window;
   obj(i).offset_ = self.offset_ + self.window(i,1);
end

if nargout == 0
   % Currently Matlab OOP doesn't allow the handle to be
   % reassigned, ie self = obj, so we do a silent pass-by-value
   % http://www.mathworks.com/matlabcentral/newsreader/view_thread/268574
   assignin('caller',inputname(1),obj);
end