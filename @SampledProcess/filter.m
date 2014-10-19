function self = filter(self,b,a,fix)
if nargin < 4
   fix = false;
end
if nargin < 3
   a = 1;
end
for i = 1:numel(self)
   for j = 1:size(self(i).window,1)
      if fix
         self(i).values_ = filtfilt(b,a,self(i).values_);
         oldOffset = self(i).offset;
         %self.offset = 'windowIsReset';
         applyWindow(self(i));
         self(i).offset = oldOffset;
      else
         self(i).values{j} = filtfilt(b,a,self(i).values{j});
      end
   end
end
