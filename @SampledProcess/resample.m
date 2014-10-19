%http://www.mathworks.fr/fr/help/matlab/ref/timeseries.synchronize.html?refresh=true
%http://www.mathworks.com/matlabcentral/fileexchange/12268-sinc-resample
function self = resample(self,newFs)
% use lcm?
% http://www.mathworks.com/matlabcentral/fileexchange/45329-sample-rate-conversion/content/SRC/srconv.m

% resample data in window
if self.Fs == newFs
   return;
end
[p,q] = rat(newFs/self.Fs);
%values = cellfun(@(x) resample(x,p,q),self.values,'uni',0);
[values{1},b] = resample(self.values{1},p,q);
nWindow = size(self.window,1);
if nWindow > 1
   values(2:nWindow,1) = cellfun(@(x) resample(x,p,q,b),...
      self.values(2:nWindow,1),'uni',0);
end

times = cellfun(@(x,y) self.tvec(x(1),1/newFs,size(y,1)),...
   self.times,values,'uni',0);

self.times = times;
self.values = values;
self.Fs = newFs;
