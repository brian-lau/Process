function [self,b] = highpass(self,corner,order,fix)
if nargin < 4
   fix = false;
end
if nargin < 3
   order = 100;
end
assert(corner > (corner-1),'Corner frequency too low');
Fs = unique([self.Fs]);
assert(numel(Fs)==1,'Must have same Fs');
nyquist = self.Fs/2;

b = firls(order,[0 (corner-1)/nyquist corner/nyquist 1],[0 0 1 1]);
%keyboard
%freqz(b,1,[],'whole',Fs);
self.filter(b,1,fix);
