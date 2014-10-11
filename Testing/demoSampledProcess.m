
x = repmat(cos(2*pi*(0:.001:1-.001))',1,3);
s = SampledProcess('values',x,'Fs',1000,'tStart',0);

% signals sampled at same Fs, tStart, numel
dt = 0.00001;
x(:,1) = cos(2*pi*(0:dt:(1-dt)))';
x(:,2) = cos(2*pi*(0:dt:(1-dt))+pi/2)';
x(:,3) = cos(2*pi*(0:dt:(1-dt))+pi)';
s = SampledProcess('values',x,'Fs',1/dt,'tStart',0);

x = cos(2*pi*(0:.001:1-.001))';
s(1) = SampledProcess('values',x,'Fs',1000,'tStart',0);
x = cos(2*pi*(0:.001:.5-.001)+pi/2)';
s(2) = SampledProcess('values',x,'Fs',1000,'tStart',0);

%% ALIGNMENT
Fs = 1;
x = zeros(100,1);
x(50) = 1;
s(1) = SampledProcess('values',x,'Fs',Fs,'tStart',0.001);
x = zeros(50,1);
x(25) = 1;
s(2) = SampledProcess('values',x,'Fs',Fs,'tStart',0.001);

window = [-10 10]./Fs;
offset = [50 25]./Fs;

s.setWindow({window+offset(1) window+offset(2)});
s.setOffset(-offset);
[times,values] = arrayfun(@(x) deal(x.times{1},x.values{1}),s,'uni',false);

% if uni = true, determine output range
minT = min(cellfun(@min,times));
maxT = min(cellfun(@max,times));
% interp

% if tStart & Fs are all equal, no need to interp

%%
Fs = 1;
x = zeros(101,1);
x([1,51,101]) = 1;
s(1) = SampledProcess('values',x,'Fs',Fs,'tStart',0);
x = zeros(51,1);
x([1,26,51]) = 1;
s(2) = SampledProcess('values',x,'Fs',Fs,'tStart',0);

window = [-51 51]./Fs;
offset = [50 25]./Fs;

[times,values] = sync(s,offset,window);
stem(times,values)
% s.setWindow({window+offset(1) window+offset(2)});
% s.setOffset(-offset);
% [times,values] = arrayfun(@(x) deal(x.times{1},x.values{1}),s,'uni',false);


%%%%%%%%%%%% Windows outside of process start and end times extend as NaNs
s = SampledProcess('values',1:5,'Fs',1,'tStart',0);
assertEqual(s.times{1},(0:4)');
assertEqual(s.values{1},(1:5)');

s.window = [-1.5 2];
assertEqual(s.times{1},(-1:2)');
assertEqual(s.values{1},[NaN;(1:3)']);

s.window = [0 5.9];
assertEqual(s.times{1},(0:5)');
assertEqual(s.values{1},[(1:5)';NaN]);

s.window = [-1.5 5.5];
assertEqual(s.times{1},(-1:5)');
assertEqual(s.values{1},[NaN;(1:5)';NaN]);

s.window = [-1.5 2 ; 0 5.9];
assertEqual(s.times{1},(-1:2)');
assertEqual(s.times{2},(0:5)');
assertEqual(s.values{1},[NaN;(1:3)']);
assertEqual(s.values{2},[(1:5)';NaN]);

%% 
Fs = 1000;
dt = 1/Fs;
s = SampledProcess('values',1:5,'Fs',Fs,'tStart',0);
assertEqual(s.times{1},(0:4)'./Fs);
assertEqual(s.values{1},(1:5)');

s.window = [-1.5 2]./Fs;
assertEqual(s.times{1},(-1:2)'./Fs);
assertEqual(s.values{1},[NaN;(1:3)']);

s.window = [0 5.9]./Fs;
assertEqual(s.times{1},(0:5)'./Fs);
assertEqual(s.values{1},[(1:5)';NaN]);

s.window = [-1.5 5.5]./Fs;
assertEqual(s.times{1},(-1:5)'./Fs);
assertEqual(s.values{1},[NaN;(1:5)';NaN]);

s.window = [-1.5 2 ; 0 5.9]./Fs;
assertEqual(s.times{1},(-1:2)'./Fs);
assertEqual(s.times{2},(0:5)'./Fs);
assertEqual(s.values{1},[NaN;(1:3)']);
assertEqual(s.values{2},[(1:5)';NaN]);



