% Trial 1, 0:1 seconds
dt = 0.00001;
t = cos(2*pi*(0:dt:(1-dt)))';
s(1) = SampledProcess('values',t,'Fs',1/dt);

dt = 0.0001;
t = cos(2*pi*(0:dt:(1-dt))+pi/2)';
s(2) = SampledProcess('values',t,'Fs',1/dt);

info(1).tAlign = 1;
sig{1} = s;

% Trial 2, 0:3 seconds
dt = 0.00001;
t = cos(2*pi*(0:dt:(3-dt)))';
s(1) = SampledProcess('values',t,'Fs',1/dt);

dt = 0.0001;
t = cos(2*pi*(0:dt:(3-dt))+pi/2)';
s(2) = SampledProcess('values',t,'Fs',1/dt);

info(2).tAlign = 2;
sig{2} = s;

plot(sig{1})
plot(sig{2})

% Segment container
for i = 1:numel(sig)
   temp = containers.Map(fieldnames(info(i)),struct2cell(info(i)));
   data(i) = Segment('info',temp,'SampledProcesses',sig{i});
end



%
query = linq();
out = query.place(data)...
   .where(@(x) numel(x.info('tAlign'))==1)...
   .select(@(x) x.sync(x.info('tAlign'),'window',[-2 2]))...
   .select(@(x) extract(x,'SampledProcess')).toArray();

out.apply(@(x) nanstd(x))