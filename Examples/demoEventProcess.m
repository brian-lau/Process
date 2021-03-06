
fix = metadata.Label('name','fix');
cue = metadata.Label('name','cue');
button = metadata.Label('name','button');

e(1) = metadata.event.Stimulus('tStart',0.5,'tEnd',1,'name',fix);
e(2) = metadata.event.Stimulus('tStart',2,'tEnd',3,'name',cue);
e(3) = metadata.event.Response('tStart',5,'tEnd',6,'name',button,'experiment',metadata.Experiment);

events = EventProcess('events',e,'tStart',0,'tEnd',10);

e(1) = metadata.event.Stimulus('tStart',0.5,'tEnd',1,'name',fix,'color','r');
e(2) = metadata.event.Stimulus('tStart',2,'tEnd',3,'name',cue,'color','b');
e(3) = metadata.event.Response('tStart',5,'tEnd',6,'name',button,'experiment',metadata.Experiment,'color','g');

events = EventProcess('events',e,'tStart',0,'tEnd',10);

% tic;
% for i = 1:1000
%    events.values;
% end
% toc

fix = metadata.Label('name','fix');
cue = metadata.Label('name','cue');
button = metadata.Label('name','button');

e(1) = metadata.event.Stimulus('tStart',0.5,'tEnd',1,'name',fix);
e(2) = metadata.event.Stimulus('tStart',2,'tEnd',3,'name',cue);
e(3) = metadata.event.Response('tStart',5,'tEnd',6,'name',button,'experiment',metadata.Experiment);

events(1) = EventProcess('events',e,'tStart',0,'tEnd',10);
events(2) = EventProcess('events',e,'tStart',0,'tEnd',10);


fix = metadata.Label('name','fix');
cue = metadata.Label('name','cue');
button = metadata.Label('name','button');

for i = 1:50
   t = rand;
   e(1) = metadata.event.Stimulus('tStart',t,'tEnd',t+1,'name',fix);
   t = 2 + rand;
   e(2) = metadata.event.Stimulus('tStart',t,'tEnd',t,'name',cue);
   t = 4 + rand;
   e(3) = metadata.event.Response('tStart',t,'tEnd',t+2,'name',button,'experiment',metadata.Experiment);
   
   events(i) = EventProcess('events',e);
end

%%
fix = metadata.Label('name','fix');
cue = metadata.Label('name','cue');
artifact = metadata.Label('name','artifact');
button = metadata.Label('name','button');

e(1) = metadata.event.Stimulus('tStart',0.5,'tEnd',1,'name',fix);
e(2) = metadata.event.Artifact('tStart',1,'tEnd',1.5,'name',artifact);
e(3) = metadata.event.Stimulus('tStart',2,'tEnd',3,'name',cue);
e(4) = metadata.event.Artifact('tStart',3,'tEnd',4,'name',artifact);
e(5) = metadata.event.Artifact('tStart',5,'tEnd',8,'name',artifact);
e(6) = metadata.event.Response('tStart',5,'tEnd',6,'name',button,'experiment',metadata.Experiment);

events = EventProcess('events',e,'tStart',0,'tEnd',10);

window = events.getWindow('eventType','Artifact')