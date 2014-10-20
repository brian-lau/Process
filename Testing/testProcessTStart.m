function test_suite = testProcessTStart
initTestSuite;

function testNoArgs
% Not enough parameters to do any alignment
p =  PointProcess();
assertTrue(isa(p,'PointProcess'),'Constructor failed to create PointProcess without inputs');

function testPointProcessSingleArg
p = PointProcess(0);
assertEqual(p.tStart,0);

p = PointProcess(10);
assertEqual(p.tStart,0);

p = PointProcess(-10);
assertEqual(p.tStart,-10);

p = PointProcess(-1:10);
assertEqual(p.tStart,-1);

p = PointProcess(0);
assertEqual(p.tStart,0);

function testSampledProcessSingleArg
s = SampledProcess(0);
assertEqual(s.tStart,0);
assertEqual(s.times{1}(1),0);
assertEqual(s.times_(1),0);

s = SampledProcess(100);
assertEqual(s.tStart,0);
assertEqual(s.times{1}(1),0);
assertEqual(s.times_(1),0);

s = SampledProcess(-100);
assertEqual(s.tStart,0);
assertEqual(s.times{1}(1),0);
assertEqual(s.times_(1),0);

function testPointProcessSetTstart
p = PointProcess('times',0:10,'tStart',0);
assertEqual(p.tStart,0);

p = PointProcess('times',0:10,'tStart',10);
assertEqual(p.tStart,10);

p = PointProcess('times',0:10,'tStart',-10);
assertEqual(p.tStart,-10);

% Odd situation where tStart will empty times
% Don't have this issue with SampledProcess, since we can only specify tStart
p = PointProcess('times',0:10,'tStart',100);
assertEqual(p.tStart,100);

function testSampledProcessSetTstart
s = SampledProcess('values',1:10,'tStart',100);
assertEqual(s.tStart,100);
assertEqual(s.times{1}(1),100);
assertEqual(s.times_(1),100);

s = SampledProcess('values',1:10,'tStart',-100);
assertEqual(s.tStart,-100);
assertEqual(s.times{1}(1),-100);
assertEqual(s.times_(1),-100);


% 
% 
% f = @() PointProcess('shouldnotwork');
% assertExceptionThrown(f, 'PointProcess:Constructor:InputFormat');
% 
% f = @() PointProcess({'shouldnotwork'});
% assertExceptionThrown(f, 'PointProcess:Constructor:InputFormat');
% assertEqual(p.times_{1}(1),0);