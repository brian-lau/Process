% TODO: test with other background shapes

clear all
rng(23351);
Fs = 2000;
T = 32;
step = 2;
p = [0.01 0.05 0.1];
N = 10;

for i = 1:N
   i
   [s,f,Sx0] = fakeLFP2(Fs,T,2);
   
   if step > 0
      win = [s.tStart:step:s.tEnd]';
      win = [win,win+step];
      win(win>s.tEnd) = s.tEnd;
      s.window = win;
   end
   
   S = Spectrum('input',s);
   S.psdParams.f = 0:.25:500;
   S.psdParams.quadratic = false; % inflates type 1 error (dof inaccurate)
   S.psdParams.hbw = .75;
   S.run;
   
   psdRaw{i} = S.psd.values{1};
   psdWhite{i} = S.psdWhite.values{1};
   for j = 1:numel(p)
      [~,fa{i,j}] = S.threshold(p(j));
   end
end

f = S.psdParams.f;
nbins = 150;

figure;
subplot(421); hold on
psdall = cat(1,psdRaw{:});
plot(f,mean(psdall));
set(gca,'yscale','log');

subplot(422); hold on
psdall = cat(1,psdWhite{:});
plot(f,mean(psdall));
grid on;

edp = linspace(0,.15,nbins);
edf = linspace(0,f(end),nbins);
bwf = mean(diff(edf));
num = cellfun(@(x) numel(x),fa) ./ numel(f);

i = 1;
subplot(4,2,3); hold on
histogram(num(:,i),edp,'Normalization','count');
plot([p(i) p(i)],get(gca,'ylim'),'--');

subplot(4,2,4); hold on
h = histogram(cat(2,fa{:,i}),edf,'Normalization','count');
y = p(i)*numel(S.psdParams.f)*N/h.NumBins;
plot(get(gca,'xlim'),[y y],'--');
plot(get(gca,'xlim'),[mean(h.Values) mean(h.Values)],'-');

i = 2;
subplot(4,2,5); hold on
histogram(num(:,i),edp,'Normalization','count');
plot([p(i) p(i)],get(gca,'ylim'),'--');

subplot(4,2,6); hold on
h = histogram(cat(2,fa{:,i}),edf,'Normalization','count');
y = p(i)*numel(S.psdParams.f)*N/h.NumBins;
plot(get(gca,'xlim'),[y y],'--');
plot(get(gca,'xlim'),[mean(h.Values) mean(h.Values)],'-');

i = 3;
subplot(4,2,7); hold on
histogram(num(:,i),edp,'Normalization','count');
plot([p(i) p(i)],get(gca,'ylim'),'--');

subplot(4,2,8); hold on
h = histogram(cat(2,fa{:,i}),edf,'Normalization','count');
y = p(i)*numel(S.psdParams.f)*N/h.NumBins;
plot(get(gca,'xlim'),[y y],'--');
plot(get(gca,'xlim'),[mean(h.Values) mean(h.Values)],'-');


% %%%
% Fs = 2000;
% T = 30;
% [s,f,Sx0] = fakeLFP2(Fs,T,0);
% 
% step = 3;
% win = [s.tStart:step:s.tEnd]';
% win = [win,win+step];
% win(win>s.tEnd) = s.tEnd;
% s.window = win;
% 
% S = Spectrum('input',s);
% S.psdParams.f = 0:.25:1000;
% S.psdParams.hbw = 1;
% S.run;
S.plotDiagnostics;
S.plot

% 
% hbw = 1;
% x = s.values;
% tic;[out,params] = sig.mtspectrum(x,'hbw',hbw,'f',0:.5:500,'Fs',Fs);toc
% 
% alpha = mean(params.k);
% 
% figure;
% xx = 0:.1:50;
% subplot(211); hold on
% % Eq 33 Das et al.
% n = histc(out.P*(7.5*2*alpha/mean(out.P)),[0:1:100]);
% bar([0:1:100],n./sum(n),'histc');
% hold on
% plot(xx,chi2pdf(xx,2*alpha),'r');
% axis([0 100 get(gca,'ylim')])
% xlabel('PSD values')
% title('Multitaper PSD is central \chi^2_\nu distributed, with \nu \approx 2\timesK')
% %sum(out.P*(2*params.k/mean(out.P)) > 100)