% TODO
%  o IIR filters?

function [self,h,d] = highpass(self,varargin)

if nargin < 2
   error('SampledProcess:highpass:InputValue',...
      'Must at least specify ''Fc'' and ''order''.');
end

p = inputParser;
p.KeepUnmatched = true;
addParameter(p,'Fpass',[],@isnumeric);
addParameter(p,'Fstop',[],@isnumeric);
addParameter(p,'Fc',[],@isnumeric);
addParameter(p,'order',[],@isnumeric);
addParameter(p,'attenuation',60,@isnumeric); % Stopband attenuation in dB
addParameter(p,'ripple',0.1,@isnumeric); % Passband ripple in dB
addParameter(p,'method','',@ischar);
addParameter(p,'plot',false,@islogical);
addParameter(p,'verbose',false,@islogical);
addParameter(p,'designOnly',false,@islogical);
parse(p,varargin{:});
par = p.Results;
   
for i = 1:numel(self)
   %------- Add to function queue ----------
   if isQueueable(self(i))
      addToQueue(self(i),par);
      if self(i).deferredEval
         continue;
      end
   end
   %----------------------------------------
   
   if isempty(par.order) % minimum-order filter
      assert(~isempty(par.Fpass)&&~isempty(par.Fstop),...
         'Minimum order filter requires Fpass and Fstop to be specified.');
      d = fdesign.highpass('Fst,Fp,Ast,Ap',...
         par.Fstop,par.Fpass,par.attenuation,par.ripple,self(i).Fs);
   else % specified-order filter
      if ~isempty(par.Fpass) && isempty(par.Fstop)
         d = fdesign.highpass('N,Fp,Ast,Ap',...
            par.order,par.Fpass,par.attenuation,par.ripple,self(i).Fs);
      elseif ~isempty(par.Fpass) && ~isempty(par.Fstop)
         d = fdesign.highpass('N,Fst,Fp,Ap',...
            par.order,par.Fstop,par.Fpass,par.ripple,self(i).Fs);
      elseif ~isempty(par.Fc) % 6dB cutoff
         d = fdesign.highpass('N,Fc,Ast,Ap',...
            par.order,par.Fc,par.attenuation,par.ripple,self(i).Fs);
      else
         error('SampledProcess:highpass:InputValue',...
            'Incomplete filter design specification');
      end
   end
   
   if isempty(par.method)
      h = design(d,'FilterStructure','dffir');
   else
      h = design(d,par.method,'FilterStructure','dffir');
   end
   
   if par.plot
      fvtool(h);
   end
   
   if par.verbose
      info(h,'long');
   end
   
   if ~par.designOnly
      self(i).filter(h.Numerator,'a',1);
   end
end
