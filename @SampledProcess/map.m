function self = map(self,func,varargin)

for i = 1:numel(self)
   values = cellfun(func,self(i).values,'uni',false);
   
   % Check dimensions
   match = cellfun(@(x,y) size(x) == size(y),self.values,values,'uni',false);
   if any(~cat(1,match{:}))
      error('SampledProcess:remap:InputFormat','func must output same size');
   end
   self(i).values = values;
end