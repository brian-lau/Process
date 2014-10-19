function out = extract(self,labels)
for i = 1:numel(self)
   ind = ismember(self(i).labels,labels);
   if any(ind)
      if size(self(i).window,1) == 1
         out{i} = self(i).values{1}(:,ind);
      else
         out{i} = cellfun(@(x) x(:,ind),self(i).values,'uni',0);
      end
   end
end
