% collection of processes defined by common start and end time
% o Probably should place tStart/tEnd
% o must check for common start and end times!

% o methods for 
%   o adding processes
%   o 
classdef(CaseInsensitiveProperties, TruncatedProperties) Segment < hgsetget & matlab.mixin.Copyable
   properties
      info@containers.Map % Information about segment
   end
   properties
      labels
      data % FIXME: rename this to processes
   end
   properties(Dependent=true)
      dataType
      %window
      sameWindow
      sameOffset
   end
   
   methods
      %% Constructor
      function self = Segment(varargin)
         
         % if all inputs are of type PointProcess or SampledProcess,
         % cat and add (no need to pass in paramvalue)
         
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'Segment constructor';
         p.addParamValue('info',containers.Map('KeyType','char','ValueType','any'));
         p.addParamValue('PointProcesses',[]);
         p.addParamValue('SampledProcesses',[]);
         p.addParamValue('labels',{},@(x) iscell(x) || ischar(x));
         p.parse(varargin{:});

         self.info = p.Results.info;
         self.data = {};
         if ~isempty(p.Results.PointProcesses)
            self.data = [self.data {p.Results.PointProcesses}];
         end
         if ~isempty(p.Results.SampledProcesses)
            self.data = [self.data {p.Results.SampledProcesses}];
         end
         
         % Create labels
         self.labels = p.Results.labels;
         
      end% constructor
      
      function list = get.dataType(self)
         list = cellfun(@(x) class(x),self.data,'uni',0);
      end
      
      function bool = get.sameWindow(self)
         % FIXME: this assumes each segment has single window
         window = cellfun(@(x) x.window,self.data,'uni',0);
         window = unique(vertcat(window{:}),'rows');
         if size(window,1) == 1
            bool = true;
         else
            bool = false;
         end
      end
      
      function bool = get.sameOffset(self)
         % FIXME: this assumes each segment has single offset
         offset = cellfun(@(x) x.offset,self.data,'uni',0);
         offset = unique(vertcat(offset{:}),'rows');
         if size(offset,1) == 1
            bool = true;
         else
            bool = false;
         end
      end
      
      function set.labels(self,labels)
         % FIXME, prevent clashes with attribute names
         % FIXME, should check that labels are unique
         n = numel(self.data);
         if isempty(labels)
            for i = 1:n
               labels{1,i} = ['id' num2str(i)];
            end
            self.labels = labels;
         elseif iscell(labels)
            if numel(labels) == n
               if all(cellfun(@isstr,labels))
                  self.labels = labels;
               else
                  error('bad label');
               end
            else
               error('mismatch');
            end
         elseif (n==1) && ischar(labels)
            self.labels = {labels};
         else
            error('bad label');
         end
      end
      
      function self = sync(self,event,varargin)
         for i = 1:numel(self)
            cellfun(@(x) x.sync(event,varargin{:}),self(i).data,'uni',false);
         end
      end
      
      function proc = extract(self,dataType)
         % FIXME, for object array
         % FIXME, handle multiple returns???
         %keyboard
         for i = 1:numel(self)
            ind = cellfun(@(x) strcmp(class(x),dataType),self(i).data);
            proc{i} = self(i).data{ind};
         end
      end
      
      % reset
      % window
      % offset
   end
   
end