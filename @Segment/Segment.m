% collection of processes defined by common start and end time
% o Probably should place tStart/tEnd
% o must check for common start and end times!

% o methods for 
%   o adding processes
%   o 
classdef(CaseInsensitiveProperties = true) Segment < hgsetget & matlab.mixin.Copyable
   properties
      info@containers.Map % Information about segment
   end
   properties%(Hidden=true)%(GetAccess=private)
      %pointProcesses@PointProcess
      %sampledProcesses@SampledProcess
      data
   end
   properties(Dependent=true)
      dataType
      window
      sameWindow
      sameOffset
   end
   
   methods
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %% Constructor
      function self = Segment(varargin)
         
         % if all inputs are of type PointProcess or SampledProcess,
         % cat and add (no need to pass in paramvalue)
         
         p = inputParser;
         p.KeepUnmatched= false;
         p.FunctionName = 'Segment constructor';
         p.addParamValue('info',containers.Map('KeyType','char','ValueType','any'));
         p.addParamValue('pointProcesses',[]);
         p.addParamValue('sampledProcesses',[]);
         p.parse(varargin{:});
         
         self.info = p.Results.info;
%          if ~isempty(p.Results.pointProcesses)
%             self.pointProcesses = p.Results.pointProcesses;
%          end
%          self.sampledProcesses = p.Results.sampledProcesses;
%          self.data = {self.pointProcesses self.sampledProcesses};
         self.data = {};
         if ~isempty(p.Results.pointProcesses)
            self.data = [self.data {p.Results.pointProcesses}];
         end
         if ~isempty(p.Results.sampledProcesses)
            self.data = [self.data {p.Results.sampledProcesses}];
         end
      end% constructor
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
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
      
      function self = sync(self,event,varargin)
         for i = 1:numel(self)
            cellfun(@(x) x.sync(event,varargin{:}),self(i).data);
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