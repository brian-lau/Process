% FIND - Find events that match criteria
%
%     ev = find(EventProcess,varargin)
%
%     When input is an array of EventProcesses, will iterate find within each.
%     Optional parameter 'policy' controls how events are returned.
%
%     All inputs are passed in using name/value pairs. The name is a string
%     followed by the value (described below).
%     The order of the pairs does not matter, nor does the case.
%
% INPUTS
%     label   - string or metadata.Label, optional, default = first channel
%               Name or label of EventProcess channel to search within
%     eventType - string, optional
%               Class name of events to match against.
%     eventProp - string, optional, default = 'name'
%               metadata.Event property to match against.
%     eventVal - arbitrary, optional
%               Value of corresponding eventProp to use in defining subset.
%               isequal or == must be a valid method.
%     func    - function handle, optional
%               Input is the EventProcess object itself, and the function handle
%               should return a vector of booleans with size equal to the #
%               of events in EventProcess
%     logic   - string, optional, default = 'or'
%               One of {'or' 'any' 'and' 'all' 'not'} defining the logic to
%               be applied.
%     policy  - string, optional, default = 'first'
%               One of {'first' 'last' 'all'} indicating that the first,
%               last or all events matching criteria will be returned. In
%               the case of 'all', events are returned in cell array.
%     nansequal - boolean, optional, default = True
%               True indicates that NaNs should be treated as equal
%     strictHandleEq - boolean, optional, default = False
%               True indicates that handle compatible EventVals will
%               require that the handles match, not just the contents of
%               the handle object
%
% EXAMPLES
%     ev(1) = metadata.event.Stimulus('tStart',0.5,'tEnd',1,'name','fix');
%     ev(2) = metadata.event.Response('tStart',5,'tEnd',6,'name','button','modality','hand');
%     ev(3) = metadata.event.Stimulus('tStart',2,'tEnd',3,'name','cue');
%     ep = EventProcess('events',ev,'tStart',0,'tEnd',10);
%
%     ev = ep.find('eventType','metadata.event.Response') % event 2 by type
%     ev = ep.find('eventVal','cue') % event 3 by name
%     % event 3 by criteria
%     ev = ep.find('eventType','metadata.event.Stimulus','func',@(x) x.tStart >= 2) 
%     % no match by criteria
%     ev = ep.find('eventType','metadata.event.Stimulus','func',@(x) x.tStart >= 2,'logic','not') 

%     $ Copyright (C) 2016 Brian Lau <brian.lau@upmc.fr> $
%     Released under the BSD license. The license and most recent version
%     of the code can be found on GitHub:
%     https://github.com/brian-lau/Process

% TODO
% multiple windows
% multiple function handles?
function ev = find(self,varargin)

p = inputParser;
p.FunctionName = 'EventProcess find';
p.addParameter('label',[]);
p.addParameter('eventType',[],@ischar);
p.addParameter('eventProp','name',@(x) ischar(x) || iscell(x));
p.addParameter('eventVal',[]);
p.addParameter('func',[],@(x) isa(x,'function_handle'));
p.addParameter('logic','or',@(x) any(strcmp(x,{'any' 'or' 'union' 'all' 'and' 'intersection' 'not'})));
p.addParameter('policy','first',@(x) any(strcmp(x,{'first' 'last' 'all'})));
p.addParameter('nansequal',true,@islogical);
p.addParameter('strictHandleEq',false,@islogical);
p.parse(varargin{:});
par = p.Results;

nObj = numel(self);
for i = 1:nObj
   switch lower(par.policy)
      case {'first' 'last'}
         ev(i) = findEach(self(i),par);
      case {'all'}
         ev{i} = findEach(self(i),par);
   end
end

%%
function result = findEach(obj,par)

if isempty(par.label)
   events = obj.values{1};
else
   if ischar(par.label)
      ind = find(strcmp({obj.labels.name},par.label));
   else
      ind = find(obj.labels == par.label);
   end
   if any(ind)
      events = obj.values{1}(:,ind(1));
   else
      events = [];
   end
end
nev = numel(events);

if isempty(events)
   result = obj.null;
   return;
end

if ~isempty(par.eventType)
   eventTypeInd = strcmpi(arrayfun(@(x) class(x),events,'uni',0),par.eventType);
else
   eventTypeInd = false(nev,1);
end

if ~isempty(par.eventVal)
   if ischar(par.eventVal)
      v = arrayfun(@(x) strcmp(x.(par.eventProp),par.eventVal),events,'uni',0,'ErrorHandler',@valErrorHandler);
   else
      if par.nansequal && ~par.strictHandleEq
         % equality of numerics as well as values in fields of structs & object properties
         % NaNs are considered equal
         v = arrayfun(@(x) isequaln(x.(par.eventProp),par.eventVal),events,'uni',0,'ErrorHandler',@valErrorHandler);
      elseif ~par.nansequal && ~par.strictHandleEq
         % equality of numerics as well as values in fields of structs & object properties
         % NaNs are not considered equal
         v = arrayfun(@(x) isequal(x.(par.eventProp),par.eventVal),events,'uni',0,'ErrorHandler',@valErrorHandler);
      else
         % This will match handle references, ie. false even if contents match
         v = arrayfun(@(x) x.(par.eventProp)==par.eventVal,events,'uni',0,'ErrorHandler',@valErrorHandler);
      end
   end
   eventPropInd = vertcat(v{:});
else
   eventPropInd = false(nev,1);
end

if ~isempty(par.func)
   funcInd = arrayfun(par.func,events,'ErrorHandler',@funcErrorHandler);
else
   funcInd = false(nev,1);
end

switch lower(par.logic)
   case {'or' 'union' 'any'}
      selection = eventTypeInd | eventPropInd | funcInd;
   case {'not'}
      selection = ~(eventTypeInd | eventPropInd | funcInd);
   case {'and' 'intersection' 'all'}
      if isempty(par.eventType)
         eventTypeInd = true(nev,1);
      end
      if isempty(par.eventVal)
         eventPropInd = true(nev,1);
      end
      if isempty(par.func)
         funcInd = true(nev,1);
      end
      selection = eventTypeInd & eventPropInd & funcInd;
end

if all(~selection)
   ev = obj.null;
else
   ev = events(selection);
end

switch lower(par.policy)
   case {'first'}
      result = ev(1);
   case {'last'}
      result = ev(end);
   case {'all'}
      result = ev;
end

%%
function result = funcErrorHandler(err,varargin)
if strcmp(err.identifier,'MATLAB:noSuchMethodOrField');
   result = false;
else
   err = MException(err.identifier,err.message);
   cause = MException('EventProcess:find:func',...
      'Problem in function handle.');
   err = addCause(err,cause);
   throw(err);
end

function result = valErrorHandler(err,varargin)
if strcmp(err.identifier,'MATLAB:noSuchMethodOrField');
   result = false;
else
   err = MException(err.identifier,err.message);
   cause = MException('EventProcess:find:eventProp',...
      'Problem in eventProp/Val pair.');
   err = addCause(err,cause);
   throw(err);
end