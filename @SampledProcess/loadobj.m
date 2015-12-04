function obj = loadobj(S)

obj = SampledProcess(...
   'tStart',S.tStart,...
   'tEnd',S.tEnd,...
   'Fs',S.Fs,...
   'info',S.info,...
   'values',S.values_{1},...
   'offset',S.offset_,...
   'window',S.window_,...
   'labels',S.labels,...
   'quality',S.quality);
obj.window = S.window;
obj.offset = S.offset;
% < v 0.4.0
% obj = SampledProcess(...
%    'tStart',S.tStart,...
%    'tEnd',S.tEnd,...
%    'Fs',S.Fs,...
%    'info',S.info,...
%    'values',S.values_,...
%    'offset',S.offset_,...
%    'window',S.window_,...
%    'labels',S.labels,...
%    'quality',S.quality);
% obj.window = S.window;
% obj.offset = S.offset;
