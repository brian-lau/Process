% pulled out of pointProcess cause static method couldn't call itself?
% may as well use it in alignTimes

function validOffset = checkOffset(offset,n)
% Validate window, and replicate if necessary
% 
% % single offset
% [offset]
% 
% % one offset for each of n elements
% [offset(1)
%  offset(2)
%  offset(n)
%    ]
% 
% % aribitrary windows for each of n elements
% {
%   [offset(1,1)   [offset(1,2)   [start(1,n) end(1,n)]
%    offset(2,1)]   offset(2,2)]   start(2,2) end(2,2)]
%  }
% 
% For example, to use the same set of windows for n elements,
% checkWindow({[-6 0;0 6;-6 6]},n)
% 
if nargin == 1
   n = 1;
end

if iscell(offset)
   % Same offsets for each element
   if numel(offset) == 1
      offset(1:n) = offset;
   end
   % Different offsets for each element
   if numel(offset) == n
      for i = 1:n
         validOffset{1,i} = checkOffset(offset{i},length(offset{i}));
      end
   else
      error('Cell array offset must be {[nx2]} or [nObjs x 2]');
   end
else
   if numel(offset) == 1
      offset = offset(:);
      offset = repmat(offset,n,1);
   end
   if numel(offset) ~= n
      error('Array offset must be [1 x 2] or [nObjs x 2]');
   end
   validOffset = offset(:);
end
