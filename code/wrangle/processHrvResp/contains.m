function yn = contains(a, v)

% function yn = contains(a, v)
%
% This function returns 0 if the single value v is not within the array a,
% or the index of v within a if it is.

yn = 0;
s = size(a, 2);
for i = 1:s
	if a(i) == v
		yn = i;
	end
end
