function t = peakfinder(arr, thresh, sens)

% function t = peakfinder(arr, thresh,sens)
% This program finds the peaks given the array "arr" and the threshold 
% "thresh". It finds the maximum value between the threshold crossings.
% The sensitivity can be given as a third optional parameter. The 
% sensitivity "sens" determines the minimum number of samples that the 
% peaks must be placed apart.
%
% This program returns a matrix "t" that has the (x,y) of all the peaks.


if(nargin < 2)
   disp('Error: incorrect number of input arguments.')
   help peakfinder.m
   return
end

arrlen=length(arr);
flag=0;
flag1=0;
j=1;

for i=1:arrlen
    if(arr(i)>=thresh && ~flag)
        flag=1;
        t(j,1)=i;
        t(j,2)=arr(i);
    end
    
    if(flag && arr(i)>t(j,2))
        t(j,1)=i;
        t(j,2)=arr(i);
    end
    
    if(flag && arr(i)<thresh)
        flag=0;
        j=j+1;
        flag1=1;
    end
    
    if(j>2 && flag1 && nargin==3)
            if(t(j-1,1)-t(j-2,1)<sens)
                if(t(j-2,2)<=t(j-1,2))
                    t(j-2,1) = t(j-1,1);
                end
                t(j-2,2)=max(t(j-1,2),t(j-2,2));
                t(j-1,:)=[];
                j=j-1;
            end
            flag1=0;
        end
end
