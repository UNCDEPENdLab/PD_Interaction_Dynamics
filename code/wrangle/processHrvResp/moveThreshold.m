function moveThreshold(x)
%
%
%

global EKG;
if x > 0  %move up
    EKG.threshold = EKG.threshold + EKG.threshold/20;  %move up 5%
else
    EKG.threshold = EKG.threshold - EKG.threshold/20;  %move down 5%
end

axes(findobj('Tag', 'AxesEkgPlot'));
% remove line if present
delete(findobj('Tag', 'LineEkgDataThreshold'))
xThreshold = [EKG.plot.startTime EKG.plot.endTime];
yThreshold = [EKG.threshold EKG.threshold];
line(xThreshold,yThreshold,'color','cyan','Tag', 'LineEkgDataThreshold');

    