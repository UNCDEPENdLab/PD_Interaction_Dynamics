function moveEkgScale(x)
%
%
%

global EKG;

t_width = floor(EKG.plot.endTime - EKG.plot.startTime);

if x < 0 
	EKG.plot.startTime = max(round(EKG.plot.startTime - t_width + 1),0);
	EKG.plot.endTime = EKG.plot.startTime + t_width;
else
	EKG.plot.endTime = min(round(EKG.plot.endTime + t_width - 1),ceil(EKG.plot.maxTime));
	EKG.plot.startTime = EKG.plot.endTime - t_width;
end
drawEkgPlot;
drawIbiPlot;

