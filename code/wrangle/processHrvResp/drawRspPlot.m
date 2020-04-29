function darwRspPlot

%
% function drawRspPlot
%
% must be called after Resp

global EKG;

startSamp = floor(EKG.plot.startTime*EKG.sampRate) + 1; 
endSamp = min(floor(EKG.plot.endTime*EKG.sampRate),length(EKG.signal)); 

rspMin = min(EKG.RSP.signal(startSamp:endSamp));
if rspMin >= 0
    rspMin = floor(0.9*rspMin);
else
    rspMin = floor(1.1*rspMin);
end
rspMax = ceil(1.1*max(EKG.RSP.signal));

axes(findobj('Tag', 'AxesRspPlot'));

% remove line if present
delete(findobj('Tag', 'LineRspData'))

set(findobj('Tag', 'AxesRspPlot'), 'XLim', [floor(EKG.plot.startTime) ceil(EKG.plot.endTime)]);
set(findobj('Tag', 'AxesRspPlot'), 'YLim', [rspMin rspMax]);

set(get(findobj('Tag', 'AxesRspPlot'), 'Title'), 'String', EKG.RSP.inFile, 'Interpreter', 'none' )
cf = gcf;
axes2 = findobj('Tag', 'AxesRspPlot');
t = linspace(EKG.plot.startTime,min(EKG.plot.endTime,EKG.plot.maxTime),length(EKG.RSP.signal(startSamp:endSamp)));
line(t,EKG.RSP.signal(startSamp:endSamp),'Tag', 'LineRspData','color','red', 'Parent',axes2); 