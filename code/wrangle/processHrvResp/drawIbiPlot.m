function drawIbiPlot
%
% function drawIbiPlot
%

global EKG;

% nothing to do if EKG.ibis is empty
if isempty(EKG.ibis)
    return
end

ibiMin = floor(0.9*min(EKG.ibis));
ibiMax = ceil(1.1*max(EKG.ibis));
axes(findobj('Tag', 'AxesIbiPlot'));

% remove 'o's if present
h = findobj(gca,'Type','text');
if ~isempty(h),delete(h),end
% remove line if present
delete(findobj('Tag', 'LineIbiData'))

set(findobj('Tag', 'AxesIbiPlot'), 'XLim', [floor(EKG.plot.startTime) ceil(EKG.plot.endTime)]);
set(findobj('Tag', 'AxesIbiPlot'), 'YLim', [ibiMin ibiMax]);

EKG.plot.endTime = EKG.plot.startTime + EKG.plot.widthTime;
spline_t_samps = find(EKG.ibi_spline_t >= EKG.plot.startTime & EKG.ibi_spline_t <= EKG.plot.endTime);
startSamp = floor(EKG.plot.startTime*10) + 1; 
endSamp = min(floor(EKG.plot.endTime*10),length(EKG.ibi_spline)); 
%t_plot = linspace(EKG.plot.startTime,EKG.plot.endTime,endSamp-startSamp+1);
line(EKG.ibi_spline_t(spline_t_samps),EKG.ibi_spline(spline_t_samps),'Tag', 'LineIbiData','color','blue');
% find peaks in plot time range
indxPeaks = find(EKG.t_peaks >= EKG.plot.startTime & EKG.t_peaks <= EKG.plot.endTime);
%no IBI for first beat; remove if present
if ~isempty(intersect(indxPeaks,1))
    indxPeaks = setxor(indxPeaks,1);
end
for iIBI = indxPeaks
    h = text(EKG.t_peaks(iIBI),EKG.ibis(iIBI-1),'o', ...
		'color','yellow','userdata','red', ...
		'HorizontalAlignment','center','VerticalAlignment','middle');
end

