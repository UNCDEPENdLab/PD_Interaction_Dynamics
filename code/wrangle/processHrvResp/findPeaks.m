function findPeaks
%
% function findPeaks
%
% 27-July-2007 added computation of third argument (minSampsBetweenPeaks) for peakfinder
% 400 was a good value for a sample rate of 1000 
global EKG;

minSampsBetweenPeaks = 400*EKG.sampRate/1000;

EKG.peaks = peakfinder(EKG.signal,EKG.threshold,minSampsBetweenPeaks);
while isempty(EKG.peaks)
    EKG.peaks = peakfinder(EKG.signal,EKG.threshold,minSampsBetweenPeaks);
end
    
EKG.t_peaks = EKG.peaks(:,1)/EKG.sampRate; %in seconds
axes(findobj('Tag', 'AxesEkgPlot'));

EKG.time_second_beat = EKG.t_peaks(2); %in seconds
EKG.ibis = 1000*diff(EKG.t_peaks); %ibi in milliseconds
y = EKG.ibis;   %leave in ms
x=EKG.t_peaks(2:end);
t=(x-x(1)); %x in seconds; second beat at t=0
tMax = round(t(end));
xx=0:0.1:tMax;        % ibi interpolated to 10 Hz
yy = spline(t,y,xx);
EKG.ibi_spline = yy;
EKG.ibi_spline_t = xx+x(1);
drawEkgPlot;
drawIbiPlot;
drawPsdPlot;
