function drawPsdPlot
%
% function drawPsdPlot
%
% must be called after initPsdPlot

global EKG;

yy = EKG.ibi_spline; %ibi spline interpolated to 10 samples/sec
yym = detrend(yy);   % detrend
yym = yym - mean(yym); % demean
nsamps = length(yym);
[pyy,f] = pwelch(yym,nsamps,0,nsamps,10); %all samples Hamming
axes(findobj('Tag', 'AxesPsdPlot'));
% delete line if already present
h = findobj(gca,'Type','line');
if ~isempty(h),delete(h),end
indx = find(f<=1);  %freqs up to 1 Hz
line(f(indx),pyy(indx));
set(findobj('Tag', 'AxesPsdPlot'), 'XLim', [0 0.5]);
%compute RSA and put it in handles.rsa edit box
%integrate power between 0.15 and 0.4 Hz
indx = find(f>=0.04 & f<=0.15);
meanP = mean(pyy(indx));
lf = log(meanP*(0.15-0.04));
lfstr = sprintf('%6.3f',lf);
indx = find(f>=0.15 & f<=0.40);
meanP = mean(pyy(indx));
hf = log(meanP*(0.40-0.15));
hfstr = sprintf('%6.3f',hf);
lfhfstr = sprintf('%6.3f',lf/hf);

set(findobj('Tag', 'LfEditBox'), 'String', lfstr);
set(findobj('Tag', 'HfEditBox'), 'String', hfstr);
set(findobj('Tag', 'LfHfEditBox'), 'String', lfhfstr);

