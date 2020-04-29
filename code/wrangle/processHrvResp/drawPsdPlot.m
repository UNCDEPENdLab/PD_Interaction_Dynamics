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

% delete text if already present
h = findobj(gca,'Type','text');
if ~isempty(h),delete(h),end

indx = find(f<=0.5);  %freqs up to 0.5 Hz
line(f(indx),pyy(indx));
maxY = max(pyy(indx));
set(findobj('Tag', 'AxesPsdPlot'), 'XLim', [0 0.5],'ylim',[0 ceil(1.5*maxY)]);
% 30-July-2007 added ability to change HF lower and upper bounds
% draw LF lower boundary at 0.04 Hz
% draw HF lower and upper bounds at HF_lower and HF_upper
line([0.04 0.04],[0.00 1e8],'color',[1 1 0]);  %
line([EKG.HF_lower EKG.HF_lower],[0.00 1e8],'color',[1 1 0]);
HF_lower_text = ['HF_L = ' sprintf('%4.2f',EKG.HF_lower)];
text(EKG.HF_lower,1.2*maxY,HF_lower_text);
line([EKG.HF_upper EKG.HF_upper],[0.00 1e8],'color',[1 1 0]);
HF_upper_text = ['HF_U = ' sprintf('%4.2f',EKG.HF_upper)];
text(EKG.HF_upper,maxY,HF_upper_text);
%compute RSA and put it in handles.rsa edit box
%integrate power between EKG.HF_lower and EKG.HF_upper Hz
indx = find(f>=0.04 & f<=EKG.HF_lower);
meanP = mean(pyy(indx));
lf = log(meanP*(EKG.HF_lower-0.04));
lfstr = sprintf('%6.3f',lf);
indx = find(f>=EKG.HF_lower & f<=EKG.HF_upper);
meanP = mean(pyy(indx));
hf = log(meanP*(EKG.HF_upper-EKG.HF_lower));
hfstr = sprintf('%6.3f',hf);
lfhfstr = sprintf('%6.3f',lf/hf);
% HR = number of beats / (time for beats in ms/(60000ms/minute))  i.e. beats per minute
% HR = number of beats x one minute/(time for beats to occur)
HR = length(EKG.ibis)*60000/sum(EKG.ibis);
hrstr = sprintf('%6.2f',HR);

set(findobj('Tag', 'LfEditBox'), 'String', lfstr);
set(findobj('Tag', 'HfEditBox'), 'String', hfstr);
set(findobj('Tag', 'LfHfEditBox'), 'String', lfhfstr);
set(findobj('Tag', 'HrEditBox'), 'String', hrstr);

%---------------------------------RSP PSD---------------------

if ~isempty(EKG.RSP.signal)
    
ys = EKG.RSP.signal(floor(EKG.RSPpointDown*EKG.sampRate)+1:ceil(EKG.RSPpointUp*EKG.sampRate)); %data of interest between upper and lower bounds
ysm = detrend(ys);   % detrend
ysm = ysm - mean(ysm); % demean
nsamps = length(ysm);
[psy,f] = pwelch(ysm,nsamps,0,nsamps,EKG.sampRate); %all samples Hamming
axes(findobj('Tag', 'AxesRSPPsdPlot'));

% delete line if already present
h = findobj(gca,'Type','line');
if ~isempty(h),delete(h),end

% % delete text if already present
% h = findobj(gca,'Type','text');
% if ~isempty(h),delete(h),end

indx = find(f<=0.5);  %freqs up to 0.5 Hz
line(f(indx),psy(indx), 'color', [1 0 0]);
end
