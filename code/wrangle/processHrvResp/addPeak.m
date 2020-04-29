function addPeak(direction)
%
% function addPeak(direction)
%
% direction: -1 add peak to left of selected peak; 1 add to right

global EKG;

axes(findobj('Tag', 'AxesEkgPlot'));

% find 'o's if present
h = findobj(gca,'Type','text');

% find selected peak which is red and has 'yellow' stored in userdata
numPeaks = numel(h);
%disp(['Length(h): ' num2str(numPeaks)]);
selected_peak = [];

for iPeak = 1:numPeaks
	test = get(h(iPeak),'userdata');  %gets peaks from right to left
%    disp([num2str(iPeak) ':  ' mat2str(test)]);
    if sum(test == [1 1 0]) > 2   %yellow
        selected_peak = [selected_peak EKG.indxPeaks(numPeaks-(iPeak-1))];
    end
end
%disp(['Selected peak(s): ' mat2str(selected_peak)]);
% if there's more than one selected peak we have a problem
if length(selected_peak) > 1
    warndlg('More than one peak is selected. Try again.','Warning!');
    return
end
% if there's no selected peak we have a problem
if length(selected_peak) < 1
    warndlg('A peak must be selected. Try again.','Warning!');
    return
end

% EKG.indxPeaks contains the indices of the plotted peaks so selected peak is really EKG.indxPeaks(selected_peak)
%selected_peak = EKG.indxPeaks(selected_peak);
numPeaks = length(EKG.t_peaks);
%disp(['Selected peak(s): ' mat2str(selected_peak)]);

% add peak half way between selected peak and one before or after
if direction < 0  % add to left
    if selected_peak > 1
        previous_peak_time = EKG.t_peaks(selected_peak-1);
        add_peak_time = (EKG.t_peaks(selected_peak)+previous_peak_time)/2;
        add_peak_sample = round(add_peak_time*EKG.sampRate);
        add_peak_amplitude = (EKG.peaks(selected_peak,2)+EKG.peaks(selected_peak-1,2))/2;
        add_peak_indx = selected_peak;
    else % selected_peak = 1; use half the interval between peaks 1 and 2
        dif_peak_time = EKG.t_peaks(2)-EKG.t_peaks(1);
        add_peak_time = max((EKG.t_peaks(1)-dif_peak_time/2),0);
        add_peak_sample = round(add_peak_time*EKG.sampRate);
        add_peak_amplitude = (EKG.peaks(1,2)+EKG.peaks(2,2))/2; %mean of peaks 1 and 2
        add_peak_indx = selected_peak;
    end
%    % make a slot for the new peak pointer
%    for iPeak = numPeaks:-1:selected_peak
%        EKGplotParams.hpeak(iPeak+1) = EKGplotParams.hpeak(iPeak);
%	end
        
else % direction > 0 add to right
    if selected_peak < numPeaks
        next_peak_time = EKG.t_peaks(selected_peak+1);
        add_peak_time = (EKG.t_peaks(selected_peak)+next_peak_time)/2;
        add_peak_sample = round(add_peak_time*EKG.sampRate);
        add_peak_amplitude = (EKG.peaks(selected_peak,2)+EKG.peaks(selected_peak+1,2))/2;
        add_peak_indx = selected_peak + 1;
    else % selected_peak = numPeaks; use half the interval between last two peaks 
        dif_peak_time = EKG.t_peaks(numPeaks)-EKG.t_peaks(numPeaks-1);
        add_peak_time = min((EKG.t_peaks(numPeaks)+dif_peak_time/2),EKGplotParams.maxTime);
        add_peak_sample = round(add_peak_time*EKG.sampRate);
        add_peak_amplitude = (EKG.peaks(numPeaks,2)+EKG.peaks(numPeaks-1,2))/2; %mean of last two peaks
        add_peak_indx = selected_peak + 1;
    end
%    % make a slot for the new peak pointer
%    for iPeak = numPeaks:-1:selected_peak+1
%        EKGplotParams.hpeak(iPeak+1) = EKGplotParams.hpeak(iPeak);
%	end
end
%disp(['Add peak index: ' num2str(add_peak_indx)]);
% update peaks and ibis and add new peak to EKG plot
tmp = zeros(numPeaks+1,2);
old_peaks = setxor(add_peak_indx,[1:numPeaks+1]);
tmp(old_peaks,:) = EKG.peaks;
tmp(add_peak_indx,:) = [add_peak_sample add_peak_amplitude];
EKG.peaks = tmp;
clear tmp;
EKG.t_peaks = EKG.peaks(:,1)/EKG.sampRate;
%update indxPeaks
EKG.indxPeaks = find(EKG.t_peaks >= EKG.plot.startTime & EKG.t_peaks <= EKG.plot.endTime); 
EKG.ibis = 1000*diff(EKG.t_peaks); %ibi in milliseconds
y = EKG.ibis;   %leave in ms
x=EKG.t_peaks(2:end);
t=(x-x(1)); %x in seconds; second beat at t=0
tMax = round(t(end));
xx=0:0.1:tMax;        % ibi interpolated to 10 Hz
yy = spline(t,y,xx);
EKG.ibi_spline = yy;
EKG.ibi_spline_t = xx+x(1);

% remove 'o's if present
h = findobj(gca,'Type','text');
if ~isempty(h)
    delete(h);        
end

% replot peaks with new one added
for i = 1:length(EKG.indxPeaks)
	iPeak = EKG.indxPeaks(i);
%	    disp(num2str(iPeak));
	EKG.hpeaks(iPeak) = text(EKG.t_peaks(iPeak),EKG.peaks(iPeak,2),'o', ...
		'color',[1 1 0],'userdata',[1 0 0], ...
		'HorizontalAlignment','center','VerticalAlignment','middle', ...
		'Tag',['peak' num2str(iPeak)], ...
		'buttondownfcn', ...
			['tmpstr = get(gco, ''userdata'');' ...
			 'set(gco, ''userdata'', get(gco, ''color''));' ...
			 'set(gco, ''color'', tmpstr); clear tmpstr;'] );
end
drawIbiPlot;
drawPsdPlot;





        
    
