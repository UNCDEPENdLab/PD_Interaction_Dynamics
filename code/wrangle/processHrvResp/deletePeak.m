function deletePeak
%
% function deletePeak
%

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

% update peaks and ibis and remove selected peak from EKG plot
EKG.peaks(selected_peak,:) = [];


EKG.t_peaks = EKG.peaks(:,1)/EKG.sampRate;
EKG.ibis = 1000*diff(EKG.t_peaks); %ibi in milliseconds
y = EKG.ibis;   %leave in ms
x=EKG.t_peaks(2:end);
t=(x-x(1)); %x in seconds; second beat at t=0
tMax = round(t(end));
xx=0:0.1:tMax;        % ibi interpolated to 10 Hz
yy = spline(t,y,xx);
EKG.ibi_spline = yy;
EKG.ibi_spline_t = xx+x(1);

%update indxPeaks
EKG.indxPeaks = find(EKG.t_peaks >= EKG.plot.startTime & EKG.t_peaks <= EKG.plot.endTime); 

% remove 'o's if present
h = findobj(gca,'Type','text');
if ~isempty(h)
    delete(h);        
end

% replot peaks without deleted one
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
