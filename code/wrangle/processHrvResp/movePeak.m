function movePeak(direction)
%
% function movePeak(direction)
%
% direction 8=up; 2=down; 4=left; 6=right


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

EKG.t_peaks = EKG.peaks(:,1)/EKG.sampRate;

switch(direction)
    case 8
        EKG.peaks(selected_peak,2) = EKG.peaks(selected_peak,2) + EKG.plot.incrUpDn;
    case 2
        EKG.peaks(selected_peak,2) = EKG.peaks(selected_peak,2) - EKG.plot.incrUpDn;
    case 6
        EKG.peaks(selected_peak,1) = EKG.peaks(selected_peak,1) + EKG.plot.incrLR;
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
    case 4
        EKG.peaks(selected_peak,1) = EKG.peaks(selected_peak,1) - EKG.plot.incrLR;
		EKG.t_peaks = EKG.peaks(:,1)/EKG.sampRate;
		%update indxPeaks
		EKG.indxPeaks = find(EKG.t_peaks >= EKG.plot.startTime & EKG.t_peaks <= EKG.plot.endTime); 
		EKG.ibis = 1000*diff(EKG.t_peaks); %ibi in milliseconds
		y = EKG.ibis;  %leave in ms
		x=EKG.t_peaks(2:end);
		t=(x-x(1)); %x in seconds; second beat at t=0
		tMax = round(t(end));
		xx=0:0.1:tMax;        % ibi interpolated to 10 Hz
		yy = spline(t,y,xx);
		EKG.ibi_spline = yy;
		EKG.ibi_spline_t = xx+x(1);
   otherwise
        disp(['Invalid direction: ' num2str(direction)]);
        return
end

%----------------------------move by click----------------------------

%---------------------  have to add bottondown on ekg axis
%-----------------add if selected "o" is red----------------
%-----------------------you might have to make a new function, such as
%moreSelected peak

% newPeakPosition  = get(gca,'CurrentPoint');
% newPeakPositionX = get(gca,'CurrentPoint');
% newPeakPositionY = get(gca,'CurrentPoint');
% 
% if ((EKG.peaks(selected_peak-1,1)/EKG.sampRate) < newPeakPositionX(1,1) < (EKG.peaks(selected_peak+1,1)/EKG.sampRate))  
%          EKG.peaks(selected_peak,2) = newPeakPositionY(1,2);
%   else
%              warndlg('New peak is out of order! Please try again!','Warning!');
% end
% 
% a = selected_peak
% a_1 = selected_peak + 1
% a_2 = selected_peak -1
% 
% b = EKG.peaks(selected_peak,1)
% c = EKG.peaks(selected_peak,2)

%-----------------------------------------------------------------------



drawIbiPlot;
drawEkgPlot;
drawPsdPlot;

