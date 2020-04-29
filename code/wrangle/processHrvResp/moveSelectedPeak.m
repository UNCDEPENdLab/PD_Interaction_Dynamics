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

newPeakPosition  = get(gca,'CurrentPoint');
newPeakPositionX = newPeakPosition(1,1);
newPeakPositionY = newPeakPosition(1,2);

% ------------------------if selected "o" is red execute the code bellow----------------

%  a = EKG.peaks(selected_peak,1)/EKG.sampRate
%     b = newPeakPositionX
    
if (length(selected_peak) >= 1);
    
    
% if there's more than one selected peak we have a problem
if length(selected_peak) > 1
    warndlg('More than one peak is selected. Try again.','Warning!');
    return
end

EKG.t_peaks = EKG.peaks(:,1)/EKG.sampRate;

% a1=(EKG.peaks(selected_peak-1,1)/EKG.sampRate)
% a2=newPeakPositionX
% a3=(EKG.peaks(selected_peak+1,1)/EKG.sampRate)

if (((EKG.peaks(selected_peak-1,1)/EKG.sampRate) < newPeakPositionX) &  (newPeakPositionX < (EKG.peaks(selected_peak+1,1)/EKG.sampRate)));
         EKG.peaks(selected_peak,1) = ceil(newPeakPositionX*EKG.sampRate);
         EKG.peaks(selected_peak,2) = ceil(newPeakPositionY);
         
         EKG.t_peaks = EKG.peaks(:,1)/EKG.sampRate;
            
	%update indxPeaks
		EKG.indxPeaks = find(EKG.t_peaks >= EKG.plot.startTime & EKG.t_peaks <= EKG.plot.endTime); 
		EKG.ibis = 1000*diff(EKG.t_peaks); %ibi in milliseconds
		y = EKG.ibis;   %leave in ms
		x=EKG.t_peaks(2:end);
		t=(x-x(1)); %x in seconds; second beat at t=0
		tMax = round(t(end));
		xx=0:0.1:tMax;  %ibi interpolated to 10 Hz
		yy = spline(t,y,xx);
		EKG.ibi_spline = yy;
		EKG.ibi_spline_t = xx+x(1);

% a = selected_peak
% a_1 = selected_peak + 1
% a_2 = selected_peak -1
% 
% c = EKG.peaks(selected_peak,1)
% ct = EKG.peaks(selected_peak,1)
% c1 = newPeakPositionX
% d = EKG.peaks(selected_peak,2)

drawIbiPlot;
drawEkgPlot;
drawPsdPlot;    

  else
             warndlg('New peak is out of order! Please try again!','Warning!');
end


end