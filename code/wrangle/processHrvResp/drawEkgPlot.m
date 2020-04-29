function drawEkgPlot
%
% function drawEkgPlot
%
% must be called after initEkgPlot
% update: 0 = no first call to drawEkgPlot; 1 = just update plot with new time range 

global EKG;

EKG.plot.endTime = EKG.plot.startTime + EKG.plot.widthTime;
startSamp = floor(EKG.plot.startTime*EKG.sampRate) + 1; 
endSamp = min(floor(EKG.plot.endTime*EKG.sampRate),length(EKG.signal)); 
%disp([num2str(startSamp)  ' endSamp: ' num2str(endSamp)]);
EKG.ekgMin = min(EKG.signal(startSamp:endSamp));
if EKG.ekgMin >= 0
    EKG.ekgMin = floor(0.9*EKG.ekgMin);
else
    EKG.ekgMin = floor(1.1*EKG.ekgMin);
end
EKG.ekgMax = ceil(1.1*max(EKG.signal));

axes(findobj('Tag', 'AxesEkgPlot'));


% remove line if present

delete(findobj('Tag', 'LineEkgData'))

set(findobj('Tag', 'AxesEkgPlot'), 'XLim', [floor(EKG.plot.startTime) ceil(EKG.plot.endTime)]);
set(findobj('Tag', 'AxesEkgPlot'), 'YLim', [EKG.ekgMin EKG.ekgMax]);

%set(get(findobj('Tag', 'AxesEkgPlot'), 'Title'), 'String', EKG.inFile, 'Interpreter', 'none' );
set(findobj('Tag', 'FigureEkgPlot'), 'Name', EKG.inFile); %add the file address on the figure name instead of the axes title

cf = gcf;
t = linspace(EKG.plot.startTime,min(EKG.plot.endTime,EKG.plot.maxTime),length(EKG.signal(startSamp:endSamp)));
line(t,EKG.signal(startSamp:endSamp),'Tag', 'LineEkgData','color','blue');

% draw peaks if there are any
if ~isempty(EKG.peaks)
	% find selected peak if there is one
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

	% remove 'o's if present
	h = findobj(gca,'Type','text');
	if ~isempty(h)
		delete(h);        
    end    
    
    
    EKG.indxPeaks = find(EKG.t_peaks >= EKG.plot.startTime & EKG.t_peaks <= EKG.plot.endTime); 
    for i = 1:length(EKG.indxPeaks)
        iPeak = EKG.indxPeaks(i);
%	    disp(num2str(iPeak));
		if iPeak == selected_peak
			EKG.hpeaks(iPeak) = text(EKG.t_peaks(iPeak),EKG.peaks(iPeak,2),'o', ...
				'color',[1 0 0],'userdata',[1 1 0], ...
				'HorizontalAlignment','center','VerticalAlignment','middle', ...
				'Tag',['peak' num2str(iPeak)], ...
				'buttondownfcn', ...
					['tmpstr = get(gco, ''userdata'');' ...
					 'set(gco, ''userdata'', get(gco, ''color''));' ...
					 'set(gco, ''color'', tmpstr); clear tmpstr;'] );
	    else
			EKG.hpeaks(iPeak) = text(EKG.t_peaks(iPeak),EKG.peaks(iPeak,2),'o', ...
				'color',[1 1 0],'userdata',[1 0 0], ...
				'HorizontalAlignment','center','VerticalAlignment','middle', ...
				'Tag',['peak' num2str(iPeak)], ...
				'buttondownfcn', ...
					['tmpstr = get(gco, ''userdata'');' ...
					 'set(gco, ''userdata'', get(gco, ''color''));' ...
					 'set(gco, ''color'', tmpstr); clear tmpstr;'] );
		end
    end
    % now plot threshold line
    xThreshold = [EKG.plot.startTime EKG.plot.endTime];
    yThreshold = [EKG.threshold EKG.threshold];
    line(xThreshold,yThreshold,'color','cyan','Tag', 'LineEkgDataThreshold');

end
%----------------------------------------darwRspPlot----------------------%

if ~isempty(EKG.RSP.signal)
  
 EKG.rspMin = min(EKG.RSP.signal(startSamp:endSamp));
if EKG.rspMin >= 0
    EKG.rspMin = floor(0.9*EKG.rspMin);
else
    EKG.rspMin = floor(1.1*EKG.rspMin);
end
EKG.rspMax = ceil(1.1*max(EKG.RSP.signal));

axes(findobj('Tag', 'AxesRspPlot'));

% remove line if present
delete(findobj('Tag', 'LineRspData'))

set(findobj('Tag', 'AxesRspPlot'), 'XLim', [floor(EKG.plot.startTime) ceil(EKG.plot.endTime)]);
set(findobj('Tag', 'AxesRspPlot'), 'YLim', [EKG.rspMin EKG.rspMax]);

cf = gcf;
axes2 = findobj('Tag', 'AxesRspPlot');
t = linspace(EKG.plot.startTime,min(EKG.plot.endTime,EKG.plot.maxTime),length(EKG.RSP.signal(startSamp:endSamp)));
line(t,EKG.RSP.signal(startSamp:endSamp),'Tag', 'LineRspData','color','red', 'Parent',axes2);    
axes(findobj('Tag', 'AxesEkgPlot'));
end;
%-------------------------------------------------------------%


