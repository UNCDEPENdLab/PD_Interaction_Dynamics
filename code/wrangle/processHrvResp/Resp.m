function Resp()

global EKG;

set (findobj('Tag', 'AxesEkgPlot'), 'Position',[0.08 0.11 0.85 0.4], ...
    'Color', 'none');  
%--------------------------------- initPlot-------------------------%

h = findobj('Tag', 'FigureEkgPlot');

h11 = axes('Parent',h, ...
   'CameraUpVector',[0 1 0], ...
   'Color', 'none', ...
   'Position',[0.08 0.51 0.85 0.4], ...
   'Tag','AxesRspPlot', ...
   'XColor',[1 0 0], ...
   'XGrid','on', ...
   'YColor',[1 0 0], ...
   'YAxisLocation','right',...
   'XAxisLocation','top', ...
   'YGrid','on',...
   'buttondownfcn','moveSelectedLine'); % callback moveSelectedLine when ever clicked on the axis
h2 = text('Parent',h11, ...
   'Color',[1 0 0], ...
   'HandleVisibility','off', ...
   'HorizontalAlignment','center', ...
   'Position',[-0.07 0.50 9.16], ...
   'Rotation',90, ...
   'String','Respiratory', ...
   'Tag','AxesRspPlotYLabel', ...
   'VerticalAlignment','baseline');
set(get(h2,'Parent'),'YLabel',h2); 
   h2 = text('Parent',h11, ...
   'Color',[0 0 0], ...
   'HandleVisibility','off', ...
   'HorizontalAlignment','center', ...
   'Position',[0.50 -0.07 9.16], ...
   'String','Respiratory Time (sec)', ...
   'Tag','AxesRspPlotXLabel', ... 
   'VerticalAlignment','cap');
set(get(h2,'Parent'),'XLabel',h2);

%--------------------------------- initPSDPlot-------------------------%

h1 = axes('Parent',findobj('Tag', 'FigurePsdPlot') , ...
   'CameraUpVector',[0 1 0], ...
   'Color', 'none', ...
   'Position',[0.10 0.11 0.8 0.8], ...
   'Tag','AxesRSPPsdPlot', ...
   'XColor',[0 0 0], ...
   'XGrid','off', ...
   'YColor',[0 0 0], ...
   'YGrid','off', ...
   'YAxisLocation','right');
h2 = text('Parent',h1, ...
   'Color',[0 0 0], ...
   'HandleVisibility','off', ...
   'HorizontalAlignment','center', ...
   'Position',[-0.07 0.50 9.16], ...
   'Rotation',90, ...
   'String','ms^2/Hz', ...
   'Tag','AxesRSPPsdPlotYLabel', ...
   'VerticalAlignment','baseline');
set(get(h2,'Parent'),'YLabel',h2);

%--------------------------------- getRspData-------------------------%

switch EKG.dataSource
    case 'acq'
        if isempty(EKG.parameter2)  % RESP channel not specified
            prompt = {'Enter RESP channel:'};
			dlg_title = 'Input RESP channel';
			num_lines = 1;
			answer = inputdlg(prompt,dlg_title,num_lines);
			EKG.parameter2 = str2num(answer{1});
		end
        [ffname,ffpath]=uigetfile({'*.acq'});
        ffileName = [ffpath ffname];
        EKG.RSP.inFile = ffileName(1:end-4);
        try,
        	[ACQsampleRate,ACQtimeAxis,chanData] = readACQFile(EKG.RSP.inFile);
        catch,
            [ACQsampleRate,nVarSampleDivider,chanData] = readVarSampRateACQ(EKG.RSP.inFile);
        end
        EKG.RSP.signal = chanData(EKG.parameter2,:);
        if length(EKG.RSP.signal) < length(EKG.signal)
            disp('Warning respiration signal is shorter than EKG signal');
            numAdd = length(EKG.signal) - length(EKG.RSP.signal);
            EKG.RSP.signal = [EKG.RSP.signal; repmat(EKG.RSP.signal(end),numAdd,1)];
        elseif length(EKG.RSP.signal) > length(EKG.signal)
            disp('Warning EKG signal is shorter than respiration signal');
            EKG.RSP.signal = EKG.RSP.signal(1:length(EKG.signal));
        end
    case 'text'
        [ffname,ffpath]=uigetfile('*.*');
        ffileName = [ffpath ffname];
        EKG.RSP.signal = dlmread(ffileName);
        if length(EKG.RSP.signal) < length(EKG.signal)
            disp('Warning respiration signal is shorter than EKG signal');
            numAdd = length(EKG.signal) - length(EKG.RSP.signal);
            EKG.RSP.signal = [EKG.RSP.signal; repmat(EKG.RSP.signal(end),numAdd,1)];
        elseif length(EKG.RSP.signal) > length(EKG.signal)
            disp('Warning EKG signal is shorter than respiration signal');
            EKG.RSP.signal = EKG.RSP.signal(1:length(EKG.signal));
        end
        EKG.RSP.inFile = ffileName(1:end-4);
    otherwise
        disp([dataSource ' is not a valid data source']);
        return
end

%--------------------------------- drawRspPlot-------------------------%
if ~isempty(EKG.RSP.signal)

iniLower = EKG.plot.startTime + floor(EKG.plot.endTime - EKG.plot.startTime)*1/3; %initial position of lower bound
iniUpper = EKG.plot.startTime + floor(EKG.plot.endTime - EKG.plot.startTime)*2/3; %initial position of upper bound



drawEkgPlot;  %darwRspPlot in drawEkgPlot needs to run before the following code to set the values of EKG.rspMin and EKG.rspMax

axes(findobj('Tag', 'AxesRspPlot'));

line([iniLower iniLower],[EKG.rspMin EKG.rspMax],'color',[1 1 1], 'userdata',[0 1 1], 'LineWidth', 1.25, ...
                     'parent',gca, 'Tag', 'lowerBound', ...
                     'buttondownfcn', ...
					['tmpstrr = get(gco, ''userdata'');' ...
					 'set(gco, ''userdata'', get(gco, ''color''));' ...
					 'set(gco, ''color'', tmpstrr); clear tmpstrr;'] );  %draw lower bound line

line([iniUpper iniUpper],[EKG.rspMin EKG.rspMax],'color',[1 1 1], 'userdata',[0 1 1], 'LineWidth', 1.25, ...
                     'parent',gca, 'Tag', 'upperBound',...
                     'buttondownfcn', ...
					['tmpstrr = get(gco, ''userdata'');' ...
					 'set(gco, ''userdata'', get(gco, ''color''));' ...
					 'set(gco, ''color'', tmpstrr); clear tmpstrr;'] );  %draw upper bound line
                 
EKG.rspBoundExists = 1; %line exists now

end


drawPsdPlot;
