function processHRV(dataSource,parameter1,parameter2)
%
% function processHRV(dataSource,parameter1,parameter2)
%
% dataSource: must be 'text' or 'acq'
% parameter1: must be sampling rate if dataSource = 'text' or EKG channel if dataSource = 'acq' 
% parameter2: must be RESP channel if dataSource = 'acq' and respiration was recorded
%             can also be input when open resp file is clicked
%             not necessary for 'text' data
% examples:
%
% processHRV('acq',4);
% processHRV('acq',4,2);   EKG = channel 4; RESP = channel 2
% processHRV('text',1000);
%
% this function reads in a Biopac recorded .acq file with EKG data on specified channel
% or text data with specified sampling rate
% R peaks are automatically picked from the recording and displayed
% basic interactions allow peaks to be moved, deleted, or additional peaks added
% after all peaks are correctly located, interbeat intervals (ibi's) are computed and plotted
% a spline fit to the ibi's allows interpolation beats to fixed times
% the power spectral density of the fit curve is computed 
% respiratory sinus arrythmia (RSA) is computed by integrating the psd (ms^2/Hz) from 0.15-0.4 Hz
% giving a value in ms^2 and setting RSA = ln(value) which is normally in the range 4-14
%
% 6-Dec-2006 LLG added text file reading
%
% 16-Jan-2007 LLG added time of second beat so that output text files _ibi.txt and _ibi10Hz.txt have time column
%
% 21-May-2007 LLG corrected drawEkgPlot to plot selected peak in RED (was setting color and userdata to yellow); 
% made deletePeak redraw peaks similar to addPeak
%
% 30-July-2007 LLG added ability to move HF upper and lower bounds
%
% 15-July-2009 LLG added parameter for .acq respiration channel

% EKG is a structure with fields: 
% .inFile
% .sampRate
% .signal
% .ibis
% .peaks
% .parameter1   sample rate if text; EKG channel if acq
% .parameter2   RESP channel if acq  (optional)
% .t_peaks
% .ibi_spline
% .ibi_spline_t
% .time_second_peak
% .threshold
% .plot.startTime
% .plot.maxTime
% .plot.endTime
% .plot.widthTime
% .plot.incrUpDn
% .plot.incrLR
% .HF_lower
% .HF_upper
% .RSP.inFile
% .RSP.signal
% .dataSource
% .RSP.goodSignal
% .rspMax
% .rspMin
% .ekgMax
% .ekgMin
% .rspBoundExists   "1" if it exists, "0" if it does not exist
% .ekgPeakExists    "1" if it exists, "0" if it does not exist
% .RSPpointUp       upper bound of RSP data 
% .RSPpointUp       lower bound of RSP data 

global EKG;

% check input arguments
if nargin < 2
    disp('processHRV requires at least two arguments');
    help processHRV;
    return;
end
EKG.parameter1 = parameter1;
if nargin > 2
    EKG.parameter2 = parameter2;
else
    EKG.parameter2 = [];
end   
EKG.dataSource = lower(dataSource)
EKG.RSP.signal = [];

switch lower(dataSource)
    case 'acq'
        [fname,fpath]=uigetfile({'*.acq'});
        fileName = [fpath fname];
        pos = findstr(fileName,'.');
        if length(pos) > 1   % 21-May-2009
            EKG.inFile = fileName(1:pos(end)-1);
        elseif length(pos) == 1
            EKG.inFile = strtok(fileName,'.');
        else
            EKG.inFile = fileName;
        end
        try,
        	[ACQsampleRate,ACQtimeAxis,chanData] = readACQFile(EKG.inFile);
        catch,
            [ACQsampleRate,nVarSampleDivider,chanData] = readVarSampRateACQ(EKG.inFile);
        end
        if exist('nVarSampleDivider','var')
            EKG.sampRate = ACQsampleRate/nVarSampleDivider(parameter1);
        else
            EKG.sampRate = ACQsampleRate;
        end
        EKG.signal = chanData(parameter1,:);
    case 'text'
        [fname,fpath]=uigetfile('*.*');
        fileName = [fpath fname];
        EKG.signal = dlmread(fileName);
        pos = findstr(fileName,'.');
        if length(pos) > 1   % 21-May-2009
            EKG.inFile = fileName(1:pos(end)-1);
        elseif length(pos) == 1
            EKG.inFile = strtok(fileName,'.');
        else
            EKG.inFile = fileName;
        end
        EKG.sampRate = parameter1;
    otherwise
        disp([dataSource ' is not a valid data source']);
        help processHRV
        return
end
        

EKG.ibis = [];
EKG.peaks = [];
EKG.t_peaks = [];
EKG.time_second_peak = [];

EKG.threshold = std(EKG.signal);
EKG.plot.startTime = 0;
EKG.plot.maxTime = length(EKG.signal)/EKG.sampRate;
EKG.plot.endTime = EKG.plot.maxTime;
EKG.plot.widthTime = EKG.plot.maxTime;
EKG.plot.incrUpDn = EKG.threshold/20; %same as threshold movement
EKG.plot.incrLR = (EKG.plot.endTime-EKG.plot.startTime);  % 1% of plot width
EKG.HF_lower = 0.15;
EKG.HF_upper = 0.40;
EKG.RSPpointDown = EKG.plot.startTime;
EKG.RSPpointUp = EKG.plot.endTime;


initEkgPlot;
initEkgControl;
initIbiPlot;
initPsdPlot;
EKG.rspBoundExists = 0; %no rsp bound exits yet
EKG.ekgPeakExists = 0;  %no peaks found yet
findPeaks;
