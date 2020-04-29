function [ACQsampleRate,ACQtimeAxis,chanData] = readACQFile(filename)

% function [ACQsampleRate,chanData] = readACQFile(filename)	             
%               File:               processData.m  
%               Author:             Karthik Kalyanam
%               Date Created:       23rd July 2004
%               Date Last Modified: 4th August 2004
% This program reads the .acq file. Further details will be added to this
% section as the program evolves.
% 
% modified by Larry Greischar 18th June 2005
% returns ACQsampleRate and
% channel data in array chanData
% updated for version <= 42
%
% 1 July 2005  renamed readACQFile 
%
% 13 July 2005 updated to read variable sampling rates
%
%
% modified by Ram Subramanian 29th September 2005
%
% 25 Nov 2008 corrected line 240 to handle version 3.7.0 which is saved from
% Mac version 3.9.1 for windows
%

%load local_path;

% filename = 'test2008Nov13_win';
if(~exist([filename '.acq'], 'file'))
   error(['Data file ' filename '.acq does not exist or is not in current patheader.'])
end

%header = [];
    
[fid,message]=fopen([filename '.acq']);
if fid<2
      fclose (fid);
      error (message)
end

fseek(fid, 0, 'eof');
filesize = ftell(fid);
frewind(fid);

% -----------------------------------
% Reading Header Section
% -----------------------------------

header.nItemHeaderLen  = fread(fid,1,'short');  % offset 0 -not used
header.lVersion= fread(fid,1,'long');  % -file version 

% As on 4th August 2004, we are using Acq 3.7.3.
% 39 = version of Acq 3.7.3 or above (Win 98, 98SE, 2000, Me, XP) 

% As on 30th June 2005, we are using Acq 3.8.1.
% 41 = version of Acq 3.8.1 or above (Win 98, 98SE, 2000, Me, XP) 
% 42 = version of BSL/PRO 3.7.x or above (Win 98, 98SE, 2000, Me, XP) 

if header.lVersion<30 |  header.lVersion>42
    error ('expecting version 2.0 -> 3.8.1')
end    

header.lExtItemHeaderLen= fread(fid,1,'long');  % offset 6 
header.nChannels =fread(fid,1,'short');         % offset 10
header.nHorizAxisType =fread(fid,1,'short');    % offset 12

%0 = Time in seconds 

header.nCurChannel  =fread(fid,1,'short');             % offset 14
header.dSampleTime  =fread(fid,1,'double'); %ms/sample % offset 16
header.dTimeOffset  =fread(fid,1,'double');            % offset 24
header.dTimeScale   =fread(fid,1,'double'); %ms/div    % offset 32
header.dTimeCursor1 =fread(fid,1,'double');            % offset 40
header.dTimeCursor2 =fread(fid,1,'double');            % offset 48 
header.rcWindow     =fread(fid,1,'int64');             % offset 56

for i=1:6
    header.nMeasurement(i)=fread(fid,1,'short');       % offset 64
end

%0 = No measurement 
%1 = Value Absolute voltage 
%2 = Delta Voltage difference 
%3 = Peak to peak voltage 
%4 = Maximum voltage 
%5 = Minimum voltage 
%6 = Mean voltage 
%7 = Standard deviation 
%8 = Integral 
%9 = Area 
%10 = Slope 
%11 = LinReg 
%13 = Median 
%15 = Time 
%16 = Delta Time 
%17 = Freq 
%18 = BPM 
%19 = Samples 
%20 = Delta Samples 
%21 = Time of Median 
%22 = Time of Max 
%23 = Time of Min 
%25 = Calculation 
%26 = Correlation 

header.fHilite  =fread(fid,1,'short');             % offset 76
%0 = Don't gray 
%1 = Gray. 

header.dFirstTimeOffset   =fread(fid,1,'double');  % offset 78
header.nRescale =fread(fid,1,'short');             % offset 86
%0 = Don't autoscale 
%1 = Autoscale. 

header.szHorizUnits1 =fread(fid,40,'*char')';
header.szHorizUnits2 =fread(fid,10,'*char')';

header.nInMemory  =fread(fid,1,'short');
%0 = Keep 
%1 = Don't keep. 

header.fGrid   =fread(fid,1,'short');
header.fMarkers   =fread(fid,1,'short');
header.nPlotDraft     =fread(fid,1,'short');
header.nDispMode     =fread(fid,1,'short');
%0 = Scope 
%1 = Chart. 

header.nReserved   =fread(fid,1,'short');

%version 3.0 and above
if header.lVersion>33

header.BShowToolBar =fread(fid,1,'short');
header.BShowChannelButtons=fread(fid,1,'short');
header.BShowMeasurements  =fread(fid,1,'short');
header.BShowMarkers  =fread(fid,1,'short');
header.BShowJournal  =fread(fid,1,'short');
header.CurXChannel  =fread(fid,1,'short');
header.MmtPrecision =fread(fid,1,'short');

end


%version 3.02 and above
if header.lVersion>34

header.NMeasurementRows = fread(fid,1,'short');

for i=1:40
    header.mmt(i)=fread(fid,1,'short');
end

for i=1:40
    header.mmtChan(i)=fread(fid,1,'short');
end
end

%version 3.5x and above
if header.lVersion>35

for i=1:40
    header.mmtCalcOpnd1(i)=fread(fid,1,'short');
end

for i=1:40
    header.mmtCalcOpnd2(i)=fread(fid,1,'short');
end

for i=1:40
    header.mmtCalcOp(i)=fread(fid,1,'short');
end

for i=1:40
    header.mmtCalcConstant(i)=fread(fid,1,'double');
end
end

%version 3.7.0 and above
if header.lVersion>36

header.bNewGridwithMinor = fread(fid,1,'long');
header.colorMajorGrid = fread(fid,1,'long');
header.colorMinorGrid = fread(fid,1,'long');
header.wMajorGridStyle=fread(fid,1,'short');
header.wMinorGridStyle =fread(fid,1,'short');
header.wMajorGridWidth=fread(fid,1,'short');
header.wMinorGridWidth=fread(fid,1,'short');
header.bFixedUnitsDiv  = fread(fid,1,'long');
header.bMid_Range_Show  = fread(fid,1,'long');
header.dStart_Middle_Point =fread(fid,1,'double');

for i=1:60
    header.dOffset_Point(i) =fread(fid,1,'double');
end

header.hGrid =fread(fid,1,'double');

for i=1:60
    header.vGrid(i) =fread(fid,1,'double'); 
end

header.bEnableWaveTools  =fread(fid,1,'long');                    
end

% version 3.7.3 and above
if header.lVersion>38
    header.horizPrecision=fread(fid,1,'short');   % offset 1894
end

% version 3.8.1 and above
if header.lVersion > 40
    reserved = fread(fid,10,'short');           % offset 1896
    bOverlapMode = fread(fid,1,'int32');
    bShowHardware = fread(fid,1,'int32');
    bXAutoPlot = fread(fid,1,'int32');
    bXAutoScroll = fread(fid,1,'int32');
    bStartButtonVisible = fread(fid,1,'int32');
    bCompressed = fread(fid,1,'int32');
    bAlwaysStartButtonVisible = fread(fid,1,'int32');
end
    
% -----------------------------------
% Per Channel Data Section
% -----------------------------------

%fseek(fid, 6, 'bof');
lExtItemHeaderLen = header.lExtItemHeaderLen;
nChannels = header.nChannels;

sectionstart = header.lExtItemHeaderLen;
fseek(fid, sectionstart, 'bof');
lChanHeaderLen = fread(fid, 1, 'int32');
fseek(fid, sectionstart+88, 'bof');
lBufLength = fread(fid, 1, 'int32');     % Number of samples, same for all channels
% if Version 3.7.0 or above get nVarSampleDivider
for i=1:nChannels
    sectionstart = lExtItemHeaderLen + (i-1)*lChanHeaderLen;
    if header.lVersion > 37                               %changed to 37 25-Nov-2008 to handle 3.7.0
        fseek(fid, sectionstart+250, 'bof');
        nVarSampleDivider(i) = fread(fid,1,'int16');
    end
    fseek(fid, sectionstart+6, 'bof');
    szCommentText(i,1:40) = fscanf(fid, '%40c', 1);  % This is the channel label
    fseek(fid, sectionstart+92, 'bof');
    dAmplScale(i) = fread(fid, 1, 'double');
    dAmplOffset(i) = fread(fid, 1, 'double');
end


% ------------------------------------
% Foreign Data Section
% ------------------------------------

fseek(fid, lExtItemHeaderLen+lChanHeaderLen*nChannels, 'bof');
nLength = fread(fid, 1, 'int16');

% ------------------------------------
% Per Channel Data Types Section
% ------------------------------------

sectionstart = lExtItemHeaderLen + lChanHeaderLen*nChannels + nLength;
fseek(fid, sectionstart, 'bof');
for i=1:nChannels
    nSize(i) = fread(fid, 1, 'int16');
    nType(i) = fread(fid, 1, 'int16');
end

sectionstart = lExtItemHeaderLen + lChanHeaderLen*nChannels + nLength + 4*nChannels; %added 23-Sep-2006

choffset = cumsum([0 nSize(1:end-1)]);  % Interleave offset

% ...then read samples, one channel at a time.
divSamp = unique(nVarSampleDivider);
if length(divSamp) == 1  % all channels have same sample rate use block read
    blocksize = sum(nSize);
    % put channel data in chanData
    %numChanSamples = lBufLength/nVarSampleDivider;
    chanData = zeros(nChannels,lBufLength);
    for k=1:nChannels
        fseek(fid, sectionstart + choffset(k), 'bof');
        switch nType(k)
            case 1  % double
                tmp = (fread(fid, lBufLength, 'double', blocksize-8));
                if size(tmp,2) == 1
                    chanData(k,:) = tmp';
                else
                    chanData(k,:) = tmp;
                end
            case 2  % int
                tmp = (dAmplScale(k) * fread(fid, lBufLength, 'int16', blocksize-2) + ... 
                                        dAmplOffset(k));
                if size(tmp,2) == 1
                    chanData(k,:) = tmp';
                else
                    chanData(k,:) = tmp;
                end
        end
    end
else  % must read complicated sample blocks
    repeatSize = max(divSamp);
    repeatBlock = zeros(1,repeatSize);
    repeatBlock(1) = sum(nSize); %first block has all channels
    for i=2:repeatSize
        sizeBlock = 0;
        for ichan = 1:nChannels
            if mod(i-1,nVarSampleDivider(ichan)) == 0
                sizeBlock = sizeBlock + nSize(ichan);
            end
        end
        repeatBlock(i) = sizeBlock;
    end
    repmatTimes = floor(lBufLength/repeatSize);
    blocksize = repmat(repeatBlock,1,repmatTimes);
    nLeftOver = lBufLength - repmatTimes*repeatSize;
    blocksize = [blocksize repeatBlock(1:nLeftOver)];

    sectionstart = lExtItemHeaderLen + lChanHeaderLen*nChannels + nLength + 4*nChannels;   
    fseek(fid, sectionstart, 'bof');
    % vector mapping of blocksizes 1-nSamp
    % blocksize = repmat([32 16 16 16 16 16 16 16],1,58961);
    % blocksize = [blocksize [32 16 16 16 16 16]];
    chanData = zeros(nChannels,lBufLength);
    % read all channels for each sample block
    for iSamp = 1:lBufLength
        if blocksize(iSamp) > 16  % read all channels
            for ichan=1:nChannels
                switch nType(ichan)
                    case 1  % double
                        chanData(ichan,iSamp) = fread(fid,1,'double');
                    case 2  % int
                        chanData(ichan,iSamp) = dAmplScale(ichan) * fread(fid,1,'int16') + dAmplOffset(ichan); 
                end
            end
        else   % read channels 1-8 only
            for ichan=1:nChannels-2
                switch nType(ichan)
                    case 1  % double
                        chanData(ichan,iSamp) = fread(fid,1,'double');
                    case 2  % int
                        chanData(ichan,iSamp) = dAmplScale(ichan) * fread(fid,1,'int16') + dAmplOffset(ichan); 
                end
            end
        end %if
    end %for
end %if length(divSamp)
    


% get length of all markers
markerHeaderOffset = sectionstart + lBufLength*blocksize;
fseek(fid,markerHeaderOffset,'bof');
lLength = fread(fid,1,'int32');
lMarkers = fread(fid,1,'int32');
lSample = fread(fid,1,'int16');
fSelected = fread(fid,1,'int16');
fTextLocked = fread(fid,1,'int16');
fPositionLocked = fread(fid,1,'int16');
nTextLength = fread(fid,1,'int16');


fclose(fid);

ACQsampleRate = 1000/header.dSampleTime;
ACQnumberSamples = lBufLength;
ACQsavedSamples = lBufLength;
ACQdata = chanData;
ACQtimeAxis = linspace(0, (ACQnumberSamples-1)/ACQsampleRate, ACQnumberSamples);

%eval(['save ' filename '_PARAM ACQsampleRate ACQnumberSamples ACQdata ACQtimeAxis']);
