function [rsaseries, countmissing] = DynRSAOneFile(ibi, low, high, resamp, tsHz, minMS)
%  Adapted from RSAActiheart. The following fields must be populated prior to running:
%
%       file = txt file containing one column of IBIs
%       low = .##; type the lower bound for the frequency range of interest (e.g., .15)
%       high = .##; type the higher bound for the frequency range of interest (e.g., .40)
%       resampHz: Frequency grid used for resampling into IBI series

if nargin < 2, low = .12; end
if nargin < 3, high = .40; end
if nargin < 4, resamp = 1; end %whether to resample onto grid
if nargin < 5, tsHz = 4; end
if nargin < 6, minMS = 0; end %used to ensure alignment between couples

%% window
% s(n,k) = sumI(ai)[sum(N-1)x(m+nL)hi(m)e^-j2pi(k/K)m]^2
now = 4; % found to work in Hannson 2006, 2007
Nw = tsHz*32; % window length for thirty seconds, when using 250 SR; use 31 so 16 is the center
SR = 1/tsHz*1000; %sampling rate

%define windowing function
[multipeak, a]=multipeakwind(Nw,now); % creates file

if resamp
    %any IBIs over 2000 milliseconds are replaced by the mean to avoid ridiculous estimates
    countmissing = length(find(ibi >= 2000));
    togetmean = ibi;
    togetmean(togetmean == 2000)=[];
    meanibi = mean(togetmean);
    ibi(ibi>=2000) = meanibi;
    
    Ndata=length(ibi); %nD is the number of occurrences in the series
    
    %integrate across time to obtain time index (just use cumsum)
    x=cumsum(ibi);

    xgrid = max(SR, minMS):SR:round(x(end));
    %xx = 250:SR:(round(x(end)));
    y= spline(x,ibi,xgrid);  % make from point-process to evenly distributed time poitns
else
    y = ibi; %already resampled externally
    countmissing = [];
end

%ensure that data are mean centered to avoid DC effects in filtering
y=y-mean(y);

% STFT
% spectrogram(x,window,noverlap,F,fs)

F= low:1/32:high;

%integer number of overlapping windows at 4Hz or 10Hz
%for now, this is hard-coded since the analytic solution is not clear.
if tsHz==4
    noverlap=124;
elseif tsHz==10
    noverlap=310;
end

for i = 1:4
    [S31,F31,T31,P32]=spectrogram(y,multipeak(:,i), noverlap, F, tsHz); %get the power (P)
    if i == 1
        RSA2 = zeros(size(P32)); %initiate
    end
    RSA2 = P32*a(i) + RSA2; %a weights add to 1
end

%even though it appears that the 4Hz version uses a 128 divisor because of the size of multipeak (Nw),
%switching to Nw leads to a divergence from the original scalingin the 10Hz no resample case.
%keep as fixed 128 divisor for now
%meanRSA = log(2*sum(RSA2)/Nw); %typical log 2*power;
meanRSA = log(2*sum(RSA2)/128); %typical log 2*power;
%scale by dividing by number of points (L) per Matlab fft documentation

rsaseries =vertcat(T31,meanRSA)';