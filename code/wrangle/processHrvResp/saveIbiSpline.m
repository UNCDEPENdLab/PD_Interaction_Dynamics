function saveIbiSpline
%
% function saveIbiSpline
%

global EKG;

ibi_out = zeros(length(EKG.ibi_spline),2);
ibi_out(:,2) = EKG.ibi_spline';
ibi_out(:,1) = 1000*EKG.ibi_spline_t';

%ibi_out(1,1) = 1000*EKG.time_second_beat;
%for iBeat = 2:length(EKG.ibi_spline)
%	ibi_out(iBeat,1) = ibi_out(iBeat-1,1) + EKG.ibi_spline(iBeat);
%end

outFile = [EKG.inFile '_ibi10Hz.txt'];
dlmwrite(outFile,ibi_out,'delimiter','\t','precision','%8.1f');
