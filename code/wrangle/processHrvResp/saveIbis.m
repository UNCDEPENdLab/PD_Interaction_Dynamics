function saveIbis
%
% function saveIbis
%

global EKG;

ibi_out = zeros(length(EKG.ibis),2);
ibi_out(:,2) = EKG.ibis';
ibi_out(1,1) = 1000*EKG.time_second_beat;
for iBeat = 2:length(EKG.ibis)
	ibi_out(iBeat,1) = ibi_out(iBeat-1,1) + EKG.ibis(iBeat);
end
outFile = [EKG.inFile '_ibis.txt'];
dlmwrite(outFile,ibi_out,'delimiter','\t','precision','%8.0f');
