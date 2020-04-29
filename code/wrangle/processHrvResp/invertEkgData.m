function invertEkgData
%
% function invertEkgData
%
global EKG;

EKG.signal = -EKG.signal;
drawEkgPlot;

