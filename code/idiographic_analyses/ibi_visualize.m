
%analysis
load('allcouples_ibi_fits.mat')

%
%visualize results
VBA_ReDisplay(celldummy{1,1}, celldummy{1,2})

pars = NaN(size(celldummy,1), 4); %a1, a2, b1, b2

for i = 1:size(pars,1)
    pars(i,:) = celldummy{i,1}.muTheta;
end
