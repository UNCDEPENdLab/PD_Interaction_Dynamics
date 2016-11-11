%analysis
load 'allcouples_ibi_fits copy.mat'


%visualize results
% for i = 1:size(celldummy,1)
%     VBA_ReDisplay(celldummy{i,1}, celldummy{i,2})
%     pause
% end


%grabs theta for each participant



pars = NaN(size(celldummy,1), 4); %a1, a2, b1, b2
 
for i = 1:size(pars,1)
    pars(i,:) = celldummy{i,1}.muTheta;
end
%array with average muTheta's
muThetaCombined = NaN(1,4);
for i = 1:4
    muThetaCombined(1,i) = mean(pars(i,:));
end
%grabs muX0 for each person
muX0Combined = NaN(size(celldummy,1), 2); %X01, X02
for i = 1:size(muX0Combined,1)
    muX0Combined(i,:) = celldummy{i,1}.muX0;
end
%array with average muX0's
muX0Avg = NaN(size(1,2));
for i = 1:2
    muX0Avg(1,i) = mean(muX0Combined(i,:));
end

muX0AvgT = muX0Avg';
%array with covariance btwn mu's
total = zeros(size(celldummy,1),8); 
celldummycorr = zeros(4,4);
for i = 1:size(celldummy,1)
    
    celldummycorr = cov2corr(celldummy{i,1}.SigmaTheta);
    total(i,1) = celldummycorr(1);
    total(i,2) = celldummycorr(2);
    total(i,3) = celldummycorr(5);
    total(i,4) = celldummycorr(6);
    total(i,5) = celldummycorr(11);
    total(i,6) = celldummycorr(12);
    total(i,7) = celldummycorr(15);
    total(i,8) = celldummycorr(16);

end  

totalcov = zeros(size(celldummy,1),8); 
celldummycov = zeros(4,4);
for i = 1:size(celldummy,1)
    
    celldummycov = celldummy{i,1}.SigmaTheta;
    totalcov(i,1) = celldummycov(1);
    totalcov(i,2) = celldummycov(2);
    totalcov(i,3) = celldummycov(5);
    totalcov(i,4) = celldummycov(6);
    totalcov(i,5) = celldummycov(11);
    totalcov(i,6) = celldummycov(12);
    totalcov(i,7) = celldummycov(15);
    totalcov(i,8) = celldummycov(16);

end  
% corr = zeros(size(celldummy,1), 4);
% for i = 1:size(celldummy,1)
%     celldummycorr = cov2corr(celldummy{i,1}.SigmaTheta);
%     corr(i,1) = celldummycorr(1);
%     corr(i,2) = celldummycorr(2);
%     corr(i,3) = celldummycorr(11);
%     corr(i,4) = celldummycorr(12);
% end
%  beta1 = corrcoef(corr(1), corr(2));
%  beta2 = corrcoef(corr(3), corr(4));

avg = zeros(1,8);
for i = 1:8
    avg(1,i) = mean(total(:,i));
end

alphas = zeros(1, size(celldummy,1));
for i = 1:size(celldummy,1)
    alphas(i) = celldummy{i,1}.a_alpha/celldummy{i,1}.b_alpha;
end
avgalpha = mean(alphas);

sigmas = zeros(1, size(celldummy,1));
for i = 1:size(celldummy,1)
    sigmas(i) = celldummy{i,1}.a_sigma/celldummy{i,1}.b_sigma;
end
avgsigma = mean(sigmas);

avgCov = zeros(1,8);
for i =1:8
    avgCov(1,i) = mean(totalcov(:,i));
end


thetasSim = zeros(1,4);
thetasSim(1) = muThetaCombined(1);
thetasSim(2) = avg(2)*muThetaCombined(1);
thetasSim(3) = muThetaCombined(3);
thetasSim(4) = avg(6)*muThetaCombined(3);
% muX02 = 0;
% for i= 1:size(celldummy,1)
%     muX02 = muX02 + celldummy{i,1}.muX0(2);
% end
% muX02avg = muX02/size(celldummy,1);
% muX0_all = [muX01avg; muX02avg];
% alpha = mean(celldummy(:,1);
% for i = 1:size(pars,