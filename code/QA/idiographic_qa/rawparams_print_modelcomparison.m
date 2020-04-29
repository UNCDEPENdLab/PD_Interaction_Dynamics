rawparameters = NaN([size(outputs), 6]); %not sure what the parameters should be 
for i = 1:125
    for j = 1:4
        logEvidence(i,j) = outputs{i,j}.F;
        rawvec = [posteriors{i,j}.muTheta];
        rawparameters(i,j,1:length(rawvec)) = rawvec;
        if ismember('transformed', fields(posteriors{i,j}))
            parvec = struct2array(posteriors{i,j}.transformed);
            parameters(i,j,1:length(parvec)) = parvec;
        end
    end
end
logEvidence = logEvidence';
save('modelcomparison_logevidence.mat', 'logEvidence', 'ids', 'models', 'rawparameters');


sfrawparams = rawparameters(:,1:4,:);
sfrawparamsm1 = rawparameters(:,1,:);
sfrawparamsm2 = rawparameters(:,2,:);
sfrawparamsm3 = rawparameters(:,3,:);
sfrawparamsm4 = rawparameters(:,4,:);
save('modelcomparison_rawparams.mat', 'sfrawparams', 'sfrawparamsm1', 'sfrawparamsm2', 'sfrawparamsm3', 'sfrawparamsm4', 'ids');
[BMCposterior,BMCout] = VBA_groupBMC(logEvidence(:,:));