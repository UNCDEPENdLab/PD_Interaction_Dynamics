%make sure that latest people and only neg interaction and that
%align_patientpartner has been run on the data (R script already in
%existence)
%change paths when get onto ics
addpath(genpath('/Users/alisonmarie526/Box Sync/DEPENd/Couples Data/code/VBA_System'));
ncpus=getenv('matlab_cpus');
if strcmpi(ncpus, '')
    ncpus=2;
    fprintf('defaulting to 2 cpus because matlab_cpus not set\n');
else
    ncpus=str2double(ncpus);
end
%change path once on ics
%basedir = '/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/dyad_txt/fit_five/8069_ibis.txt';
basedir = '/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/dyad_txt/fit_five';
subjs = dir([basedir, '/*.txt']);
nsubjs = length(subjs);
ids = NaN(1, 5);

% fullmodel = 'A_B_C_D_E_F_G_H';
% split = strsplit(fullmodel, '_');
% models = cell(1, length(split));
% for m = 1:length(split)
%     models{m} = strjoin(split(1:m), '_');
% end
% 
% clear m

models = {'A','B', 'C', 'D', 'E'};

posteriors = cell(length(nsubjs), length(models));
outputs = cell(length(nsubjs), length(models));

poolobj = parpool('local', ncpus);

try
    %for i = 1:nsubjs
    parfor i = 1:nsubjs
        filetofit = fullfile('/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/dyad_txt/fit_five', subjs(i).name);
        % filetofit =
        % fullfile('/storage/group/mnh5174_collab/couples_stage/ecg_raw/',
        % files(i).names);
        %ids(i) = str2douple(regexprep(nsubjs(i).name, '(...);
        ids(i) = str2double(regexprep(subjs(i).name,'(\d+)_ibis.txt','$1'));
        fprintf('id: %d\n', ids(i));
        ipost = cell(1, length(models));
        iout = cell(1, length(models));
        for j = 1:length(models)
            [ipost{j}, iout{j}] = march2017_fitIBISystem(filetofit, 0, models{j}); %suppress figure (0)
        end
        posteriors(i,:) = ipost;
        outputs(i,:) = iout;
    end
catch err
    disp('error in optimization. killing parpool');
    delete(poolobj);
    rethrow(err);
end

delete(poolobj);

save('fit5fivemodels.mat','posteriors', 'outputs', 'models', 'ids','-v7.3');

logEvidence = NaN(size(outputs));
parameters = NaN([size(outputs), 6]); % not sure about what the number should be
rawparameters = NaN([size(outputs), 6]); %not sure what the parameters should be 
for i = 1:nsubjs
    for j = 1:length(models)
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

save('IBIfitfivefivemodels_logevidence.mat', 'logEvidence', 'ids', 'models', 'parameters', 'rawparameters');


%VBA_groupBMC
badf = NaN(1,size(logEvidence, 2));
for i = 1:size(logEvidence, 2)
    badf(i) = any(logEvidence(:,i) < -70000);
end

goodEvidence = logEvidence(:, badf == 0);
goodIDs = NaN(1, size(goodEvidence, 2));
goodIDs = ids(:, badf==0);
[BMCposterior,BMCout] = VBA_groupBMC(goodEvidence([4 5],:));



save('goodEvidencebmc.mat', 'BMCposterior', 'BMCout', 'goodIDs');
