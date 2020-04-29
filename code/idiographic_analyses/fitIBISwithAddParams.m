%make sure that latest people and only neg interaction and that
%align_patientpartner has been run on the data (R script already in
%existence)

%addpath(genpath('/storage/group/mnh5174_collab/lab_resources/VBA-toolbox'));
ncpus=getenv('matlab_cpus');
if strcmpi(ncpus, '')
    ncpus=4;
    fprintf('defaulting to 4 cpus because matlab_cpus not set\n');
else
    ncpus=str2double(ncpus);
end


ncpus = 1;
basedir = '/Users/alisonmarie526/Desktop/negint_caid_dyad_txt';
subjs = dir([basedir, '/*.txt']);
nsubjs = length(subjs);
ids = NaN(1, nsubjs);

% fullmodel = 'A_B_C_D_E_F_G_H_I_J_K';
% split = strsplit(fullmodel, '_');
% models = cell(1, length(split));
% for m = 1:length(split)
%     models{m} = strjoin(split(1:m), '_');
% end
models = {'A','B', 'C', 'D', 'E', 'F', 'G', 'H'};

%models = {'A_B_C_D_E_F_G_H_I_J_K'};

posteriors = cell(1, length(models));
outputs = cell(1, length(models));

poolobj = parpool('local', ncpus);

 
logEvidence = NaN(size(outputs));
parameters = NaN([size(outputs), 14]);
rawparameters = NaN([size(outputs), 14]);
    parfor i = 1:nsubjs
        filetofit = fullfile('/Users/alisonmarie526/Desktop/negint_caid_dyad_txt', subjs(i).name);        % filetofit =
        % fullfile('/storage/group/mnh5174_collab/couples_stage/ecg_raw/',
        % files(i).names);
        %ids(i) = str2douple(regexprep(nsubjs(i).name, '(...);
        ids(i) = str2double(regexprep(subjs(i).name,'(\d+)_caid_ibis.txt','$1'));
        fprintf('id: %d\n', ids(i));
        ipost = cell(1, length(models));
        iout = cell(1, length(models));
        id = ids(i);
        try
        for j = 1:length(models)
            [ipost{j}, iout{j}] = fitCoupleSystemAddParams(filetofit, 0, models{j}, id); %suppress figure (0)
        end
        catch err
            display('error fitting subject.');
            rethrow(err)
        end
        posteriors(i,:) = ipost;
        outputs(i,:) = iout;
    end
%catch err
%    disp('error in optimization. killing parpool');
%    delete(poolobj);
%    rethrow(err);
%end

delete(poolobj);

save('caid_modelcomparisonmodels.mat','posteriors', 'outputs', 'models', 'ids','-v7.3');





for i = 1:nsubjs
    for j = 1:length(models)
        logEvidence(i,j) = outputs{i,j}.F;
        rawvec = [posteriors{i,j}.muTheta; posteriors{i,j}.muPhi];
        rawparameters(i,j,1:length(rawvec)) = rawvec;
        if ismember('transformed', fields(posteriors{i,j}))
            parvec = struct2array(posteriors{i,j}.transformed);
            parameters(i,j,1:length(parvec)) = parvec;
        end
    end
end
logEvidence = logEvidence';
save('CAIDmodels_logevidence.mat', 'logEvidence', 'ids', 'models', 'parameters', 'rawparameters');





%just trying on 8069
