%for cluster computing
%addpath(genpath('/storage/home/ams939/MATLAB/VBA-toolbox'));
%addpath(genpath('/storage/group/mnh5174_collab/Couples_Interaction/'));

%for local computing
addpath(genpath('/Users/ams939/Box Sync/DEPENd/Couples Data/code/VBA_System'));
addpath(genpath('/Users/ams939/Desktop/MATLAB/VBA-toolbox'));


files = dir('/Users/ams939/Box Sync/DEPENd/Couples Data/data/dyad_txt/fit_five');
files = files(3:end); %remove . and .. (first 2 elements)
celldummy = cell(length(files), 2);

ncpus=getenv('matlab_cpus');
if strcmpi(ncpus, '')
    ncpus=4;
    fprintf('defaulting to 4 cpus because matlab_cpus not set\n');
else
    ncpus=str2double(ncpus);
end

poolobj=parpool('local',ncpus); %just use shared pool for now since it seems not to matter (no collisions)

parfor i = 1:size(celldummy,1)
   filetofit = fullfile('/Users/ams939/Box Sync/DEPENd/Couples Data/data/dyad_txt/fit_five', files(i).name);
   [post, out] = fitCoupleSystem(filetofit);
   celldummy(i,:) = {post, out};
end

save('allcouples_ibi_fits.mat', 'celldummy', 'files');

delete(poolobj);

