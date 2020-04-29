addpath(genpath('/Users/ams939/Downloads/VBA-toolbox-master'));
load('/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/IBImodels_logevidence_feb2017s.mat');
rawparamsm9 = rawparameters(:,9,:)
rawparamsm4 = rawparameters(:, 4,:)
save('paramsm9_VARCoreg.mat', 'rawparamsm9', 'rawparamsm4', 'ids')

%load('/Users/alisonmarie526/Desktop/feb2018_VAR_muThetas.mat');
load('/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/feb2018_VAR_muThetas.mat');
load('/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/feb2018_VAR_logEvidence.mat');
rawparamsm1 = zeros(125,4);
for i=1:125
    for k = 1:4
    rawparamsm1(i,k) = muThetas{i,1}(k);
    end
end
save('feb2018_params_VARCoreg_m1.mat', 'rawparamsm1', 'ids', 'logEvidence');


%load('/Users/alisonmarie526/Desktop/vanBse_fitallibimodels_feb2018_logevidence.mat');
load('/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/scratch/vanBse_fitallibimodels_feb2018_logevidence.mat');
vanbse_rawparamsm1 = zeros(125,6);
for i = 1:125
    for k = 1:6
        if (k < 5)
            vanbse_rawparamsm1(i,k) = rawparameters(i,1,k);
        elseif (k == 5)
            vanbse_rawparamsm1(i,k) = logEvidence(1,i);
        else 
            vanbse_rawparamsm1(i, k) = R2_evidence(i,1);
        end
    end
end
save('vanbse_rawparamsm1.mat', 'vanbse_rawparamsm1', 'ids');
            
%load('/Users/alisonmarie526/Desktop/VAR_output/fitallibimodels_feb2018_logevidence.mat');
load('/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/VAR_output/fitallibimodels_feb2018_logevidence.mat');
VAR_rawparamsm1 = zeros(125,6);
VAR_rawparamsm2 = zeros(125, 8);
for i = 1:125
    for k = 1:6
        if (k < 5)
            VAR_rawparamsm1(i,k) = rawparameters(i,1,k);
        elseif (k == 5)
            VAR_rawparamsm1(i,k) = logEvidence(1,i);
        else 
            VAR_rawparamsm1(i, k) = R2_evidence(i,1);
        end
    end
end

for i = 1:125
    for k = 1:8
        if (k < 6)
            VAR_rawparamsm2(i,k) = rawparameters(i,2,k);
        elseif (k == 7)
            VAR_rawparamsm2(i,k) = logEvidence(2,i);
        else 
            VAR_rawparamsm2(i, k) = R2_evidence(i,2);
        end
    end
end

save('VAR_rawparamsm1m2.mat', 'VAR_rawparamsm1', 'VAR_rawparamsm2','ids');
a = load('/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/modelcomparison_logevidence.mat')  
ll = a.logEvidence
b = load('/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/VAR_output/fitallibimodels_feb2018_logevidence.mat');
ll2 = b.logEvidence
ll_total = zeros(6, 125);
for i = 1:125
    id = a.ids(i)
    if (id < 8040) 
    for k = 1:6
        if (k < 5)
            ll_total(k, i) = ll2(k,i);
        elseif (k == 6) 
            ll_total(k,i) = ll(1, i);
        else 
            ll_total(k,i) = ll(2, i);
        end
    end 
    elseif (id < 8048 & id > 8040) 
    for k = 1:6
        mm = i + 1;
        if (k < 5)
            ll_total(k, mm) = ll2(k,i);
        elseif (k == 6) 
            ll_total(k,mm) = ll(1, i);
        else 
            ll_total(k,mm) = ll(2, i);
        end
    end 
    elseif (id > 8048) 
        for k = 1:6
        mm = i -2;
        a.ids(i)
        b.ids(i)
        if (k < 5)
            ll_total(k, mm) = ll2(k,i);
        elseif (k == 6) 
            ll_total(k,mm) = ll(1, i);
        else 
            ll_total(k,mm) = ll(2, i);
        end
        end 
    end
end
badid = [8000 8001 8021 8035 8040 8073 8100 8104]
ids = b.ids
badids = ismember(ids, badid)
ll2onlygoodids = ll2(:,badids == 0)
a = load('/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/modelcomparison_logevidence.mat')  
ll = a.logEvidence
llonlygoodids = ll(1:2, badids == 0)
allllonlygoodids = [ll2onlygoodids; llonlygoodids]
xx = VBA_groupBMC(allllonlygoodids)
y  = VBA_groupBMC(ll2onlygoodids)
a = load('/Users/alisonmarie526/Desktop/Files_Directory/Archive_April2017/vanBse_modelcomparison_logevidence.mat')  
ll = a.logEvidence
b = load('/Users/alisonmarie526/Desktop/vanBse_fitallibimodels_feb2018_logevidence.mat');
ll2 = b.logEvidence
ll_total = zeros(6, 125);
for i = 1:125
    id = a.ids(i)
    if (id < 8040) 
    for k = 1:6
        if (k < 5)
            ll_total(k, i) = ll2(k,i);
        elseif (k == 6) 
            ll_total(k,i) = ll(1, i);
        else 
            ll_total(k,i) = ll(2, i);
        end
    end 
    elseif (id < 8048 & id > 8040) 
    for k = 1:6
        mm = i + 1;
        if (k < 5)
            ll_total(k, mm) = ll2(k,i);
        elseif (k == 6) 
            ll_total(k,mm) = ll(1, i);
        else 
            ll_total(k,mm) = ll(2, i);
        end
    end 
    elseif (id > 8048) 
        for k = 1:6
        mm = i -2;
        a.ids(i)
        b.ids(i)
        if (k < 5)
            ll_total(k, mm) = ll2(k,i);
        elseif (k == 6) 
            ll_total(k,mm) = ll(1, i);
        else 
            ll_total(k,mm) = ll(2, i);
        end
        end 
    end
end


% rawparamsm2 = zeros(125,5);
% for i=1:125
%     for k = 1:5
%     rawparamsm1(i,k) = muThetas{i,2}(k);
%     end
% end


% rawparamsm3 = zeros(125,7);
% for i=1:125
%     for k = 1:7
%     rawparamsm1(i,k) = muThetas{i,3}(k);
%     end
% end
%save('feb2018_params_VARCoreg.mat', 'rawparamsm1', 'rawparamsm2','rawparamsm3','logEvidence', 'ids')



