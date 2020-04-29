%read in all data into cell array
addpath(genpath('/gpfs/group/mnh5174/default/lab_resources/VBA-toolbox'));
ncpus=getenv('matlab_cpus');
if strcmpi(ncpus, '')
    ncpus=2;
    fprintf('defaulting to 2 cpus because matlab_cpus not set\n');
else
    ncpus=str2double(ncpus);
end
%change path once on ics
%basedir = '/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/dyad_txt/fit_five/8069_ibis.txt';
%basedir = '/Users/alisonmarie526/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/data/dyad_txt';
basedir = '/gpfs/group/mnh5174/default/pd_interaction_dynamics_analyses_feb2017/negint_dyad_txt';
%myCluster = parcluster('local');
%myCluster.NumWorkers = ncpus;
%saveProfile(myCluster);
subjs = dir([basedir, '/*.txt']);
nsubjs = length(subjs);
ids = NaN(1, nsubjs);

% fullmodel = 'A_B_C_D_E_F_G_H';
% split = strsplit(fullmodel, '_');
% models = cell(1, length(split));
% for m = 1:length(split)
%     models{m} = strjoin(split(1:m), '_');
% end
%
% clear m

models = {'A','B', 'C', 'D'};

posteriors = cell(length(nsubjs), length(models));
outputs = cell(length(nsubjs), length(models));

%poolobj = parpool('local', ncpus);
y = cell(nsubjs, 1);
optionsfull = cell(nsubjs, 4);



for i = 1:nsubjs

    filetofit = fullfile('/gpfs/group/mnh5174/default/pd_interaction_dynamics_analyses_feb2017/negint_dyad_txt', subjs(i).name);
        % filetofit =
        % fullfile('/storage/group/mnh5174_collab/couples_stage/ecg_raw/',
        % files(i).names);
        %ids(i) = str2douple(regexprep(nsubjs(i).name, '(...);
        ids(i) = str2double(regexprep(subjs(i).name,'(\d+)_ibis.txt','$1'));
        fprintf('id: %d\n', ids(i));
    data = dlmread(filetofit);
    data =data(:, [3 4]);
    y_temp = data';
    [y{i}] = y_temp;
    id = ids;
    showfig = 0;
    for j = 1:length(models);
        if showfig == 1;
            options.DisplayWin = 1;
        else
            options.DisplayWin = 0;
        end

        options.isYout = zeros(size(y_temp));
        if (id == 8030)
            options.isYout(1,[1:20,970:990, 1000:1020, 1040:1060, 2805:2820, 2875:2890, 2900:2920, 2935:2950, 3000:3020, 4390:4410, 4655:4670, 4710:4730, 5240:5260, 5315:5330, 5345:5360, 5440:5460, 5740:5765, 6020:6040, 6050:6070]) = 1;
        elseif (id == 8032)
            options.isYout(1,[45:60, 320:370, 855:875, 895:915, 955:970, 1080:1100, 1215:1230, 1250:1265, 1285:1305, 1335:1350, 1350:1375, 2205:2220, 2230:2250, 2555:2300, 2330:2345, 2350:2370, 2620:2640, 2690:2710, 2715:2750, 3825:3840, 4265:4280, 4320:4340, 4370:4390, 4420:4460, 4605:4645, 5070:5085, 5115:5135, 5220:5290, 5405:5530, 5745:5760, 5785:5825, 5860:5875, 5935:5975]) = 1;
        elseif (id == 8034)
            options.isYout(2, 120:150) = 1;
            options.isYout(1, [2265:2285, 3260:3280, 4345:4365, 5065:5085]) = 1;
        elseif (id == 8039)
            options.isYout(2, [1:10, 4500:4515]) = 1;
        elseif (id == 8048) %8048l needs to be censored at 14.75-17; 446-451 (1475:1700, 44600-45100); partner
            options.isYout(2, [147:170, 4460:4510]) = 1;
        elseif (id == 8060)
            options.isYout(1,[560:575, 695:715, 1050:1070, 1575:1615, 1955:1970, 2315:2350, 3145:3190, 3315:3330, 3395:3415, 3530:3545, 3715:3730, 4265:4285, 4515:4530,4555:4570, 5130:5140,5940:5960, 5980:6000 ]) = 1; %add in later
        elseif (id == 8064)
            options.isYout(2, [1895:1935, 2040:2075, 5620:5650]) = 1;
        elseif (id == 8084)
            options.isYout(1, 5050:5070) = 1;
        elseif (id == 8100)
            options.isYout(1, [20:30, 400:420, 540:580, 1030:1050, 1070:1090, 1100:1120, 1240:1260, 1960:1980, 2055:2075, 2155:2175, 2255:2265, 2405:2425, 2445:2485, 2810:2830, 3150:3170, 3255:3275, 3475:3495, 3585:3625, 3935:3955, 4135:4155, 4295:4315, 4525:4555, 4755:4815, 4935:4980, 5205:5225, 5455:5490, 5775:5795, 5950:5970, 6035:6055]) = 1;
        elseif (id == 8106)
            options.isYout(1, [620:630, 1130:1140, 1330:1345, 1720:1735, 1915:1930, 2575:2805, 3370:3390, 3750:3765, 3855:3870, 4105:4505, 5000:5020, 5125:5140, 5190:5210, 5310:5390, 5590:5605]) = 1;
        elseif (id == 8114)
            options.isYout(2, [710:750, 1210:1225]) = 1;
        elseif (id == 8126)
            options.isYout(1, [1:80, 2950:2990, 5920:5960]) = 1;
        elseif (id == 8127)
            options.isYout(1, [120:140, 325:345, 415:435, 605:635, 755:790, 905:980, 1015:1055, 1195:1205, 1270:1290, 1450:1470, 1545:1570, 1710:1740, 1815:1830, 2120:2150, 2190:2220, 2260:2290, 2415:2440, 2465:2490, 3335:3410, 3570:3600, 4490:4590, 4835:4870, 5720:5740, 5780:5795, 5950:6008]) = 1;
        elseif (id == 8133)
            options.isYout(1, [2110:2135, 3140:3165, 3515:3590, 3950:3965, 4490:4665, 5155:5185, 5825:5920, 5950:5985]) = 1;
        elseif (id == 8134)
            options.isYout(2, 5880:5905) = 1;
        elseif (id == 8144)
            options.isYout(2, [35:230, 1830:1880, 2255:2290, 3710:3725, 5470:5495]) = 1;
        elseif (id == 8152)
            options.isYout(1, [4080:4100, 4700:4710, 4770:4800, 4880:4905, 5230:5250]) = 1;
        end


        %n_t = size(data, 1);
        dim.n = 2;%identity observation function
        %number of physiological signals
        %add in if statement so that dim.n_theta reflects the actual number of
        %parameters being estimated


        priors.muX0 = [y_temp(1,1); y_temp(2,1)]; %use observed initial values of IBI series
        priors.SigmaX0 = 100*eye(dim.n); %zero covariance in prior. sd of 10 in initial states (since it should be close to observed)
        %fit deterministic system
        %priors.a_alpha = Inf;
        %priors.b_alpha = 0;
        priors.a_alpha = 1e0;
        priors.b_alpha = 1e1;
        priors.a_sigma = 1e1;
        priors.b_sigma = 1e1;
        options.priors = priors;
        options.backwardLag = 16; %number of observations to consider in making prediction of next sample (lagged Kalman filter)
        delta_t = 0.1; %10Hz series

        inF.deltat=delta_t;
        options.n_t = size(data,1);
        options.MaxIter = 100;
        options.TolFun = 1e-3;
        options.GnMaxIter = 100;
        options.GnTolFun = 1e-3;




        %options.dim = dim;

%priors.muX0 = 0*ones(dim.n, 1); %initial IBI signal values. If not demeaned, might be avg IBI value
        %inF.models = model;
        if (j ==1)%traditional coreg with p1star and p2star estimated (so baseline can be other than 0)
            dim.n_theta = 6;
            inF.models = models{1};
            priors_group.muTheta = [.02893; .00318; .02942; -.00377; .00067; .00008];
            priors_group.SigmaTheta = [0.11649440 -0.19669229  0.042511560  0.036464188 -0.0107241149 -0.0125329274;-0.19669229  0.34468999 -0.072753560 -0.072493106  0.0161745368  0.0187105845; 0.04251156 -0.07275356  0.037345335 -0.007655287 -0.0027049989 -0.0024647670;0.03646419 -0.07249311 -0.007655287  0.043358072 -0.0024981199 -0.0035568543;-0.01072411  0.01617454 -0.002704999 -0.002498120  0.0035523529  0.0008840662;-0.01253293  0.01871058 -0.002464767 -0.003556854  0.0008840662  0.0033074457];
            elseif (j ==2) %traditional core
            dim.n_theta = 4;
            inF.models = models{2};
            priors_group.muTheta = [.02917; .00249; .02708; -.00377];
            priors_group.SigmaTheta = [1.2973928 -1.2907146  0.3162871 -0.1366203; -1.2907146  1.5924602 -0.1396698 -0.3406481;0.3162871 -0.1396698  2.1772255 -2.3187324;-0.1366203 -0.3406481 -2.3187324  2.7871806];
        elseif (j ==3) %new coreg with p1star and p2star estimated (so baseline can be other than 0)
            dim.n_theta = 6;
            inF.models = models{3};
            priors_group.muTheta = [.028915; .028903; .027895; -.00085; .00018];
            priors_group.SigmaTheta = [ 1.0648081890  0.0289549995  0.31199819 -0.0315524061  0.0003412199  0.0034414106; 0.0289549995  0.0263081448  0.32398961 -0.0237814360 -0.0001881858 -0.0006540782;0.3119981891  0.3239896101  6.00095679 -0.2733109024  0.0155737514  0.0229168397; -0.0315524061 -0.0237814360 -0.27331090  0.0223441794  0.0005485674  0.0009571537; 0.0003412199 -0.0001881858  0.01557375  0.0005485674  0.0011572683  0.0003567402;0.0034414106 -0.0006540782  0.02291684  0.0009571537  0.0003567402  0.0009104495];
        elseif (j==4) % new coregulation with p1star and p2star not estimated
            dim.n_theta = 4;
            inF.models = models{4};
            priors_group.muTheta = [.027744; .02989; .02708; .028168];
            priors_group.SigmaTheta = [2.11708557 -0.04639396  0.04297602  0.04389460;-0.04639396  0.08367330  0.01157175 -0.08223661;  0.04297602  0.01157175  4.13944204 -0.01354416; 0.04389460 -0.08223661 -0.01354416  0.08173164];
        end
        inF.p1star = 0;
        inF.p2star = 0;
        priors.muTheta = 0*ones(dim.n_theta, 1);
        priors.SigmaTheta = eye(dim.n_theta);%all coupling parameters are zero centered and max ~1.0
        n_t = options.n_t;
        u = zeros(1, n_t);
        options.u = u;
        options.priors = priors;
        options.priors_group = priors_group;
        dim.n_phi = 0;
        dim.n = 2;
        options.dim = dim;
        options.inF = inF;


        [optionsfull{i,j}] = options;
    end
end




%poolobj = parpool('myCluster');
poolobj = parpool('local', ncpus);
%for j = 1:length(models)
    %poolobj = parpool('local', ncpus);
    %ipost = cell(1, length(models));
    %iout = cell(1, length(models));
    j = 2
    options = cell(nsubjs, 1);
    for k = 1:nsubjs
        options{k} = optionsfull{k,j};
    end
    u = cell(nsubjs, 1);
    for l = 1:nsubjs
        u{l} = optionsfull{l,j}.u;
    end
    %dim = struct();
    %for m = 1:nsubjs
    dim = optionsfull{1, j}.dim;
    priors_group = optionsfull{1,j}.priors_group;
    %end
    [p_sub, o_sub, p_group, o_group] = mfx_modelcomparison_fitIBISystem(y, options, u, dim, priors_group);
    if (j==1)
    filename = 'mfx_model1.mat';
    elseif (j ==2)
    filename = 'mfx_model2.mat';
    elseif (j ==3)
    filename = 'mfx_model3.mat';
    elseif (j ==4)
    filename = 'mfx_model4.mat';
    end
    %delete(poolobj);
    save(filename,'p_sub', 'o_sub', 'p_group', 'o_group', 'models', 'ids','-v7.3');
end
delete(poolobj);
