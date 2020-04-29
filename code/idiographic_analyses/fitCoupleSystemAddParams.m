function [posterior, out] = fitCoupleSystemAddParams(file, showfig, model, id)
%I believe we should be able to use the g_Id observation function
%This is just an identity mapping that is matrix-compatible.
%It allows for a scaling parameter: inG.scale that multiplies the state
%vector by some scalar value. Not sure how useful this would be.

%file='/storage/home/mnh5174/Tresors/Couples_Interaction/example_dyad_ibis.txt';
%file='/Users/michael/Tresors/Couples_Interaction/example_dyad_ibis.txt';
%f

if showfig == 1
    options.DisplayWin = 1;
else
    options.DisplayWin = 0;
end


data = dlmread(file);

%assuming PTNUM, time, l_ibi_interp_detrend, r_ibi_interp_dtrend, l_dom_interp_detrend, r_dom_interp_detrend, l_aff_interp_detrend,
%r_aff_interp_detrend
data = data(:,[3 4]); %all possibilities

%reduce to first 60 seconds for initial tests
%data = data(1:1200,1);
%y = data(:,1)'; %individual oscillator
%y = data(1:100,:)';
%data = data(1:800,:);
y = data';
options.isYout = zeros(size(y));
if (id == 8030)
    options.isYout(1,[1:20,970:990, 1000:1020, 1040:1060, 2805:2820, 2875:2890, 2900:2920, 2935:2950, 3000:3020, 4390:4410, 4655:4670, 4710:4730, 5240:5260, 5315:5330, 5345:5360, 5440:5460, 5740:5765, 6020:6040, 6050:6068]) = 1;
elseif (id == 8032)
    oqptions.isYout(1,[45:60, 320:370, 855:875, 895:915, 955:970, 1080:1100, 1215:1230, 1250:1265, 1285:1305, 1335:1350, 1350:1375, 2205:2220, 2230:2250, 2555:2300, 2330:2345, 2350:2370, 2620:2640, 2690:2710, 2715:2750, 3825:3840, 4265:4280, 4320:4340, 4370:4390, 4420:4460, 4605:4645, 5070:5085, 5115:5135, 5220:5290, 5405:5530, 5745:5760, 5785:5825, 5860:5875, 5935:5975]) = 1;
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
end

delta_t = 0.1; %10Hz series

u_data = dlmread(file);
u_data = u_data(:, 5:8);
u_data = u_data';
%u = u_data;
inF.u = u_data;
inF.deltat=delta_t;
inF.models = model;
options.MaxIter = 100;
options.TolFun = 1e-3;
options.GnMaxIter = 100;
options.GnTolFun = 1e-3;


options.inF = inF;
%n_t = size(data, 1);
if strcmpi(inF.models, 'A') %coreg dom
    dim.n_theta = 10;
elseif strcmpi(inF.models, 'B') %selfreg dom 
    dim.n_theta = 8;
elseif strcmpi(inF.models, 'C') %coreg aff
    dim.n_theta = 10;
elseif strcmpi(inF.models, 'D') %selfreg aff
    dim.n_theta = 8;
elseif strcmpi(inF.models, 'E') %dom self + other, aff self
    dim.n_theta = 12;
elseif strcmpi(inF.models, 'F')%aff self + other, dom self
    dim.n_theta = 12;
elseif strcmpi (inF.models, 'G') %aff self, dom self
    dim.n_theta = 10;
elseif strcmpi(inF.models, 'H') %coreg aff + dom
    dim.n_theta = 14;
end 


dim.n_phi = 0; %identity observation function
dim.n = size(data, 2); %number of physiological signals

options.backwardLag = 16; %number of observations to consider in making prediction of next sample (lagged Kalman filter)

%priors.muX0 = 0*ones(dim.n, 1); %initial IBI signal values. If not demeaned, might be avg IBI value
priors.muX0 = [y(1,1); y(2,1)]; %use observed initial values of IBI series
priors.SigmaX0 = 100*eye(dim.n); %zero covariance in prior. sd of 10 in initial states (since it should be close to observed)
priors.muTheta = 0*ones(dim.n_theta, 1);
priors.SigmaTheta = eye(dim.n_theta); %all coupling parameters are zero centered and max ~1.0

%fit deterministic system
%priors.a_alpha = Inf;
%priors.b_alpha = 0;
priors.a_alpha = 1e0;
priors.b_alpha = 1e1;
priors.a_sigma = 1e1;
priors.b_sigma = 1e1;
options.priors = priors;
f_fname = @dynphysio_evolution_3par;
g_fname = @g_Id;
%u = zeros(1, n_t);
u = u_data;
[posterior,out] = VBA_NLStateSpaceModel(y,u,f_fname,g_fname,dim,options);
hfp = findobj('tag','VBNLSS');
set(hfp,'tag','0','name','inversion with delays');
