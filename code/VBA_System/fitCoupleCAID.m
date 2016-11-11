%fitCoupleSystem(file)
%I believe we should be able to use the g_Id observation function
%This is just an identity mapping that is matrix-compatible.
%It allows for a scaling parameter: inG.scale that multiplies the state
%vector by some scalar value. Not sure how useful this would be.

%file='/storage/home/mnh5174/Tresors/Couples_Interaction/example_dyad_ibis.txt';
file='/Users/ams939/Box Sync/DEPENd/Couples Data/data/example_dyad_ibis_caid.txt';

data = dlmread(file);
%l_ibi_interp_detrend r_ibi_interp_detrend l_dom_interp_detrend r_dom_interp_detrend l_aff_interp_detrend r_aff_interp_detrend

data = data(:,3:6); %just caid for now

%reduce to first 120 seconds for initial tests
%data = data(1:1200,:);
y = data';
delta_t = 0.1; %10Hz series     

inF.p1star = 0; %since the IBIs were demeaned and detrended in R, for now, default to 0 equilibrium
inF.p2star = 0;
inF.deltat=delta_t;
options.inF = inF;
n_t = size(data, 1);

dim.n_theta = 8; %self and other coupling separately for dom and aff (not currently supporting dom-aff covariation)
dim.n_phi = 0; %identity observation function
dim.n = size(data, 2); %number of physiological signals

options.backwardLag = 16; %controls the lag of the Kalman filter (lagged impact of hidden states on observations -- 1.6 seconds)

priors.muX0 = 0*ones(dim.n, 1); %initial IBI signal values. If not demeaned, might be avg IBI value
priors.SigmaX0 = 10*eye(dim.n); %zero covariance in prior
priors.muTheta = 0*ones(dim.n_theta, 1);
priors.SigmaTheta = eye(dim.n_theta); %all coupling parameters are zero centered and max ~1.0

%fit deterministic system
%priors.a_alpha = Inf;
%priors.b_alpha = 0;

%stochastic fitting (necessary to get diverging dynamics)
priors.a_alpha = 1e0;
priors.b_alpha = 1e1;

priors.a_sigma = 1e1;
priors.b_sigma = 1e1;
options.priors = priors;
f_fname = @dyncaid_evolution;
g_fname = @g_Id;
u = zeros(1, n_t);

[posterior,out] = VBA_NLStateSpaceModel(y,u,f_fname,g_fname,dim,options);
hfp = findobj('tag','VBNLSS');
set(hfp,'tag','0','name','inversion with backward lag'); 
