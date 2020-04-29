function [posterior, out] = fitCoupleSystem(file)
%I believe we should be able to use the g_Id observation function
%This is just an identity mapping that is matrix-compatible.
%It allows for a scaling parameter: inG.scale that multiplies the state
%vector by some scalar value. Not sure how useful this would be.

%file='/storage/home/mnh5174/Tresors/Couples_Interaction/example_dyad_ibis.txt';
%file='/Users/michael/Tresors/Couples_Interaction/example_dyad_ibis.txt';
%file='/Users/ams939/Box Sync/DEPENd/Couples Data/data/example_dyad_ibis.txt';

data = dlmread(file);

%assuming PTNUM, time, l_ibi_interp_detrend, r_ibi_interp_dtrend, l_dom_interp_detrend, r_dom_interp_detrend, l_aff_interp_detrend,
%r_aff_interp_detrend
data = data(:,[3 4]); %IBIs only

%reduce to first 60 seconds for initial tests
%data = data(1:1200,1);
%y = data(:,1)'; %individual oscillator
%y = data(1:100,:)';
%data = data(1:800,:);
y = data';
delta_t = 0.1; %10Hz series

inF.p1star = 0; %since the IBIs were demeaned and detrended in R, for now, default to 0 equilibrium
inF.p2star = 0;
inF.deltat=delta_t;
options.inF = inF;
n_t = size(data, 1);

dim.n_theta = 4; %a1, a2, b1, b2
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
f_fname = @dynphysio_evolution;
g_fname = @g_Id;
u = zeros(1, n_t);

[posterior,out] = VBA_NLStateSpaceModel(y,u,f_fname,g_fname,dim,options);
hfp = findobj('tag','VBNLSS');
set(hfp,'tag','0','name','inversion with delays');


%save('myfit.mat', 'posterior', 'out', 'y', 'u', 'dim', 'options');




%5-lag embedding (I guess this needs to be fit/compared)
%embedding looks horrible compared to the non-embedded fits above.
%options.delays = 1.*[[1,5];[5,1]];
%[posterior_embed,out_embed] = VBA_NLStateSpaceModel(y,u,f_fname,g_fname,dim,options);

%options.delays = 1.*[[1,2];[2,1]];
%[posterior_embed2,out_embed2] = VBA_NLStateSpaceModel(y,u,f_fname,g_fname,dim,options);

%[posterior,out] = VBA_NLStateSpaceModel(y,u,f_fname,g_fname,dim,options);
%[posterior,out] = VBA_NLStateSpaceModel(y,u,@dynphysio_evolution_onesignal,g_fname,dim,options);

%try fitting the data using the same function as in demo_2Dlin
% priors.muX0 = zeros(2,1);
% priors.SigmaX0 = 100*eye(2);
% priors.muTheta = 0*ones(1,1);
% priors.SigmaTheta = 1e-1*eye(1);
% priors.a_alpha = 1e0;
% priors.b_alpha = 1e0;
% priors.a_sigma = 1e0;
% priors.b_sigma = 1e0;
% 
% options.priors      = priors;
% options.inF.deltat = delta_t;
% options.inF.b = 5e-1;
% options.backwardLag  = 16;
% dim.n_theta         = 1;
% dim.n_phi           = 0;
% dim.n               = 2;
% 
% f_fname = @f_lin2D;
% g_fname = @g_Id;
% 
% dim.n_theta = 1; %a
% dim.n_phi = 0; %identity observation function
% dim.n = size(data, 2); %number of physiological signals
% 
% [posterior,out] = VBA_NLStateSpaceModel(y,u,f_fname,g_fname,dim,options);
% 
% 
% hfp = findobj('tag','VBNLSS');
% set(hfp,'tag','0','name','inversion with delays');


%simulation exdample
% function dz=dif_eq(t,y,ti,fi)
% mu=0.7;
% r=1;
% K=1;
% w=1;
% F=interp1(ti,fi,t);
% dz(1)= (mu - r^2)*y(1) - w*y(2) +K*F;
% dz(2) = (mu - r^2)*y(2) + w*y(1);
% end
% 
% ti=[0.1:0.1:10]; % time vector
% fi=rand(1,numel(t)); % your perturbation
% t=ti;
% [t,y]=ode45(@(t,y) dif_eq(t,y,ti,fi),t,[0;0]);
