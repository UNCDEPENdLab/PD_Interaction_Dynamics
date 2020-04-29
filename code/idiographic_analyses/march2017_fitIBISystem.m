function [posterior, out] = march2017_fitIBISystem(file, showfig, model)
%I believe we should be able to use the g_Id observation function
%This is just an identity mapping that is matrix-compatible.
%It allows for a scaling parameter: inG.scale that multiplies the state
%vector by some scalar value. Not sure how useful this would be.

%file='/storage/home/mnh5174/Tresors/Couples_Interaction/example_dyad_ibis.txt';
%file='/Users/michael/Tresors/Couples_Interaction/example_dyad_ibis.txt';
%file='/Users/ams939/Box Sync/DEPENd/Couples Data/data/example_dyad_ibis.txt';

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
inF.p1star = 0;
inF.p2star = 0;
y = data';
delta_t = 0.1; %10Hz series

inF.deltat=delta_t;
inF.models = model;
options.inF = inF;
n_t = size(data, 1);
%add in if statement so that dim.n_theta reflects the actual number of
%parameters being estimated
if strcmpi(inF.models, 'A') %traditional coreg with p1star and p2star estimated (so baseline can be other than 0)
   dim.n_theta = 6;
elseif strcmpi(inF.models, 'B') %traditional coreg 
    dim.n_theta = 4;
elseif strcmpi(inF.models, 'C') %new coreg with p1star and p2star estimated (so baseline can be other than 0)
    dim.n_theta = 6;
elseif strcmpi(inF.models, 'D') % new coregulation with p1star and p2star not estimated
    dim.n_theta = 4;
elseif strcmpi(inF.models, 'E') %sanity check only estimating p1 and p1 star
    dim.n_theta = 5;
elseif strcmpi(inF.models, 'F')
    dim.n_theta = 5;
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
f_fname = @march2017_dynphysio_evolution;
g_fname = @g_Id;
u = zeros(1, n_t);

%set options to have y out for participants whose data is untrustworthy 


[posterior,out] = VBA_NLStateSpaceModel(y,u,f_fname,g_fname,dim,options);
hfp = findobj('tag','VBNLSS');
set(hfp,'tag','0','name','inversion with delays');


