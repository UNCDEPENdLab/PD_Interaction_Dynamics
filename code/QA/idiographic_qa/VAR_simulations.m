%steele and ferrer 2013 simulations
%verifying that I can capture the oscillatory dynamics
%simulate from Ferrer and Steele differential equation model
%no state or measurement noise
alpha=Inf;
sigma=Inf;
phi=[];
f_fname = @VAR_dynphysio_evolution;
g_fname = @g_Id;

%for refitting simulated data
dim.n_theta = 4; %a1, a2, b1, b2
dim.n_phi = 0; %identity observation function
dim.n = 2; %coupled oscillator
priors.muX0 = zeros(dim.n, 1); %in refitting, assume initial values of zero
priors.SigmaX0 = 1*eye(dim.n); %zero covariance in prior
priors.muTheta = 0*ones(dim.n_theta, 1); %but if oscillatory parameters are initialized at zero, no convergence
priors.SigmaTheta = 1*eye(dim.n_theta); %all coupling parameters are zero centered and max ~|2.0|
%priors.a_alpha = Inf; %no state noise
%priors.b_alpha = 0;
priors.a_alpha = 1e0;
priors.b_alpha = 1e1;
priors.a_sigma = 1e0;
priors.b_sigma = 1e0;

%figure 1 from Steele
n_t=25;
theta = [1, 0, 1,1];
options.inF.p1star = .5;
options.inF.p2star = .5;
options.inF.deltat = .2;
options.decim = 1;
priors.muX0 = [1; 2];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
fig1ysim = ysim
fig1xsim = xsim
n_t=120;
theta = [2, 1, 2, 1];
options.inF.p1star =3;
options.inF.p2star = 5;
options.inF.deltat = .2;
options.decim = 1;
priors.muX0 = [4; 5];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)

% 
% %figure 2 from steele
%clear variables
n_t=120;
theta = [1, 0, 1, 0];
options.inF.p1star = 3;
options.inF.p2star = 5;
options.inF.deltat = .167;
options.decim = 1;
priors.muX0 = [7; 3];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
fig2ysim = ysim
fig2xsim = xsim
% %figure 4 from steele
n_t=105;
f_fname = @VAR_dynphysio_evolution;
g_fname = @g_Id;

theta = [0, -1, 1, 1];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .167;
options.decim = 1;
priors.muX0 = [15; 5];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
fig4ysim = ysim
fig4xsim = xsim
% Figure 5 from Steele & Ferrer (slightly modified because the parameters in figure don't match the graph
n_t=105;
theta=[0, 1, 0, -2];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat=.167;
options.decim=1;
priors.muX0 = [12; 15];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
fig5ysim = ysim
fgi5xsim = xsim

options.priors = priors;
options.backwardLag = 12; %unsure of whether this is at all necessary
%for t=1:n_t
%    options.priors.iQx{t} = 1*eye(2);
%end
%options.delays = 1.*[[1,2];[2,1]]; %likewise, delay embedding doesn't s
%refit simulated data
dim.n_theta = 4; %a1, a2, b1, b2
dim.n_phi = 0; %identity observation function
dim.n = 2; %coupled oscillator
priors.muX0 = zeros(dim.n, 1); %in refitting, assume initial values of zero
priors.SigmaX0 = 1*eye(dim.n); %zero covariance in prior
%priors.muTheta = [0; 0; 0]; %it converges with these population settings
priors.muTheta = [0; 0; 0; 0]; %it converges with these population settings
priors.SigmaTheta = 1*eye(dim.n_theta); %all coupling parameters are zero centered and max ~|2.0|
options.priors=priors;
[posterior,out] = VBA_NLStateSpaceModel(ysim,u,f_fname,g_fname,dim,options);


% TEST OTHER VARIANTS
n_t=30;
f_fname = @VAR_dynphysio_evolution;
g_fname = @g_Id;

theta = [0, -1, 0, -1];
options.inF.p1star = 2;
options.inF.p2star = .5;
options.inF.deltat = .167;
options.decim = 1;
priors.muX0 = [9; 2];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
% 

fig3xsim = xsim
fig3ysim = ysim


save('var_figsims.mat', 'fig1ysim', 'fig1xsim', 'fig2ysim', 'fig2xsim', 'fig3ysim', 'fig3xsim', 'fig4ysim', 'fig4xsim', 'fig5ysim', 'fig5xsim')


n_t=500;
f_fname = @VAR_dynphysio_evolution;
g_fname = @g_Id;

theta = [.8, 0, .8, 1.2];
options.inF.p1star = 2;
options.inF.p2star = .5;
options.inF.deltat = .167;
options.decim = 1;
priors.muX0 = [9; -9];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)


%Creating fig1b
alpha=Inf;
sigma=Inf;
phi=[];
f_fname = @VAR_dynphysio_evolution;
g_fname = @g_Id;

dim.n_theta = 4; %a1, a2, b1, b2
dim.n_phi = 0; %identity observation function
dim.n = 2; %coupled oscillator
priors.muX0 = zeros(dim.n, 1); %in refitting, assume initial values of zero

%figure 1 from Steele

%dependent
n_t=40;
theta = [1, 0, 1,1];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .2;
options.decim = 1;
priors.muX0 = [-1; 2];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)

%independent


n_t=40;
theta = [1, 0, 1,0];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .2;
options.decim = 1;
priors.muX0 = [-1; 2];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)


%contrarian


n_t=40;
theta = [1, 0, 1,-2];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .2;
options.decim = 1;
priors.muX0 = [-2; 1];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)



























