addpath(genpath('/Users/ams939/Documents/MATLAB/VBA-toolbox-master'))

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
n_t=160;
theta = [1, 0, 1,1];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .05;
options.decim = 1;
priors.muX0 = [-1; 2];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)

save('/Users/ams939/Desktop/feb2019_xsim_dependent.mat', 'xsim')


% neg dependent (baseline above where they start)
n_t=40;
theta = [1, 0, 1,.5];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .2;
options.decim = 1;
priors.muX0 = [-4; -2];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
%independent


n_t=160;
theta = [1, 0, 1,0];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .05;
options.decim = 1;
priors.muX0 = [-1; 2];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
save('/Users/ams939/Desktop/feb2019_xsim_independent.mat', 'xsim')

%neg independent

n_t=40;
theta = [1, 0, 1,0];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .2;
options.decim = 1;
priors.muX0 = [-1; -2];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
%contrarian


n_t=160;
theta = [1, 0, 1,-2];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .05;
options.decim = 1;
priors.muX0 = [-2; 1];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)

save('/Users/ams939/Desktop/feb2019_xsim_contrarian.mat', 'xsim')



%neg contrarian (baseline above where they start)



n_t=40;
theta = [1, 0, 1,0];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat = .2;
options.decim = 1;
priors.muX0 = [2; -2];
u = zeros(1, n_t);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
