%simulating data based
a1 = [-.1, -.05,  0,  .05, .1];
a2 = [-.1,  -.05,  0, .05,  .1];
a3 = [-.1,  -.05,  0,  .05,  .1];
a4 = [-.1,  -.05,  0,  .05,  .1];
a5 = combvec(a1, a2, a3, a4);
save('simulation_params.mat', 'a5');
%simulate data based off of a5
n_t = 3000;
f_fname = @dynphysio_evolution;
g_fname = @g_Id;
phi=[];
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat=.1;
u = zeros(1, n_t);
dim.n_theta = 4; %a1, a2, b1, b2
dim.n_phi = 0; %identity observation function
dim.n = 2; %coupled oscillator
priors.muX0 = zeros(dim.n, 1); %in refitting, assume initial values of zero
priors.SigmaX0 = 1*eye(dim.n); %zero covariance in prior
priors.muTheta = 0*ones(dim.n_theta, 1); %but if oscillatory parameters are initialized at zero, no convergence
priors.SigmaTheta = 1*eye(dim.n_theta); %all coupling parameters are zero centered and max ~|2.0|

%alphas = NaN(1,125);
%balphas = NaN(1,125);
%asigmas = NaN(1,125);
%bsigmas = NaN(1,125);
%for i = 1:125
 %   alphas(1,i) = posteriors{i,4}.a_alpha;
 %  balphas(1,i) = posteriors{i,4}.b_alpha;
 %   asigmas(1,i) = posteriors{i,4}.a_sigma;
 %   bsigmas(1,i) = posteriors{i,4}.b_sigma;
%end
%avg_a_alpha = mean(alphas);
%avg_b_alpha = mean(balphas);
%avg_a_sigma = mean(asigmas);
%avg_b_sigma = mean(bsigmas);
%priors.a_alpha = avg_a_alpha; %no state noise
%priors.b_alpha = avg_b_alpha;
%priors.a_sigma = avg_a_sigma;
%priors.b_sigma = avg_b_sigma;
%alpha = avg_a_alpha/avg_b_alpha;
%sigma = avg_a_sigma/avg_b_sigma;
alpha = 0.0377;
sigma = 0.0048;
for i = 524:625
    theta = a5(:, i)';
    [ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
    num = i + 1000;
    name = strcat('sim_',string(num), '.txt'); 
    fileID = fopen(name, 'w');
    fprintf(fileID, '%6.2f  %12.8f\n', xsim);
end