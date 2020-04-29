load('/Users/ams939/Desktop/pdid_params_98.mat')
scpt = x(:,1);
ccpt = x(:,2);
scpr = x(:,3);
ccpr = x(:,4);

addpath(genpath('/Users/ams939/Downloads/VBA-toolbox-master-2'))
pt = zeros(98, 801);
pr = zeros(98, 801);
for i = 1:98
       n_t=800;
   scpt_tmp = scpt(i,1);
   ccpt_tmp = ccpt(i,1);
   scpr_tmp = scpr(i,1);
   ccpr_tmp = ccpr(i,1);
   
   dim.n_theta = 4; %a1, a2, b1, b2
   dim.n_phi = 0; %identity observation function
   dim.n = 2; %coupled oscillator
   
   
   theta = [scpt_tmp; ccpt_tmp; scpr_tmp; ccpr_tmp];
   priors.SigmaX0 = 0*eye(dim.n); %zero covariance in prior
   priors.muTheta = [.0305; -.1221; 2.8154; .0903]; %but if oscillatory parameters are initialized at zero, no convergence
   priors.SigmaTheta = [.000156 0.000000955      0      0 ; 0.000000955 0.000115      0      0; 0      0 0.000115 0.000000955; 0      0 0.000000955 0.000156]; %all coupling parameters are zero centered and max ~|2.0|
   priors.muX0 = [180; 0]; %in refitting, assume initial values of zero
   %priors.SigmaX0 = 1*eye(dim.n); %zero covariance in prior
   options.priors = priors;
   options.inF.p1star = 0;
   options.inF.p2star = 0;
   options.inF.deltat=.1;
   
   u = zeros(1, n_t);

   
   f_fname = @VAR_dynphysio_evolution;
   g_fname = @g_Id;
   
   
   alpha=2858160;
   sigma=253.0548;
   
   phi=[];
   
  
   [ysim,xsim,x0sim,eta,e] = VBA_simulate(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
    pt(i, 1:800) = ysim(1,:);
    pr(i, 1:800) = ysim(2,:);
    pt(i, 801) = x(i,5);
    pr(i, 801) = x(i, 5);
end

save('/Users/ams939/Desktop/sep2019_pt_crazy.mat', 'pt', 'pr');

pt = zeros(98, 801);
pr = zeros(98, 801);
for i = 1:98
       n_t=800;
   scpt_tmp = scpt(i,1);
   ccpt_tmp = ccpt(i,1);
   scpr_tmp = scpr(i,1);
   ccpr_tmp = ccpr(i,1);
   
   dim.n_theta = 4; %a1, a2, b1, b2
   dim.n_phi = 0; %identity observation function
   dim.n = 2; %coupled oscillator
   
   
   theta = [scpt_tmp; ccpt_tmp; scpr_tmp; ccpr_tmp];
   priors.SigmaX0 = 0*eye(dim.n); %zero covariance in prior
   priors.muTheta = [.0305; -.1221; 2.8154; .0903]; %but if oscillatory parameters are initialized at zero, no convergence
   priors.SigmaTheta = [0.000283 -0.0000255 0 0; -0.0000255 0.000525 0 0; 0 0 0.000525 -0.0000255; 0 0 -0.0000255 0.000283]; %all coupling parameters are zero centered and max ~|2.0|
   priors.muX0 = [0; 180]; %in refitting, assume initial values of zero
   %priors.SigmaX0 = 1*eye(dim.n); %zero covariance in prior
   options.priors = priors;
   
   options.inF.p1star = 0;
   options.inF.p2star = 0;
   options.inF.deltat=.1;
   
   u = zeros(1, n_t);

   
   f_fname = @VAR_dynphysio_evolution;
   g_fname = @g_Id;
   
   
   alpha=2858160;
   sigma=253.0548;
   
   phi=[];
   
  
   [ysim,xsim,x0sim,eta,e] = VBA_simulate(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
    pt(i, 1:800) = ysim(1,:);
    pr(i, 1:800) = ysim(2,:);
    pt(i, 801) = x(i,5);
    pr(i, 801) = x(i, 5);
end

save('/Users/ams939/Desktop/sep2019_pr_crazy.mat', 'pt', 'pr');