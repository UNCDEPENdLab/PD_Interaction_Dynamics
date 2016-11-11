%simulate with good VBA people (n = 4)

load 'allcouples_ibi_fits copy.mat'


%visualize results
% for i = 1:10
%     VBA_ReDisplay(celldummy{i,1}, celldummy{i,2})
%     pause
% end


for i = 1:10
    n_t=6000;
f_fname = @dynphysio_evolution;
g_fname = @g_Id;
phi=[];
theta = celldummy{i,1}.muTheta;
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat=.1;
%options.decim=1;
priors.muX0 = celldummy{i,1}.muX0;
u = zeros(1, n_t);
% alpha = celldummy{1,1}.b_alpha;
% sigma = celldummy{1,1}.b_sigma;
alpha = celldummy{i,1}.a_alpha/celldummy{i,1}.b_alpha;
sigma = celldummy{i,1}.a_sigma/celldummy{i,1}.b_sigma;
% alpha = avgalpha;
% sigma = avgsigma;
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)
end



n_t=6000;
theta5=zeros(4,1);
for i = 1:4
    theta5(i) = 5*celldummy{1,1}.muTheta(i);
end

thetaDiff = zeros(1,4);
thetaDiff(1) = .0068*5;
thetaDiff(2) = .0245;
thetaDiff(3) = .0781*5;
thetaDiff(4) = -.0356

theta = thetaDiff
f_fname = @dynphysio_evolution;
g_fname = @g_Id;
phi=[];

var = xcorr(xsim(1,:)', xsim(2,:)', 'coeff', 6000);
vec = zeros(1, 12001);
for i=1:length(vec)
    vec(1,i) = i-6001;
end


plot(vec, var)
%getfield(theta, 'muTheta')
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat=.1;
%options.decim=1;
% priors.muX0 = muX0AvgT;
priors.muX0 = celldummy{1,1}.muX0;
u = zeros(1, n_t);
% alpha = celldummy{1,1}.b_alpha;
% sigma = celldummy{1,1}.b_sigma;
alpha = celldummy{1,1}.a_alpha/celldummy{1,1}.b_alpha;
sigma = celldummy{1,1}.a_sigma/celldummy{1,1}.b_sigma;
% alpha = avgalpha;
% sigma = avgsigma;
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
displaySimulations(ysim,xsim,eta,e)