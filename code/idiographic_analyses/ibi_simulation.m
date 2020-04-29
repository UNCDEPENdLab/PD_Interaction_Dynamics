%simulate with good VBA people (n = 4)

% load 'allcouples_ibi_fits copy.mat'


%visualize results
for i = 1:length(celldummy(:,1))
    VBA_ReDisplay(celldummy{i,1}, celldummy{i,2})
    pause
end


% for i = 1:10
%     n_t=6000;
% f_fname = @dynphysio_evolution;
% g_fname = @g_Id;
% phi=[];
% theta = celldummy{i,1}.muTheta;
% options.inF.p1star = 0;
% options.inF.p2star = 0;
% options.inF.deltat=.1;
% %options.decim=1;
% priors.muX0 = celldummy{i,1}.muX0;
% u = zeros(1, n_t);
% % alpha = celldummy{1,1}.b_alpha;
% % sigma = celldummy{1,1}.b_sigma;
% alpha = celldummy{i,1}.a_alpha/celldummy{i,1}.b_alpha;
% sigma = celldummy{i,1}.a_sigma/celldummy{i,1}.b_sigma;
% % alpha = avgalpha;
% % sigma = avgsigma;
% [ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);
% displaySimulations(ysim,xsim,eta,e)
% end




var = xcorr(xsim(1,:)', xsim(2,:)', 'coeff', 3000);
vec = zeros(1, 6001);
for i=1:length(vec)
    vec(1,i) = i-3001;
end
plot(vec, var)


n_t=6000;
theta5=zeros(4,1);
for i = 1:4
    theta5(i) = 5*celldummy{1,1}.muTheta(i);
end

thetaDiff = zeros(1,4);
thetaDiff(1) = .0068;
thetaDiff(2) = 0*.0245;
thetaDiff(3) = .0781;
thetaDiff(4) = 0*.0356;
 
%theta = [.002;.4;.2;-.4];
%theta = thetaDiff;

theta = [.5; -.30; .5; -.15];
f_fname = @dynphysio_evolution;
g_fname = @g_Id;
phi=[];

%plot(vec, var)
%getfield(theta, 'muTheta')
options.inF.p1star = 0;
options.inF.p2star = 0;
options.inF.deltat=.1;
%options.decim=1;
% priors.muX0 = muX0AvgT;
%priors.muX0 = celldummy{1,1}.muX0;
%priors.muX0 = celldummy{1,1}.muX0;
priors.muX0 = [.025; -.025];
u = zeros(1, n_t);
% alpha = celldummy{1,1}.b_alpha;
% sigma = celldummy{1,1}.b_sigma;
alpha = 100000000;
%alpha = 1000*celldummy{1,1}.a_alpha/celldummy{1,1}.b_alpha;
% sigma = 1000*celldummy{1,1}.a_sigma/celldummy{1,1}.b_sigma;
sigma = Inf;
% alpha = avgalpha;
% sigma = avgsigma;
rng(1000);
[ysim,xsim,x0sim,eta,e] = simulateNLSS(n_t,f_fname,g_fname,theta,phi,u,alpha,sigma,options, priors.muX0);


displaySimulations(ysim,xsim,eta,e)
var = xcorr(xsim(1,:)', xsim(2,:)', 'coeff', 3000);
vec = zeros(1, 6001);
for i=1:length(vec)
    vec(1,i) = i-3001;
end
plot(vec, var)
[~,i]=max(var)