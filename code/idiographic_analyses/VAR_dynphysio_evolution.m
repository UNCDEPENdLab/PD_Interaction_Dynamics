function [fx] = VAR_dynphysio_evolution(Xt,Theta,ut,inF)
% Evolution function for couples physiological data
%
% For couple data, the hidden states are the values of the IBI time series
% These evolve according to self-regulatory and co-regulatory influences
% In the Ferrer & Helm (2013) notation, a parameters represent signal evolution of partner a
%    and b parameters define evolution for partner b.
%
% Xt: hidden states (IBI series valsue)
% Theta: evolution parameters
%     - a1: self-regulatory influence for partner a: a1(p1* - p1_t)
%     - a2: co-regulatory influence for partner a: a2(p2_t - p1_t)
%     - b1: self-regulatory influence for partner b: b1(p2* - p2_t)
%     - b2: co-regulatory influence for partner b: b2(p1_t - p2_t)
% ut: inputs
% 
% inF: options structure for evolution
%     - p1star: "ideal"/equilibrium point for p1 (set to average value of IBI series during baseline)
%     - p2star: "ideal"/equilibrium point for p2 (average value of IBI series during baseline)

%eventually theta could include parametric modulators of regulation
%such as interpersonal behaviors during the session

%this is the integration time step (Euler method)
%unclear whether this is needed in our application. In demo, deltat=1.0
%deltat = inF.deltat;
p1star=0;
p2star=0;
deltat = inF.deltat;
a1 = Theta(1);
a2 = Theta(2);
b1 = Theta(3);
b2 = Theta(4);

p1next = Xt(1) + deltat*(a1*(p1star- Xt(1)) + a2*(Xt(2)));
p2next = Xt(2) + deltat*(b1*(p2star - Xt(2)) + b2*(Xt(1)));

fx = [p1next; p2next];

end