function [fx] = dyncaid_evolution(Xt,Theta,ut,inF)
% Evolution function for couples interpersonal data
%
% Similar notion to the Steele differential equations, but here the system is tracking dominance and affiliation
% Can have self-coupling and other coupling.
%
% Xt: hidden states (CAID time series)
%   (1): p1 dominance
%   (2): p2 dominance
%   (3): p1 affiliation
%   (4): p2 affiliation
%
% Theta: evolution parameters
%   (1) d1self: self-regulation/autocorrelation of p1 dominance
%   (2) d2self: self-regulation/autocorrelation of p2 dominance
%   (3) d1d2: p1 dominance -> p2 dominance effect
%   (4) d2d1: p2 dominance -> p1 dominance effect
%   (5) a1self: self-regulation/autocorrelation of p1 affiliation
%   (6) a2self: self-regulation/autocorrelation of p2 affiliation
%   (7) a1a2: p1 affiliation -> p2 affiliation effect
%   (8) a2a1: p2 affiliation -> p1 affiliation effect
%
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
%p1star=inF.p1star;
%p2star=inF.p2star;
deltat = inF.deltat;
d1self = Theta(1);
d2self = Theta(2);
d1d2 = Theta(3);
d2d1 = Theta(4);
a1self = Theta(5);
a2self = Theta(6);
a1a2 = Theta(7);
a2a1 = Theta(8);

%for now, we are only modeling evolution within each dimension (not dominance-affiliation effects)
p1dom_next = Xt(1) + deltat*( d1self*Xt(1) + d2d1*Xt(2) );
p2dom_next = Xt(2) + deltat*( d2self*Xt(2) + d1d2*Xt(1) );
p1aff_next = Xt(3) + deltat*( a1self*Xt(3) + a2a1*Xt(4) );
p2aff_next = Xt(4) + deltat*( a2self*Xt(4) + a1a2*Xt(3) );

fx = [p1dom_next; p2dom_next; p1aff_next; p2aff_next];

end