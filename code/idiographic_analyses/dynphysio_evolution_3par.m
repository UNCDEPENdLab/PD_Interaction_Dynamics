 function [fx] = dynphysio_evolution_3par(Xt,Theta,u,inF)
% Evolution function for couples physiological data
%
% For couple data, the hidden states are the values of the IBI time series
% These evolve according to self-regulatory and co-regulatory influences
% In the Ferrer & Helm (2013) notation, a parameters represent signal evolution of partner a
%    and b parameters define evolution for partner b.
%
% Xt: hidden states (IBI series value and CAID values)
% Theta: evolution parameters
%     - a1: self-regulatory influence for partner a: a1(p1* - p1_t)
%     - a2: co-regulatory influence for partner a: a2(p2_t - p1_t)
%     - a3: self-regulatory influence for partner a DOM: a3(d1* - d1_t)
%     - a4: co-regulatory influence for partner a DOM: a4(d2_t - d1_t)
%     - a5: self-regulatory influence for partner a AFF: a5(af1* - af1_t)
%     - a6: co-regulatory inflluence for partner a AFF: a6(af2_t - af1_t)
%     - b1: self-regulatory influence for partner b: b1(p2* - p2_t)
%     - b2: co-regulatory influence for partner b: b2(p1_t - p2_t)
%     - b3: self-regulatory influence for partner b DOM: a3(d2* - d2_t)
%     - b4: co-regulatory influence for partner b DOM: a4(d1_t - d2_t)
%     - b5: self-regulatory influence for partner b AFF: a5(af2* - af2_t)
%     - b6: co-regulatory inflluence for partner b AFF: a6(af1_t - af2_t)
% ut: inputs (eventually spaff)
% 
% inF: options structure for evolution
%     - p1star: "ideal"/equilibrium point for p1 (set to average value of IBI series during baseline)
%     - d1star: "ideal"/equilibrium point for d1 (average value of DOM)
%     - af1star: "ideal" equilibrium point for af1 (set to average of AFF)
%     - p2star: "ideal"/equilibrium point for p2 (set to avg IBI)
%     - d2star: "ideal" point for d2 (DOM)
%     - af2star: "ideal" point for af2 (AFF)
%
%Models are set up such that iteratively include all IBI and CAID
%parameters. Note that in A and C act as sanity checks insofar that simpreg
%means that selfreg and coreg are set to be the same for both patient and
%partner (respectively), which doesn't make a whole lot of sense. Nevertheless need to test simpler models to ensure that Steele and Ferrer models are in fact the best.
%Although this can be tested in dom and
%aff reg, did not do this because doesn't make a whole lot of sense. In
%future, can test with affself since interpersonal theory predicts that
%they'll typically match. d
% Shorthand for models
%       A: simpself
%       B: selfreg
%       C: simpcoreg
%       D: coreg
%       E: domself
%       F: domcoreg
%       G: affself
%       H: affreg
%       I: affregselfdom
%       J: domregselfaff
%       K: coregdomaff
%    
%eventually theta could include parametric modulators of regulation
%such as interpersonal behaviors during the session, SPAFF

%this is the integration time step (Euler method)
%unclear whether this is needed in our application. In demo, deltat=1.0
%deltat = inF.deltat;
deltat = inF.deltat;


%initialize dummy variables
p1next = 0;
p2next = 0;
fx = [p1next; p2next];

%p1next = Xt(3);
%p2next = Xt(4);
if strcmpi(inF.models, 'A') %dom self + other
%if ~isempty(regexp(inF.models, 'A', 'once'))
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1star = Theta(5);
    p2star = Theta(6);
    c1 = Theta(7);
    c2 = Theta(8);
    d1 = Theta(9);
    d2 = Theta(10);
    p1next = Xt(1) + deltat*(a1*(p1star-Xt(1)) + (-a1+a2)*(Xt(2)-Xt(1)) + c1*u(1) + c2*u(2));
    p2next = Xt(2) + deltat*(b1*(p2star-Xt(2)) + (-b1+b2)*(Xt(1)-Xt(2))+ d1*u(1) + d2*u(2));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'B') %selfreg dom 
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1star = Theta(5);
    p2star = Theta(6);
    c1 = Theta(7);
    d1 = Theta(8);
    p1next = Xt(1) + deltat*(a1*(p1star-Xt(1)) + (-a1+a2)*(Xt(2)-Xt(1)) + c1*u(1));
    p2next = Xt(2) + deltat*(b1*(p2star-Xt(2)) + (-b1+b2)*(Xt(1)-Xt(2))+ d1*u(1));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'C') %aff self + other
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1star = Theta(5);
    p2star = Theta(6);
    e1 = Theta(7);
    e2 = Theta(8);
    f1 = Theta(9);
    f2 = Theta(10);
    p1next = Xt(1) + deltat*(a1*(p1star-Xt(1)) + (-a1+a2)*(Xt(2)-Xt(1)) + e1*u(3) + e2*u(4));
    p2next = Xt(2) + deltat*(b1*(p2star-Xt(2)) + (-b1+b2)*(Xt(1)-Xt(2))+ f1*u(4) + f2*u(3));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'D') %aff self
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1star = Theta(5);
    p2star = Theta(6);
    e1 = Theta(7);
    f1 = Theta(8);
    p1next = Xt(1) + deltat*(a1*(p1star-Xt(1)) + (-a1+a2)*(Xt(2)-Xt(1)) + e1*u(3));
    p2next = Xt(2) + deltat*(b1*(p2star-Xt(2)) + (-b1+b2)*(Xt(1)-Xt(2))+ f1*u(4));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'E') %dom self + other, aff self
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1star = Theta(5);
    p2star = Theta(6);
    c1 = Theta(7);
    c2 = Theta(8);
    d1 = Theta(9);
    d2 = Theta(10);
    e1 = Theta(11);
    f1 = Theta(12);
    p1next = Xt(1) + deltat*(a1*(p1star-Xt(1)) + (-a1+a2)*(Xt(2)-Xt(1)) + c1*u(1) + c2*u(2) + e1*u(3));
    p2next = Xt(2) + deltat*(b1*(p2star-Xt(2)) + (-b1+b2)*(Xt(1)-Xt(2))+ d1*u(2) + d2*u(1) + f1*u(4));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'F')%coreg aff, dom self
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1star = Theta(5);
    p2star = Theta(6);
    c1 = Theta(7);
    d1 = Theta(8);
    e1 = Theta(9);
    e2 = Theta(10);
    f1 = Theta(11);
    f2 = Theta(12);
    p1next = Xt(1) + deltat*(a1*(p1star-Xt(1)) + (-a1+a2)*(Xt(2)-Xt(1)) +c1*u(1)+ e1*u(3) + e2*u(4));
    p2next = Xt(2) + deltat*(b1*(p2star-Xt(2)) + (-b1+b2)*(Xt(1)-Xt(2))+ d1*u(2) + f1*u(4) + f2*u(3));
    fx = [p1next; p2next];
elseif strcmpi (inF.models, 'G') %aff + self self
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1star = Theta(5);
    p2star = Theta(6);
    c1 = Theta(7);
    d1 = Theta(8);
    e1 = Theta(9);
    f1 = Theta(10);
    p1next = Xt(1) + deltat*(a1*(p1star-Xt(1)) + (-a1+a2)*(Xt(2)-Xt(1)) +c1*u(1)+ e1*u(3));
    p2next = Xt(2) + deltat*(b1*(p2star-Xt(2)) + (-b1+b2)*(Xt(1)-Xt(2))+ d1*u(2) + f1*u(4));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'H') %coreg dom + aff
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1star = Theta(5);
    p2star = Theta(6);
    c1 = Theta(7);
    c2 = Theta(8);
    d1 = Theta(9);
    d2 = Theta(10);
    e1 = Theta(11);
    e2 = Theta(12);
    f1 = Theta(13);
    f2 = Theta(14);
    p1next = Xt(1) + deltat*(a1*(p1star-Xt(1)) + (-a1+a2)*(Xt(2)-Xt(1)) +c1*u(1)+c2*u(2)+ e1*u(3) + e2*u(4));
    p2next = Xt(2) + deltat*(b1*(p2star-Xt(2)) + (-b1+b2)*(Xt(1)-Xt(2))+ d1*u(2) + d2*u(1) + f1*u(4) + f2*u(3));
    fx = [p1next; p2next];
end