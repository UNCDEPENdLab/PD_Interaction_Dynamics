 function [fx] = ibi_dynphysio_evolution(Xt,Theta,ut,inF)
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
%     - a3: self-regulatory influence for partner a VAR: a3(d1_t)
%     - a4: co-regulatory influence for partner a VAR: a4(d2_t)
%     - b1: self-regulatory influence for partner b: b1(p2* - p2_t)
%     - b2: co-regulatory influence for partner b: b2(p1_t - p2_t)
%     - b3: self-regulatory influence for partner b VAR: a3(d2_t)
%     - b4: co-regulatory influence for partner b VAR: a4(d1_t)
% ut: inputs (eventually spaff)
% 
% inF: options structure for evolution
%     - p1star: "ideal"/equilibrium point for p1 (set to average value of IBI series during baseline)
%     - p2star: "ideal"/equilibrium point for p2 (set to avg IBI)
%
%Models are set up such that iteratively include all IBI
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
%       E: varselfsimp
%       F: varself
%       G: varcoregsimp
%       H: varcoreg
%    
%eventually theta could include parametric modulators of regulation
%such as interpersonal behaviors during the session, SPAFF

%this is the integration time step (Euler method)
%unclear whether this is needed in our application. In demo, deltat=1.0
%deltat = inF.deltat;
p1star=inF.p1star;
p2star=inF.p2star;
deltat = inF.deltat;

%initialize dummy variables

p1next = 0;
p2next = 0;
fx = [p1next; p2next];

%a1 = a2
if strcmpi(inF.models, 'A')
%if ~isempty(regexp(inF.models, 'A', 'once'))
    a1 = Theta(1);
    p1next = Xt(1) + deltat*(a1*(p1star - Xt(1)));
    p2next = Xt(2) + deltat*(a1*(p2star - Xt(2)));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'B') %selfreg
    a1 = Theta(1);
    b1 = Theta(2);
    p1next = Xt(1) + deltat*(a1*(p1star - Xt(1)));
    p2next = Xt(2) + deltat*(b1*(p2star - Xt(2)));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'C') %simpcoreg
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    p1next = Xt(1) + deltat*(a1*(p1star - Xt(1)) + a2*(Xt(2) - Xt(1)));
    p2next = Xt(2) + deltat*(b1*(p2star - Xt(2)) + a2*(Xt(1) - Xt(2)));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'D') %coregulation, two coregulatory terms
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1next = Xt(1) + deltat*(a1*(p1star - Xt(1)) + a2*(Xt(2) - Xt(1)));
    p2next = Xt(2) + deltat*(b1*(p2star - Xt(2)) + b2*(Xt(1) - Xt(2)));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'E') %neg coreg
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    p1next = Xt(1) + deltat*(a1*(p1star - Xt(1)) + a2*(Xt(2) - Xt(1)));
    p2next = Xt(2) + deltat*(b1*(p2star - Xt(2)) - a2*(Xt(1) - Xt(2)));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'F')%test simple selfreg with VAR parameterization
    a1 = Theta(1);
    p1next = Xt(1) + deltat*(a1*(Xt(1)));
    p2next = Xt(2) + deltat*(a1*(Xt(2)));
    fx = [p1next; p2next];
elseif strcmpi (inF.models, 'G') %test selfreg with var
    a1 = Theta(1);
    b1 = Theta(2);
    p1next = Xt(1) + deltat*(a1*(Xt(1)));
    p2next = Xt(2) + deltat*(b1*(Xt(2)));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'H') %test simp coreg with var
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    p1next = Xt(1) + deltat*(a1*(Xt(1)) + a2*(Xt(2)));
    p2next = Xt(2) + deltat*(b1*(Xt(2)) + a2*(Xt(1)));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'I') %test coreg with VAR 
    a1 = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    b2 = Theta(4);
    p1next = Xt(1) + deltat*(a1*(Xt(1)) + a2*(Xt(2)));
    p2next = Xt(2) + deltat*(b1*(Xt(2)) + b2*(Xt(1)));
    fx = [p1next; p2next];
elseif strcmpi(inF.models, 'J') %negcoreg with var
    a1  = Theta(1);
    a2 = Theta(2);
    b1 = Theta(3);
    p1next = Xt(1) + deltat*(a1*(Xt(1)) + a2*(Xt(2)));
    p2next = Xt(2) + deltat*(b1*(Xt(2)) - a2*(Xt(1)));
    fx = [p1next; p2next];
    
end 

