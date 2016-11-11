function [fx] = dynphysio_evolution_onesignal(Xt,Theta,ut,inF)
% Evolution function for individual physiological data
%
% Just include the individual damping parameter, a1

%this is the integration time step (Euler method)
%unclear whether this is needed in our application. In demo, deltat=1.0
%deltat = inF.deltat;
p1star=inF.p1star;
p2star=inF.p2star;
a1 = Theta(1);

%p1next = a1*(p1star - Xt(1)) + a2*(Xt(2) - Xt(1));
%p2next = b1*(p2star - Xt(2)) + b2*(Xt(1) - Xt(2));

%p1next = Xt(1) + a1*(p1star - Xt(1));
p1next = Xt + a1*(p1star - Xt);
fx = p1next;

%dF_dX,dF_dTheta

end