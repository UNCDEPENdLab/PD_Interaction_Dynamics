function [fx, dF_dX, dF_dTheta] = dynphysio_evolution_lin2D(Xt,Theta,ut,inF)
deltat = inF.deltat;
a = Theta(1);
b = Theta(2);

A = [   -b  -a
        1   -b];

fx = Xt + deltat.*(A*Xt);
dF_dX = eye(size(Xt,1)) + deltat.*A';
dF_dTheta = deltat.*[-a.*Xt(2),0];
end