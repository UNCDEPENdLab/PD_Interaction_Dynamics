function moveHF(direction)
%
% function moveHF(direction)
%
% direction 1=HF_lower down; 2=HF_lower up; 3=HF_upper down; 4=HF_upper up


global EKG;

if direction < 3   %move HF_lower
    if direction == 1
        EKG.HF_lower = EKG.HF_lower - 0.01;
    else
        EKG.HF_lower = EKG.HF_lower + 0.01;
    end
else   %move HF_upper
    if direction == 3
        EKG.HF_upper = EKG.HF_upper - 0.01;
    else
        EKG.HF_upper = EKG.HF_upper + 0.01;
    end
end

drawPsdPlot;
