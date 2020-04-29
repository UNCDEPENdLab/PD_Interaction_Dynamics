function [posterior, out] = modelcomparison_fitIBISystem(file, showfig, model, id)
%I believe we should be able to use the g_Id observation function
%This is just an identity mapping that is matrix-compatible.
%It allows for a scaling parameter: inG.scale that multiplies the state
%vector by some scalar value. Not sure how useful this would be.

%file='/storage/home/mnh5174/Tresors/Couples_Interaction/example_dyad_ibis.txt';
%file='/Users/michael/Tresors/Couples_Interaction/example_dyad_ibis.txt';
%file='/Users/ams939/Box Sync/DEPENd/Couples Data/data/example_dyad_ibis.txt';

if showfig == 1
    options.DisplayWin = 1;
else
    options.DisplayWin = 0;
end

data = dlmread(file);

%assuming PTNUM, time, ibi_interp_detrend_patient, ibi_interp_dtrend_partner, l_dom_interp_detrend, r_dom_interp_detrend, l_aff_interp_detrend,
%r_aff_interp_detrend
data = data(:,[3 4]); %all possibilities

%reduce to first 60 seconds for initial tests
%data = data(1:1200,1);
%y = data(:,1)'; %individual oscillator
%y = data(1:100,:)';
%data = data(1:800,:);
inF.p1star = 0;
inF.p2star = 0;
y = data';
options.isYout = zeros(size(y));
if (id == 8030)
    options.isYout(1,[0:200, 9700:9900, 10000:10200, 10400:10600, 28050:28200, 287500:28900, 29050:29200, 29350:29500, 30000:30200, 43900:44100, 46550:46700, 47100:47300, 52400:52600, 53150:53300, 53450:53600, 54400:54600, 57400:57650, 60200:60400, 60500:60700]) = 1;
elseif (id == 8032)
    options.isYout(1,[450:600, 3200:3700, 8550:8750, 8950:9150, 9550:9700, 10800:11000, 12150:12300, 12500:12650, 12850:13050, 13350:13500, 13500:13750, 22050:22200, 22300:22500, 25550:23000, 23300:23450, 23500:23700, 26200:26400, 26900:27100, 27150:27500, 38250:38400, 42650:42800, 43200:43400, 43700:43900, 44200:44600, 46050:46450, 50700:50850, 51150:51350, 52200:52900, 54050:55300, 57450:57600, 57850:58250, 58600:58750, 59350:59750]) = 1;
elseif (id == 8034)
    options.isYout(2, 1200:1500) = 1;
    options.isYout(1, [22650:22850, 32600:32800, 43450:43650, 50650:50850]) = 1;
elseif (id == 8039)
    options.isYout(2, [0:100, 45000:45150]) = 1;
elseif (id == 8048) %8048l needs to be censored at 14.75-17; 446-451 (1475:1700, 44600-45100); partner
    options.isYout(2, [1475:1700 44600:451000]) = 1;
elseif (id == 8060)
    options.isYout(1,[]) = 1; %add in later 
elseif (id == 8064)
    options.isYout(2, [18950:19350, 20400:20750, 56200:56500]) = 1; 
elseif (id == 8084)
    options.isYout(1, 50500:50700) = 1;
elseif (id == 8100)
    options.isYout(1, [200:300, 4000:4200, 5400:5800, 10300:10500, 10700:10900, 11000:11200, 12400:12600, 19600:19800, 20550:20750, 21550:21750, 22550:22650, 24050:24250, 24450:24850, 28100:28300, 31500:31700, 32550:32750, 34750:34950, 35850:36250, 39350:39550, 41350:41550, 42950:43150, 45250:45550, 47550:48150, 49350:49800, 52050:52250, 54550:54900, 57750:57950, 59500:59700, 60350:60550]) = 1;
elseif (id == 8106)
    options.isYout(1, [6200:6300, 11300:11400, 13300:13450, 17200:17350, 19150:19300, 25750:28050, 33700:33900, 37500:37650; 38550:38700, 41050:45050, 50000:50200, 51250:51400, 51900:52100, 53100:53900, 55900:56050]) = 1; 
elseif (id == 8114) 
    options.isYout(2, [7100:7500, 12100:12250]) = 1;
elseif (id == 8126)
    options.isYout(1, [0:800, 29500:29900, 60400:60800]) = 1;
elseif (id == 8127)
    options.isYout(1, [1200:1400, 3250:32450, 4150:4350, 6050:6350, 7550:7900, 9050:9800, 10150:10550, 11950:12050, 12700:12900, 14500:14700, 15450:15700, 17100:17400, 18150:18300, 21200:21500, 21900:22200, 22600:22900, 24150:24400, 24650:24900, 33350:34100, 35700:36000, 44900:45900, 48350:48700, 57200:57400, 57800:57950, 59500:60250]) = 1;
elseif (id == 8133)
    options.isYout(1, [21100:21350, 31400:31650, 35150:35900, 39500:39650, 44900:46650, 51550:51850, 58250:59200, 59500:59850]) = 1; 
elseif (id == 8134)
    options.isYout(2, 58800:59050) = 1;
elseif (id == 8144)
    options.isYout(2, [350:2300, 18300:18800, 22550:22900, 37100:37250, 54700:54950]) = 1;
elseif (id == 8152)
    options.isYout(1, [40800:41000, 47000:47100, 47700:48000, 48800:49050, 52300:52500]) = 1;
end

delta_t = 0.1; %10Hz series

inF.deltat=delta_t;
inF.models = model;
options.inF = inF;

options.MaxIter = 100;
options.TolFun = 1e-3;
options.GnMaxIter = 100;
options.GnTolFun = 1e-3;
n_t = size(data, 1);
%add in if statement so that dim.n_theta reflects the actual number of
%parameters being estimated
if strcmpi(inF.models, 'A') %traditional coreg with p1star and p2star estimated (so baseline can be other than 0)
   dim.n_theta = 6;
elseif strcmpi(inF.models, 'B') %traditional coreg 
    dim.n_theta = 4;
elseif strcmpi(inF.models, 'C') %new coreg with p1star and p2star estimated (so baseline can be other than 0)
    dim.n_theta = 6;
elseif strcmpi(inF.models, 'D') % new coregulation with p1star and p2star not estimated
    dim.n_theta = 4;
end 

dim.n_phi = 0; %identity observation function
dim.n = size(data, 2); %number of physiological signals

options.backwardLag = 16; %number of observations to consider in making prediction of next sample (lagged Kalman filter)

%priors.muX0 = 0*ones(dim.n, 1); %initial IBI signal values. If not demeaned, might be avg IBI value
priors.muX0 = [y(1,1); y(2,1)]; %use observed initial values of IBI series
priors.SigmaX0 = 100*eye(dim.n); %zero covariance in prior. sd of 10 in initial states (since it should be close to observed)
priors.muTheta = 0*ones(dim.n_theta, 1);
priors.SigmaTheta = eye(dim.n_theta); %all coupling parameters are zero centered and max ~1.0

%fit deterministic system
%priors.a_alpha = Inf;
%priors.b_alpha = 0;
priors.a_alpha = 1e0;
priors.b_alpha = 1e1;
priors.a_sigma = 1e1;
priors.b_sigma = 1e1;
options.priors = priors;
f_fname = @modelcomparison_dynphysio_evolution;
g_fname = @g_Id;
u = zeros(1, n_t);

%set options to have y out for participants whose data is untrustworthy 
%8034r drop 12-15seconds (time points 1200-1500)
%partner so drop second time series: id (27) --verify before fitting

[posterior,out] = VBA_NLStateSpaceModel(y,u,f_fname,g_fname,dim,options);
hfp = findobj('tag','VBNLSS');
set(hfp,'tag','0','name','inversion with delays');


