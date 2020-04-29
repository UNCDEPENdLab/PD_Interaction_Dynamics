%this code matches perfectly the original code
%okay, this is the file that the original chews on
ibi = load('/Users/michael/Tresors/Couples_Interaction/RSAseconds/Data/ps8102.txt');
[rsaseries, countmissing] = DynRSAOneFile(ibi, .12, .40, 1, 4);
xlswrite('ps8102_resamp', rsaseries)

%perfect match to file from RSAActiheart2
%RSAActiheart2('Data', .12, .40)

%this code uses the data that have already been spline interpolated,
%sampled onto the same (across partners) aligned 10Hz grid, and
%matching light on events
ibi = load('/Users/michael/Tresors/Couples_Interaction/RSAseconds/Data/as8102_noresamp.txt');
[rsaseries, countmissing] = DynRSAOneFile(ibi, .12, .40, 0, 10);
xlswrite('as8102_noresamp', rsaseries)
