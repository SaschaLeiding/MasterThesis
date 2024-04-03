N  = 2;             
J  = 11;            
Y  = 12;            
us = N;             
yvec   = 2003:2014; 
pollutants = {'CO2Total', 'CO2ElectricityHeat'};


%pm.alpha = [.0040; .0022; .0103; .0223; .0212; .0205; .0048; .0303; .0557; .0019; .0015; .0023; .0005; .0014; .0016; .0019; .0047];
% Electricitiy alpha
pm.alpha = [0.015312467; 0.011947714; 0.009090094; 0.010930445; 0.310747175;
                0.023011401;0.018743139;0.031608299;0.017921373;0.011879142;0.009118925;]
% Total alpha
%pm.alpha = [0.022921654; 0.010355064; 0.009922657; 0.015046847; 1.424171113; 
%                0.042434938;0.016481261; 0.035719722; 0.025509975; 0.013410227; 0.009189898]
%inputshare = [.74;.79;.83;.79;.88;.70;.78;.73;.85;.79;.76]%;.81;.79;.65; .82;.74;.73];
inputshare = [0.9020178; 0.8976492; 0.8844424; 0.8749241; 0.9920765; 0.6990549
                0.8257984;0.8468596;0.9007181; 0.8659748; 0.8857564]
pm.sigma = (1-pm.alpha)./((1-pm.alpha)-inputshare);  
%pm.theta = [4.81; 5.38; 8.30; 4.29; 17.52; 4.13; 5.02; 3.39; 9.72; 5.60; 4.30; 5.07; 4.13; 2.09; 5.29; 3.27; 4.77];
pm.theta =[3.89; 4.80; 6.20; 5.21; 9.91; 3.50; 4.62; 4.05; 10.01; 4.19; 3.87]


%pm.dirty = [4 5 6 8 9];
%pm.clean = [1 2 3 7 10 11 12 13 14 15 16 17];