clear all
clc
cd('C:\MasterThesis\MATLAB\');
%addpath('E:\Dropbox\ado\matlab');
addpath('C:\MasterThesis\MATLAB\codeMATLAB')
options = optimset('Display','off','MaxFunEvals',60000,'MaxIter',4500,'TolFun',1e-14,'TolX',1e-14,'Algorithm','trust-region-dogleg');
    

for mainpoll = 2 
    mainpoll
    clearvars -except options tol mainpoll;

    load 'dataMATLAB'/allDNK.mat
    
    run p2.m
    run p3.m
    
    if mainpoll == 2 run r2.m; end
    %                 run r2.m
    %if mainpoll == 7 run r3.m; end
   

end;

    writematrix(Z_hat_o,'ActualEmission.xls')
    writematrix(Z_hat_o_cf,'ShocksTotal.xls')
    writematrix(Z_hat_cf,'ShocksbyIndustry.xls')
    writematrix(Z_hat_cf,'ShocksbyIndustry.xls')
    writematrix(wwM_hat,'wwM_hat.xls')
    writematrix(t_hat,'t_hat.xls')
