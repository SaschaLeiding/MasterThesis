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
    
    %if mainpoll == 2 run r2.m; end
    %                 run r2.m
    %if mainpoll == 7 run r3.m; end
    writematrix(Z_hat_o,'ActualEmission.xls')
    writematrix(Z_hat_o_cf,'ShocksTotal.xls')
    writematrix(Z_hat_cf,'ShocksbyIndustry.xls')
    writematrix(w_hat,'w_hat.xls')
    writematrix(M_hat,'M_hat.xls')
    writematrix(t_hat,'t_hat.xls')
end;


% For MATLAB exCOke
for mainpoll = 2 
    mainpoll
    clearvars -except options tol mainpoll;

    load 'dataMATLAB'/allDNK_exCoke.mat
    
    run p2_exCoke.m
    run p3.m
    
    %if mainpoll == 2 run r2.m; end
    %                 run r2.m
    %if mainpoll == 7 run r3.m; end
    writematrix(Z_hat_o,'ActualEmission_exCoke.xls')
    writematrix(Z_hat_o_cf,'ShocksTotal_exCoke.xls')
    writematrix(Z_hat_cf,'ShocksbyIndustry_exCoke.xls')
    writematrix(w_hat,'w_hat_exCoke.xls')
    writematrix(M_hat,'M_hat_exCoke.xls')
    writematrix(t_hat,'t_hat_exCoke.xls')
    writematrix(wwM_hat,'wwM_hat_exCoke.xls')

end;
