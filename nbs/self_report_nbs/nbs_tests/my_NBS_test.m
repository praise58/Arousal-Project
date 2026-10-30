function my_NBS_test(thres, test, hyp, design, matrices)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This script runs the Networks-based statistic (NBS) toolbox without the %
% need of the GUI. (More info about NBS in https://doi.org/10.1016/j.     %
% neuroimage.2010.06.041                                                  %
% This script detects differences between the ASD and TC for one smoothing%
% level at a time. All the parameters to change are at the beginning of   %
% the file, according to the NBS documentation.                           %
%                                                                         %
% 26.09.2019 Modified by Ana T. Added comments on the contents of the file%
% 03.10.2019 Modified by Ana T. Changed the file to be compatible with    %
%            the triton computational cluster. AKA submit many jobs at =t %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%add required paths
addpath(genpath("C:\Users\tempu\Downloads\research\labs\gratton\NBS1.2"));
%PC^
%addpath(genpath("/projects/illinois/las/psych/cgratton/networks-pm/arousal/Arousal-Project/toolbox/NBS1.2")) % Cluster

% paths
%cluster_path = "/projects/illinois/las/psych/cgratton/networks-pm/arousal/Arousal-Project/";
pc_path = "C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\";

NBS_fname = test + "_thres" + thres + "_" + hyp + "_" + design + ".mat";
savepath = pc_path + "nbs/nbs_tests/" + NBS_fname;

matrices = "precision_matrices.mat";
data_path = pc_path + "nbs/within/" + matrices;

design_matrices = "precision_design_matrix.mat";
design_path = pc_path + "nbs/" + design + "/" + design_matrices;

labels = "labels.txt";
labels_path = pc_path + "nbs/" + labels;

node_coord = "node_coordinates.txt";
node_path = pc_path + "nbs/" + node_coord;

exchange_block = "precision_exchange_block.mat";
exchange_path = pc_path + "nbs/" + design + "/" + exchange_block;


% Other parameters
%thres = 4.00; 
pipeline = 'Forward'; %'Forward' or 'Inverse'
%test = 't-test';
N = 286; %number of ROIs 286 for PK sample
data = data_path;
design_mat = design_path;
labels = labels_path;
node_coord = node_path;
exchange_block = exchange_path;

%% Config the structure for the NBS 
UI.method.ui = 'Run NBS'; %'Run NBS' | 'Run FDR'
UI.test.ui = test; %'One Sample' | 't-test' | 'F-test'
UI.size.ui = 'Extent'; %'Extent' | 'Intensity'
UI.thresh.ui = num2str(thres); 
UI.perms.ui = '5000'; 
UI.alpha.ui = '0.05'; 
if hyp == "Spos"
    UI.contrast.ui = '[-1 1]';%'[0 0 0 0 0 1]'; %which group is assumed to be bigger?
elseif hyp == "Apos"
    UI.contrast.ui = '[1 -1]'; % Assuming the other group is larger
end
UI.design.ui = design_mat; 
UI.exchange.ui = exchange_block; 
UI.matrices.ui = data; 
UI.node_coor.ui = node_coord;                         
UI.node_label.ui = labels; 

run NBSrun(UI,[]);
global nbs

save(savepath,'nbs')

end