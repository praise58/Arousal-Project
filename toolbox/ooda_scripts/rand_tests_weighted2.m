%%% Script to test random data through OODA
% Mixing up groups in original data

set(0, 'DefaultFigureVisible', 'off'); %don't want figures to be visible

% Load original data
load('/data/cn5/caterina/TaskConn_Methods/all_data/block_segment_analysis_filtertmask_orig_taskresid_fc_scrubbed_Power/corrmats_OODA.mat');
nsubs = 25;

% Constants
nperms = 250;
group_names = {'rand1','rand2'};
outdir = '/data/cn5/caterina/Scripts/OODA/rand_perm_weighted2/';
if ~exist(outdir)
    mkdir(outdir);
end

for n = 1:nperms
    
    % Create random group assignments and reorder data_OODA matrix
    rand_ind = randperm(nsubs*2);

    % make a new random matrix based on this permutation
    rand_data = data_ooda(:,:,rand_ind);
    
    % groups always kept the same! Since order permuted.
    groups = [ones(1,nsubs) ones(1,nsubs)*2];
    
    % run weighted OODA
    run_OODA_weighted_groups(rand_data,groups,['rand_weighted_' num2str(n)],group_names,outdir,'keep');
end

%%%% Do Plotting %%%%
for n = 1:nperms
    fname = [outdir 'OODA/rand_weighted_' num2str(n) '_rand1vrand2_pvalue.txt'];
    pvals(n) = load(fname);
end

figure;
hist(pvals);
