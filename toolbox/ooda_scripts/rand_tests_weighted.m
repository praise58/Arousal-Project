%%% Script to test random data through OODA

set(0, 'DefaultFigureVisible', 'off'); %don't want figures to be visible
num_nodes = 264;
num_subs = 50;
group_names = {'rand1','rand2'};
outdir = '/data/cn5/caterina/Scripts/OODA/rand_perm_weighted3/';
if ~exist(outdir)
    mkdir(outdir);
end
nperms = 100;

for n = 1:nperms
    %create uniformly distributed matrices with numbers between 0 and 1 for 30
    %sample subjects (15 "paired")
    rand_mat_orig = rand(num_nodes,num_nodes,num_subs);
    groups = [ones(1,num_subs/2) ones(1,num_subs/2)*2];
    
    % make half the values in the matrix negative (more similar to our actual
    % distribution)
    mult_rand = randsample([-1 1],size(rand_mat_orig(:)),true);
    mult_rand = reshape(mult_rand,size(rand_mat_orig));
    rand_mat = rand_mat_orig .* mult_rand;
    
    % set up groups
    run_OODA_weighted_groups(rand_mat,groups,['rand_weighted_' num2str(n)],group_names,outdir,'keep');
end

%%%% Do Plotting %%%%
for n = 1:nperms
    fname = [outdir 'OODA/rand_weighted_' num2str(n) '_rand1vrand2_pvalue.txt'];
    pvals(n) = load(fname);
end

figure;
hist(pvals);