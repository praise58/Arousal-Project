%%% Script to test random data through OODA

set(0, 'DefaultFigureVisible', 'off'); %don't want figures to be visible
num_nodes = 264;
num_subs = 1000;
group_names = {'rand1','rand2'};
outdir = '/data/cn5/caterina/Scripts/OODA/rand_out4/';
if ~exist(outdir)
    mkdir(outdir);
end

%create uniformly distributed matrices with numbers between 0 and 1 for 30
%sample subjects (15 "paired")
rand_mat_orig = rand(num_nodes,num_nodes,num_subs); 
groups = [ones(1,num_subs/2) ones(1,num_subs/2)*2];

% make half the values in the matrix negative (more similar to our actual
% distribution)
mult_rand = randsample([-1 1],size(rand_mat_orig(:)),true);
mult_rand = reshape(mult_rand,size(rand_mat_orig));
rand_mat = rand_mat_orig .* mult_rand;

% loop through a set of thresholds and binarize the matrix, prep it as we usually do
thresholds = [0.02:0.01:0.1 0.15:0.05:0.3];
rand_mat_bin = zeros(size(rand_mat,1),size(rand_mat,2),size(rand_mat,3),length(thresholds));
for t = 1:length(thresholds)
    for m = 1:size(rand_mat,3)
        rand_mat_bin(:,:,m,t) = prep_matrix(rand_mat(:,:,m),thresholds(t));
    end
end

% set up groups
bingat_by_thresh(rand_mat_bin,groups,'rand_arr',group_names,thresholds,outdir);

% PLOT some things out
mask = ones(num_nodes);
mask = triu(mask,1);

figure('Position',[200 200 1000 1000]);
for t = 1:length(thresholds)
    fname = sprintf('%sOODA/rand_arr_rand1vrand2_t%f_gstarG1.txt',outdir,thresholds(t));
    arr = importdata(fname);
    matrix = zeros(num_nodes);
    matrix(logical(mask)) = arr;
    matrix_fin = matrix + matrix';
    
    fname = sprintf('%sOODA/rand_arr_rand1vrand2_t%f_pvalue.txt',outdir,thresholds(t));
    pval = importdata(fname);
    
    subplot(4,4,t)
    imagesc(matrix_fin);
    title_str = [num2str(thresholds(t)) ', p=' num2str(pval)];
    if isempty(find(arr))
        title_str = [title_str ', EMPTY'];
    end
    title(title_str);
end
save_fig(gcf,[outdir 'summary_gstarG1.png']);

figure('Position',[200 200 1000 1000]);
for t = 1:length(thresholds)
    fname = sprintf('%sOODA/rand_arr_rand1vrand2_t%f_gstarG2.txt',outdir,thresholds(t));
    arr = importdata(fname);
    matrix = zeros(num_nodes);
    matrix(logical(mask)) = arr;
    matrix_fin = matrix + matrix';
    
    fname = sprintf('%sOODA/rand_arr_rand1vrand2_t%f_pvalue.txt',outdir,thresholds(t));
    pval = importdata(fname);
    
    subplot(4,4,t)
    imagesc(matrix_fin);
    title_str = [num2str(thresholds(t)) ', p=' num2str(pval)];
    if isempty(find(arr))
        title_str = [title_str ', EMPTY'];
    end
    title(title_str);
end
save_fig(gcf,[outdir 'summary_gstarG2.png']);
close('all');