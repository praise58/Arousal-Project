%% 2.19.2026 PK. I'm moving all of my visualizations to this script.
%% 2.2.2026 PK. This code checks how similar between subjects correlations are with each other.
% Each connectivity matrix is 286 x 286. I only need the upper triangle,
% minus the identity. That means I only need the matrices of (286 / 2) - 1.
% That would equal 142 x 142 matrices.

% Choose main_root:
if loc == "home"
    main_root = fullfile("C:\Users", "tempu", "Arousal-Project");
elseif loc == "lab"
    disp("You didn't write code for this loc")
    return
end

% Load toolboxes
addpath(genpath(main_root, "toolbox"));
addpath(genpath("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\toolbox\avg_network_corr.m"))

% Between-matrices file location and load:
matricesPath = fullfile(main_root, "nbs", "larger_sample", "full_matrices_id-rm.mat");
between_matrices = load(matricesPath);
struct_name = fieldnames(between_matrices);
struct_name = struct_name{1};
between_matrices = between_matrices.(struct_name);

% Load atlas_params
atlas_params = load(fullfile(main_root, "brain_masks", "atlas_params_v3.mat"));
atlas_params = atlas_params.atlas_params;

%%-----------BUILD MATRICES --- RUN FIRST -----------%%
% 2.9.2026 PK. I need to make separate heatmaps for awake and sleepy, then subtract.
matrix_A = between_matrices(:, :, 1:94);
matrix_S = between_matrices(:, :, 95:146);

% Average the individual matrices by  condition.
avr_A = mean(matrix_A, 3);
avr_S = mean(matrix_S, 3);

% Compute the difference matrices
avr_AS = avr_A - avr_S;
avr_SA = avr_S - avr_A; % This is the main difference matrix

% 5.1.2026 PK. Average group networks by network
avr_A_avr = avg_network_corr(avr_A, atlas_params);
avr_S_avr = avg_network_corr(avr_S, atlas_params);
avr_SA_avr = avg_network_corr(avr_SA, atlas_params);

% Load within_matrices
within_matrices = load(fullfile(main_root, "nbs", "larger_sample_nbs", "within_matrices_id-rm.mat")); % change path as needed
struct_name = fieldnames(within_matrices);
struct_name = struct_name{1};
within_matrices = within_matrices.(struct_name);

% !!![change range based on the concatenated precision matrices.]!!!%
within_S_idx = 1:18;
within_A_idx = 19:36;

%% 4.29.2026 PK. Average correlations by the network for precision subj.

A_prec_mat = within_matrices(:, :, within_S_idx); 
S_prec_mat = within_matrices(:, :, within_A_idx);
SA_prec_mat = S_prec_mat - A_prec_mat;

num_prec_subj = size(within_matrices, 3);
within_matrices_avr = nan(286, 286, num_prec_subj);

for i = 1:num_prec_subj
    within_matrix = within_matrices(:, :, i);

    within_matrix_avr = avg_network_corr(within_matrix, atlas_params);

    within_matrices_avr(:, :, i) = within_matrix_avr;
end

A_prec_mat_avr = within_matrices_avr(:, :, within_A_idx);
S_prec_mat_avr = within_matrices_avr(:, :, within_S_idx);
SA_prec_mat_avr = S_prec_mat_avr - A_prec_mat_avr;

%%-----------------------------------------------------------------------%%
%% Similarity Matrix
corr_array = {};
num_sub = size(between_matrices, 3);

for i = 1:num_sub
    matrix = between_matrices(:, :, i);
    
    % triu() only preserves the upper right triangle.
    tri = triu(matrix, 1);
    
    % Turns it into a vector.
    vec = tri(:);
    
    % Remove the 0s (repeats).
    vec(vec == 0) = [];
    
    % Store the subject's correlations in a cell array.
    corr_array{i} = vec;
end

btw_matrix = [corr_array{:}];

% Correlate each subject's vectorized correlations with every other
% subject's vectorized correlations.
btw_corr = corr(btw_matrix);

% 2.5.2026 PK. Visualize btw_corr using heatmap.
labels = {"A" + (1:20), "S" + (1:20)};
labels = [labels{:}];
labels = string(labels);

btw_corr_h = heatmap(labels, labels, btw_corr);
btw_corr_h.ColorLimits = [.09 1];
btw_corr_h.Colormap = turbo(200);
btw_corr_h.ColorbarVisible = 'on';
btw_corr_h.Title = "Between Subject Similarity Matrix";
btw_corr_h.XLabel = "Subjects";
btw_corr_h.YLabel = "Subjects";

%%-----------------------------------------------------------------------%%
%% 2.16.2026 PK. Display just the averaged heatmaps within conditions for 
% btw subjects.

% I will use figure_corrmat_GrattonLab() from the Gratton Lab general repo to construct the heat maps.
% figure_corrmat_GrattonLab(matrix, atlasparams, varargin, titletext) varargin = -1, 1

addpath(genpath("C:\Users\tempu\Downloads\research\labs\gratton\Arousal Project Gratton Lab\GrattonLab-General-Repo-20251225T012749Z-1-001\GrattonLab-General-Repo\FCProcess"))
addpath(genpath("C:\Users\tempu\Downloads\research\labs\gratton\Arousal Project Gratton Lab\GrattonLab-General-Repo-20251225T012749Z-1-001\GrattonLab-General-Repo\motion_calc_utilities"))
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\brain_masks\atlas_params_v3.mat")
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\better_jet_colormap.mat")

% btw
h1 = corr_mat_subplot(avr_A, atlas_params, subplot(1, 3, 1), "FC for High Arousal Group", -.8, .8);
h2 = corr_mat_subplot(avr_S, atlas_params, subplot(1, 3, 2), "FC for Low Arousal Group", -.8, .8);
h3 = corr_mat_subplot(avr_SA, atlas_params, subplot(1, 3, 3),"FC Low - High", -.4, .4);

% Averaged by network pair group level.
figure;
h7 = corr_mat_subplot(avr_S_avr, atlas_params, subplot(1, 3, 1), "FC Low Arousal", -.4, .4);
h8 = corr_mat_subplot(avr_A_avr, atlas_params, subplot(1, 3, 2), "FC High Arousal", -.4, .4);
h9 = corr_mat_subplot(avr_SA_avr, atlas_params, subplot(1, 3, 3), "FC Low - High", -.2, .2);

% Re-apply colormap to each axes explicitly after all subplots are drawn
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\better_jet_colormap.mat")
colormap(h7, better_jet_colormap_diff);
colormap(h8, better_jet_colormap_diff);
colormap(h9, better_jet_colormap_diff);

drawnow;
exportgraphics(gcf, fullfile(main_root, 'visualizations\larger_sample_vis\full_fc_matrices_v2.tiff'), 'Resolution', 1000);


%% 2.19.2026 PK. Visualize heatmap differences within subject conditions.

% INET024 FC
figure;
h4 = corr_mat_subplot(S_prec_mat(:, :, 9), atlas_params, subplot(1, 3, 1), "FC INET024 Low Arousal", -1, 1);
h5 = corr_mat_subplot(A_prec_mat(:, :, 9), atlas_params, subplot(1, 3, 2), "FC INET024 High Arousal", -1, 1);
h6 = corr_mat_subplot(SA_prec_mat(:, :, 9), atlas_params, subplot(1, 3, 3), "FC INET024 Low - High", -1, 1);

% PM01 FC
h4 = corr_mat_subplot(S_prec_mat(:, :, 18), atlas_params, subplot(1, 3, 1), "FC PM01 Low Arousal", -1, 1);
h5 = corr_mat_subplot(A_prec_mat(:, :, 18), atlas_params, subplot(1, 3, 2), "FC PM01 High Arousal", -1, 1);
h6 = corr_mat_subplot(SA_prec_mat(:, :, 18), atlas_params, subplot(1, 3, 3), "FC PM01 Low - High", -1, 1);

% Re-apply colormap to each axes explicitly after all subplots are drawn
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\better_jet_colormap.mat")
colormap(h4, better_jet_colormap_diff);
colormap(h5, better_jet_colormap_diff);
colormap(h6, better_jet_colormap_diff);

drawnow;
exportgraphics(gcf, 'C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\pm001_within-sub_fig.tiff', 'Resolution', 600);

% INET002 FC
figure;
h7 = corr_mat_subplot(A_prec_mat(:, :, 2), atlas_params, subplot(1, 3, 1), "FC INET002 High Arousal", -1, 1);
h8 = corr_mat_subplot(S_prec_mat(:, :, 2), atlas_params, subplot(1, 3, 2), "FC INET002 Low Arousal", -1, 1);
h9 = corr_mat_subplot(SA_prec_mat(:, :, 2), atlas_params, subplot(1, 3, 3), "FC INET002 Low - High", -1, 1);

% Re-apply colormap to each axes explicitly after all subplots are drawn
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\better_jet_colormap.mat")
colormap(h7, better_jet_colormap_diff);
colormap(h8, better_jet_colormap_diff);
colormap(h9, better_jet_colormap_diff);

drawnow;
exportgraphics(gcf, 'C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\inet002_within-sub_fig.tiff', 'Resolution', 600);

% INET063 FC
figure;
h10 = corr_mat_subplot(A_prec_mat(:, :, 3), atlas_params, subplot(1, 3, 1), "FC INET063 High Arousal", -.9, .9);
h11 = corr_mat_subplot(S_prec_mat(:, :, 3), atlas_params, subplot(1, 3, 2), "FC INET063 Low Arousal", -1, 1);
h12 = corr_mat_subplot(SA_prec_mat(:, :, 3), atlas_params, subplot(1, 3, 3), "FC INET063 Low - High", -1, 1);

% Re-apply colormap to each axes explicitly after all subplots are drawn
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\better_jet_colormap.mat")
colormap(h10, better_jet_colormap_diff);
colormap(h11, better_jet_colormap_diff);
colormap(h12, better_jet_colormap_diff);

drawnow;
exportgraphics(gcf, 'C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\inet063_within-sub_fig.tiff', 'Resolution', 600);

%% Visualize correlations by network
% PM01 FC
figure;
h4 = corr_mat_subplot(S_prec_mat_avr(:, :, 18), atlas_params, subplot(1, 3, 1), "FC PM001 Low Arousal", -1, 1);
h5 = corr_mat_subplot(A_prec_mat_avr(:, :, 18), atlas_params, subplot(1, 3, 2), "FC PM001 High Arousal", -1, 1);
h6 = corr_mat_subplot(SA_prec_mat_avr(:, :, 18), atlas_params, subplot(1, 3, 3), "FC PM001 Low - High", -.8, .8);
exportgraphics(gcf, fullfile(main_root, 'visualizations\larger_sample_vis\pm01_fc_matrices.tiff'), 'Resolution', 1000);

% Re-apply colormap to each axes explicitly after all subplots are drawn
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\better_jet_colormap.mat")
colormap(h4, better_jet_colormap_diff);
colormap(h5, better_jet_colormap_diff);
colormap(h6, better_jet_colormap_diff);

drawnow;
exportgraphics(gcf, 'C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\pm001_within-sub_avr_fig.tiff', 'Resolution', 1000);

% INET017 FC
figure;
h4 = corr_mat_subplot(S_prec_mat_avr(:, :, 2), atlas_params, subplot(1, 3, 1), "FC INET017 Low Arousal", -1, 1);
h5 = corr_mat_subplot(A_prec_mat_avr(:, :, 2), atlas_params, subplot(1, 3, 2), "FC INET017 High Arousal", -1, 1);
h6 = corr_mat_subplot(SA_prec_mat_avr(:, :, 2), atlas_params, subplot(1, 3, 3), "FC INET017 Low - High", -1, 1);
exportgraphics(gcf, fullfile(main_root, 'visualizations\larger_sample_vis\inet017_fc_matrices.tiff'), 'Resolution', 1000);

% Re-apply colormap to each axes explicitly after all subplots are drawn
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\better_jet_colormap.mat")
colormap(h4, better_jet_colormap_diff);
colormap(h5, better_jet_colormap_diff);
colormap(h6, better_jet_colormap_diff);

drawnow;
exportgraphics(gcf, 'C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\inet063_within-sub_avr_fig.tiff', 'Resolution', 1000);

% INET002 FC
figure;
h4 = corr_mat_subplot(S_prec_mat_avr(:, :, 3), atlas_params, subplot(1, 3, 1), "FC INET002 Low Arousal", -1, 1);
h5 = corr_mat_subplot(A_prec_mat_avr(:, :, 3), atlas_params, subplot(1, 3, 2), "FC INET002 High Arousal", -1, 1);
h6 = corr_mat_subplot(SA_prec_mat_avr(:, :, 3), atlas_params, subplot(1, 3, 3), "FC INET002 Low - High", -1, 1);

% Re-apply colormap to each axes explicitly after all subplots are drawn
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\better_jet_colormap.mat")
colormap(h4, better_jet_colormap_diff);
colormap(h5, better_jet_colormap_diff);
colormap(h6, better_jet_colormap_diff);

drawnow;
exportgraphics(gcf, 'C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\inet002_within-sub_avr_fig.tiff', 'Resolution', 1000);
