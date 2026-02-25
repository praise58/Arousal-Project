%% 2.19.2026 PK. I'm moving all of my visualizations to this script.
%% 2.2.2026 PK. This code checks how similar between subjects correlations are with each other.
% Each connectivity matrix is 286 x 286. I only need the upper triangle,
% minus the identity. That means I only need the matrices of (286 / 2) - 1.
% That would equal 142 x 142 matrices.
path = fullfile("C:\Users", "tempu", "Downloads", "research", "labs", "gratton", "Arousal-Project");
load(fullfile(path, "nbs", "between", "between_matrices.mat"))

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

%% 2.5.2026 PK. Visualize btw_corr using heatmap.
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


%% 2.9.2026 PK. I need to make separate heatmaps for awake and sleepy, then subtract.
matrix_A = between_matrices(:, :, 1:20);
matrix_S = between_matrices(:, :, 21:40);

% Average the conditions' matrices within each other.
avr_A = mean(matrix_A, 3);
avr_S = mean(matrix_S, 3);

avr_AS = avr_A - avr_S;
avr_SA = avr_S - avr_A;

%% 2.16.2026 PK. Display just the averaged heatmaps within conditions for btw subjects.

% I will use figure_corrmat_GrattonLab() from the Gratton Lab general repo to construct the heat maps.
% figure_corrmat_GrattonLab(matrix, atlasparams, varargin, titletext) varargin = -1, 1

addpath(genpath("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\toolbox\FCProcess"))
addpath(genpath("C:\Users\tempu\Downloads\research\labs\gratton\GrattonLab-General-Repo-20251225T012749Z-1-001\GrattonLab-General-Repo\motion_calc_utilities"))
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\brain masks\atlas_params_v3.mat")
load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\visualizations\better_jet_colormap.mat")

% btw
h1 = corr_mat_subplot(avr_A, atlas_params, subplot(1, 3, 1), "FC for High Arousal Group", -1, 1);
h2 = corr_mat_subplot(avr_S, atlas_params, subplot(1, 3, 2), "FC for Low Arousal Group", -1, 1);
h3 = corr_mat_subplot(avr_AS, atlas_params, subplot(1, 3, 3),"FC High - Low", -.4, .4);

%% 2.19.2026 PK. Visualize heatmap differences within subject conditions.
path = fullfile("C:\Users", "tempu", "Downloads", "research", "labs", "gratton", "Arousal-Project");
load(fullfile(path, "nbs", "within", "within_matrices.mat"))

A_prec_mat = within_matrices(:, :, [1 2 7]);
S_prec_mat = within_matrices(:, :, [14 15 21]);
AS_prec_mat = A_prec_mat - S_prec_mat;


% PM01 FC
h4 = corr_mat_subplot(A_prec_mat(:, :, 1), atlas_params, subplot(3, 3, 1), "FC PM001 High Arousal", -1, 1);
h5 = corr_mat_subplot(S_prec_mat(:, :, 1), atlas_params, subplot(3, 3, 2), "FC PM001 Low Arousal", -1, 1);
h6 = corr_mat_subplot(AS_prec_mat(:, :, 1), atlas_params, subplot(3, 3, 3), "FC PM001 High - Low", -.4, .4);

% INET002 FC
h7 = corr_mat_subplot(A_prec_mat(:, :, 2), atlas_params, subplot(3, 3, 4), "FC INET002 High Arousal", -1, 1);
h8 = corr_mat_subplot(S_prec_mat(:, :, 2), atlas_params, subplot(3, 3, 5), "FC INET002 Low Arousal", -1, 1);
h9 = corr_mat_subplot(AS_prec_mat(:, :, 2), atlas_params, subplot(3, 3, 6), "FC INET002 High - Low", -.4, .4);

% INET063 FC
h10 = corr_mat_subplot(A_prec_mat(:, :, 3), atlas_params, subplot(3, 3, 7), "FC INET063 High Arousal", -1, 1);
h11 = corr_mat_subplot(S_prec_mat(:, :, 3), atlas_params, subplot(3, 3, 8), "FC INET063 Low Arousal", -1, 1);
h12 = corr_mat_subplot(AS_prec_mat(:, :, 3), atlas_params, subplot(3, 3, 9), "FC INET063 High - Low", -.4, .4);

set(gcf, 'Position', [100 100 1400 600]);

%% run t tests (sleep > awake)