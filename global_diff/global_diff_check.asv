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
labels_A  = {"A" + (1:20)};
labels_A = [labels_A(:)];
labels_A = string(labels_A);

labels_S = {"S" + (1:20)};
labels_S = [labels_S(:)];
labels_S = string(labels_S);

M_between_awake = between_matrices(:, :, 1:20);
M_between_sleepy = between_matrices(:, :, 21:40);

corr_array_A = {};
corr_array_S = {};

num_sub = size(M_between_awake, 3);

matrix_A = M_between_awake(:, :, i);
matrix_S = M_between_sleepy(:, :, i);

% Average the conditions' matrices within each other.
avr_A = mean(matrix_A, 3);
avr_S = mean(matrix_S, 3);

btw_subtracted_corr_as = avr_A - avr_S;
btw_subtracted_corr_sa = avr_S - avr_A;


btw_subtracted_corr_as_h = heatmap(btw_subtracted_corr_as);
btw_subtracted_corr_as_h.ColorLimits = [.09 1];
btw_subtracted_corr_as_h.Colormap = turbo(200);
btw_subtracted_corr_as_h.ColorbarVisible = 'on';
btw_subtracted_corr_as_h.Title = "Subtracted Between Subject Similarity (Awake - Sleepy)";
btw_subtracted_corr_as_h.XLabel = "Subjects";
btw_subtracted_corr_as_h.YLabel = "Subjects";

btw_subtracted_corr_sa_h = heatmap(btw_subtracted_corr_sa);
btw_subtracted_corr_sa_h.ColorLimits = [.09 1];
btw_subtracted_corr_sa_h.Colormap = turbo(200);
btw_subtracted_corr_sa_h.ColorbarVisible = 'on';
btw_subtracted_corr_sa_h.Title = "Subtracted Between Subject Similarity (Sleepy - Awake)";
btw_subtracted_corr_sa_h.XLabel = "Subjects";
btw_subtracted_corr_sa_h.YLabel = "Subjects";


% Something is very off with these heat maps. The max is above 1, not a
% correlation.

ana_corr_mat = load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\nbs\sub-1_sleepy_example_matrix.mat");
ana_corr_mat = ana_corr_mat.corrmat;
min_ana_comat = min(min(ana_corr_mat));
max_ana_comat = max(max(ana_corr_mat));

%% 2.16.2026 PK. Display just the heatmaps of the condit
figure;
subplot(1, 3, 1)
h1 = heatmap(avr_A, 'Colormap', jet)
h1.GridVisible = 'off';
axis equal; 

subplot(1, 3, 2)
h2 = heatmap(avr_S, 'Colormap', jet)
h2.GridVisible = 'off';
axis equal; 

subplot(1, 3, 3)
h3 = heatmap(avr_A - avr_S, 'Colormap', jet)
h3.GridVisible = 'off';
axis equal; 

%% Visualize heatmap differences within subject conditions.

%% run t tests (sleep > awake)

%% 1.29.2026 PK. I need to conduct a t-test on whether there are differences in global activation within and between subject.

btw_mat_awake = between_matrices(:, :, 1:20);
btw_mat_sleepy = between_matrices(:, :, 21:40);

avr_btw_mat_awake = mean(btw_mat_awake, 3);
avr_btw_mat_sleepy = mean(btw_mat_sleepy, 3);

avr_btw_mat_awake = triu(avr_btw_mat_awake);
avr_btw_mat_sleepy = triu(avr_btw_mat_sleepy);

avr_btw_vec_awake = avr_btw_mat_awake(:);
avr_btw_vec_sleepy = avr_btw_mat_sleepy(:);

% ttest2 for independent subjects.
[h, p, ci] = ttest2(avr_btw_vec_awake, avr_btw_vec_sleepy);

%% Histogram global corr differences
figure;
edges = -.4:.025:.6;

hist_awake = histogram(avr_btw_vec_awake, edges, 'Normalization', 'probability', 'FaceAlpha', .3');
hold on
hist_sleepy = histogram(avr_btw_vec_sleepy, edges, 'Normalization', 'probability', 'FaceAlpha', .3');

hist_awake.FaceColor = [0 0.4470 0.7410]; % blue
hist_sleepy.FaceColor = [0.8500 0.3250 0.0980]; % orange

hold off;

%% The histogram distributions look the same, but the ttest is significant, cohen small.

n_sleepy = numel(avr_btw_vec_sleepy); n_awake = numel(avr_btw_vec_awake);
m_sleepy = mean(avr_btw_vec_sleepy); m_awake = mean(avr_btw_vec_awake);
std_sleepy = std(avr_btw_vec_sleepy); std_awake = std(avr_btw_vec_awake);

sp = sqrt(((n_sleepy-1)*std_sleepy^2 + (n_awake-1)*std_awake^2) / (n_sleepy+n_awake-2));
cohens_d = (m_sleepy - m_awake)/sp;
fprintf('Cohen''s d = %.4f\n', cohens_d);


%% 2.6.2026 PK. Same checks for within subject.
clear all

path = fullfile("C:\Users", "tempu", "Downloads", "research", "labs", "gratton", "Arousal-Project");
clear path
load(fullfile(path, "nbs", "within", "within_matrices.mat"))

%% 2.16.2025 PK