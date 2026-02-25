% i copy pasted these but idk where they came from
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
