%% 12.26.2025 PK, I need to make the design matrix.
%% 1.7.2026 PK, Updated to exclude one subject from each condition (103)

% r = number of subjects (35: awake, 35: sleepy)
% c = number of effects/nuisance variables (2 effects: awake and sleepy)

template = load("C:\Users\tempu\Downloads\research\labs\gratton\NBS1.2\SchizophreniaExample\designMatrix.mat");
template = template.design;
[r, c] = size(template);

design_matrix = zeros(r, c, 'like', template);

% Populate the design matrix with the appropriate values for awake and sleepy subjects
design_matrix(1:34, 1) = 1; % Awake subjects
design_matrix(35:68, 2) = 1; % Sleepy subjects

save("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\nbs\design_matrix.mat", 'design_matrix')

