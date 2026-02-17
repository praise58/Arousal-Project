%% 1.9.2026 PK. Remove the 1 in all matrices.
% Copilot states that including the identity distorts the NBS.

load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\nbs\matrices_all.mat");

[r, c, n] = size(matrices_all);
matrices_all_v2 = zeros(r, c, n, 'like', matrices_all);

% Choose a clamp based on precision
if isa(matrices_all, 'single')
    tol = single(1e-7);
else
    tol = 1e-12;  % for double
end

for i = 1:n
    mat = matrices_all(:, :, i);

    % Zero diagonal (correlations of a variable with itself)
    mat(1:size(mat,1)+1:end) = 0;

    % Clamp to open interval (-1, 1) to avoid Inf in atanh
    mat = max(min(mat, 1 - tol), -1 + tol);

    % Fisher z-transform
    mat = atanh(mat);

    matrices_all_v2(:, :, i) = mat;

    matrices_all_v2(:, :, i) = tanh(matrices_all_v2(:, :, i));

end

save_file = fullfile('C:', 'Users', 'tempu', 'Downloads', 'research', 'labs', 'gratton', 'Arousal-Project', 'nbs', 'matrices_all_v2.mat');
save(save_file, 'matrices_all_v2');

test = matrices_all_v2(:, :, i);
figure; heatmap(test, 'Colormap', jet)