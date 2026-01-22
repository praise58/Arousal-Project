%% 1.9.2026 PK. Remove the 1 in all matrices.
% Copilot states that including the identity distorts the NBS.

load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\nbs\matrices_all.mat");

[r, c, n] = size(matrices_all);
matrices_all_v2 = zeros(r, c, n, 'like', matrices_all);

for i = 1 : n
    mat = matrices_all(:, :, i);

    % Normalize to account for global activation differences.
    mat = atanh(mat);                           % Fisher z
    mat(1:size(mat,1)+1:end) = 0;              % zero diagonal

    matrices_all_v2(:, :, i) = mat;
end

save_file = fullfile('C:', 'Users', 'tempu', 'Downloads', 'research', 'labs', 'gratton', 'Arousal-Project', 'nbs', 'matrices_all_v2.mat');
save(save_file, 'matrices_all_v2');