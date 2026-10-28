%% 1.9.2026 PK. Remove the 1 in all matrices.
% Copilot states that including the identity distorts the NBS.

main_root = "C:\Users\tempu\Arousal-Project";
matrices_all = load(fullfile(main_root, "nbs", "larger_sample", "within_matrices.mat"));
matrices_all = matrices_all.matrices_all;

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
end

save_file = fullfile(main_root, 'nbs', 'larger_sample', 'within_matrices_id-rm.mat');
save(save_file, 'matrices_all_v2');

%% Proceed to build_3d_matrix.m to split matrices_all_v2 into between and within