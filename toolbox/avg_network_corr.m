function avg_mat = avg_network_corr(corr_mat, atlas_params)
% AVG_NETWORK_CORR  Produces a 286x286 correlation matrix where each
%   network-pair block is filled with the average correlation for that block.
%
%   Internally sorts the matrix (matching corr_mat_subplot) to compute
%   correct block boundaries, then UNSORTES the result before returning.
%   This means avg_mat is in the original ROI space and can be passed
%   directly to corr_mat_subplot with the original atlas_params unchanged.
%
% INPUTS:
%   corr_mat     - 286x286 correlation matrix (functional connectivity),
%                  in original (unsorted) ROI space
%   atlas_params - structure with atlas info, must contain:
%                    .sorti       - 1x286 sorting index
%                    .transitions - 1x13 vector of network END indices
%                                   in the SORTED space
%
% OUTPUT:
%   avg_mat - 286x286 matrix in ORIGINAL (unsorted) ROI space.
%             Pass directly to corr_mat_subplot with the same atlas_params.

    % ------------------------------------------------------------------ %
    % 1. Sort matrix the same way corr_mat_subplot does
    % ------------------------------------------------------------------ %
    sorti  = atlas_params.sorti;
    matrix = corr_mat(sorti, sorti);

    % ------------------------------------------------------------------ %
    % 2. Build network boundary index vectors (in sorted space)
    % ------------------------------------------------------------------ %
    transitions = atlas_params.transitions;
    n_networks  = numel(transitions);
    mat_size    = size(matrix, 1);          % 286

    end_idx   = transitions(:)';
    end_idx(end) = mat_size;               % clamp last boundary to 286

    start_idx = [1, end_idx(1:end-1) + 1]; % 1x13

    % ------------------------------------------------------------------ %
    % 3. Compute block averages in sorted space
    % ------------------------------------------------------------------ %
    avg_mat_sorted = zeros(mat_size, mat_size);

    for i = 1:n_networks
        row_idx = start_idx(i):end_idx(i)-1;

        for j = 1:n_networks
            col_idx = start_idx(j):end_idx(j)-1;

            block = matrix(row_idx, col_idx);

            if i == j
                % Diagonal block: exclude main diagonal
                block_size = numel(row_idx);
                off_diag   = ~eye(block_size, 'logical');
                block_mean = mean(block(off_diag), 'omitnan');

                filled_block = block_mean * ones(block_size);
                filled_block(eye(block_size, 'logical')) = NaN;
            else
                % Off-diagonal block: average all elements
                block_mean   = mean(block(:), 'omitnan');
                filled_block = block_mean * ones(numel(row_idx), numel(col_idx));
            end

            avg_mat_sorted(row_idx, col_idx) = filled_block;
        end
    end

    % ------------------------------------------------------------------ %
    % 4. UNSORT: map averaged matrix back to original ROI space
    %    sorti maps original -> sorted, so we need the inverse permutation
    %    to map sorted -> original.
    % ------------------------------------------------------------------ %
    [~, unsorti] = sort(sorti);
    avg_mat = avg_mat_sorted(unsorti, unsorti);

end