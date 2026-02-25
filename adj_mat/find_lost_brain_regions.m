%% PK 11.20.2025
% Find the regions lost in my sample.

% Load the standard atlas params to compare the regions I lost.
atlas_params_standard = load("atlas_params_standard.mat");
atlas_params_standard = atlas_params_standard.atlas_params;
atlas_params_standard_roi = atlas_params_standard.sorti;

atlas_params_sample = load("atlas_params_v2.mat");
atlas_params_sample = atlas_params_sample.atlas_params;
atlas_params_sample_roi = atlas_params_sample.sorti;

% Compare the two atlas parameters
difference = setdiff(atlas_params_standard_roi, atlas_params_sample_roi, "stable");

% save it to my repo.
% GRatton:
% writematrix(difference, '/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/brain masks/regions_lost.xlsx')
% Laptop:
writematrix(difference, 'C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\adj mat')

%% PK 11.21.2025
% I need to make a brain mask that represents the ROIs that my sample has.
% Paths:
NIFTI_toolbox = '/MATLAB Add-Ons/Collections/Tools for NIfTI and ANALYZE image';
group_mask = '/MATLAB Drive/group_mask_v3.mat';
seitzman_atlas_path = '/MATLAB Drive/Seitzman300_MNI_res02_allROIs.nii';

% Need the nifti toolbox to make a .nii brain mask.
% Gratton: addpath(genpath('/Users/grattonlab/Desktop/Praise_Learning/NIfTI_20140122'));
% Laptop:
addpath(genpath(NIFTI_toolbox));

% I want to reshape my brain mask vector into an array with 3 columns.
% Gratton: vector_mask = load('/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/adj mat/group_mask_v3.mat');
% laptop:
vector_mask = load(group_mask);
vector_mask = vector_mask.mask;

% represent the mask as 3 dimensions
three_dim_mask = reshape(vector_mask, [91, 109, 91]);

% Gratton: seitzman_atlas = load_nii('/Volumes/illinois-las-psych-gratton/networks-pm/Atlases/Seitzman300/Seitzman300_MNI_res02_allROIs.nii');
% Laptop: 
seitzman_atlas = load_nii(seitzman_atlas_path);

% save the header from the template
nii = seitzman_atlas;
nii.img = three_dim_mask; % use the sample brain mask
save_nii(nii, 'sample_group_mask_2mm.nii');

% I have the sample brain mask, but I need to turn it into an atlas format
% like the Seitzman.

% Ensure logical binary mask
three_dim_mask = three_dim_mask > 0;

% Unique labels in atlas excluding background 0
seitzman_img = seitzman_atlas.img;
labels = unique(seitzman_img);
labels(labels==0) = []; % There is an extra row.

kept_labels = [];
removed_labels = [];
kept_counts = [];
removed_counts = [];

out_atlas = zeros(size(seitzman_img), 'like', seitzman_img); % 'like' matches data type

for i = 1:numel(labels)
    L = labels(i); % 
    roi_vox = (seitzman_img == L);  % the .img of the atlas has values 1-300.
    roi_count = nnz(roi_vox); % size of the roi
    overlap_count = nnz(roi_vox & three_dim_mask); % finds the voxels for the roi in my mask.

    if overlap_count == roi_count
        % keep ROI with original label
        out_atlas(roi_vox) = L; % In the result atlas where the voxel value is == 1 for that roi, assign the ROI index L.
        kept_labels(end+1, 1) = L; % Add the L value to the list of kept labels.
        kept_counts(end+1, 1) = roi_count; % keeps track of the ROI sizes in an array list.
    else
        removed_labels(end+1,1) = L;
        removed_counts(end+1,1) = roi_count;
    end
end

% Save output NIfTI using same header but set datatype to match out_atlas
info_fout = seitzman_atlas;
out_path = "C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\sample_atlas.nii";
niftiwrite(out_atlas, out_path, info_fout, 'Compressed', false);

% Save a small CSV report
T_kept = table(kept_labels, kept_counts, 'VariableNames', {'Label','VoxelCount'});
T_removed = table(removed_labels, removed_counts, 'VariableNames', {'Label','VoxelCount'});
writetable(T_kept, ['kept_' report_csv]);
writetable(T_removed, ['removed_' report_csv]);

% Summary printed to console
fprintf('Total ROIs in Seitzman atlas: %d\n', numel(labels));
fprintf('Kept ROIs: %d. Report saved to kept_%s\n', size(T_kept,1), report_csv);
fprintf('Removed ROIs: %d. Report saved to removed_%s\n', size(T_removed,1), report_csv);

% Now I need to subtact this nii file from the Seitzman atlas to extract
% the ROIs. To do that, I need to load the seitzman atlas.
seitzman_atlas = seitzman_atlas.img;
sample_mask = nii.img;
sample_mask(seitzman_atlas == 0) = 0;
