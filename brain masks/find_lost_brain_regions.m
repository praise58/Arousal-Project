% PK 11.20.2025
% Find the regions lost in my sample.

% Load the standard atlas params to compare the regions I lost.
atlas_params_standard = load("atlas_params_standard.mat");
atlas_params_standard = atlas_params_standard.atlas_params.sorti;

atlas_params_sample = load("atlas_params_v2.mat");
atlas_params_sample = atlas_params_sample.atlas_params.sorti;

% Compare the two atlas parameters
difference = setdiff(atlas_params_standard, atlas_params_sample, "stable");

% save it to my repo.
writematrix(difference, '/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/brain masks/regions_lost.xlsx')