mask_var = load("/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/Arousal-Project/brain masks/mask_paths.mat");

mask_var = mask_var.filesStructv8;

folder = "/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/Arousal-Project/brain masks";

writetable(mask_var, fullfile(folder, 'mask_paths.csv'));