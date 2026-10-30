function group_matrix = build_3d_matrix(condition, location, dataset)

% condition = "sleepy"
% condition = "nsleepy"

% location determines file paths
% location = "home"
% location = "lab" 

% dataset determines file paths for pm vs inet vs etc.
% dataset = "inet"

%% 12.24.2025 PK. I need to turn the matrices into a 286 x 286 x n array.
%% Sleepy subjects
% Extract corrmat variables and put them in an array. 
if location == "home"
    main_root = fullfile("C:/Users", "tempu", "Arousal-Project");

elseif location == "lab"
    main_root = fullfile('/Volumes', 'illinois-las-psych-gratton', ...
        'member_directories', 'praisek2', 'Arousal-Project');
    

end

save_folder = fullfile(main_root, 'nbs', 'larger_sample_nbs'); 

%  Where is the data?
if condition == "sleepy"
    root = fullfile(main_root, 'adj_mat', ...
            'larger_sample', 'within_nsleepy_corrmats_Seitzman300');

elseif condition == "nsleepy"
    root = fullfile(main_root, 'adj_mat', ...
            'larger_sample', 'nsleepy_corrmats_Seitzman300');
end
        
    
    subjects = dir(fullfile(root, 'sub-*')); % only keep files starting with sub-
    subjects = subjects([subjects.isdir]); % only keep files that are folders

if dataset == "inet"

    file_end = '_task-rest_corrmat_Seitzman300.mat';
    file_name = [subjects(1).name file_end];
    first_path = fullfile(root, subjects(1).name, file_name);
    mat_file = load(first_path, 'corrmat');
    mat = mat_file.corrmat;


elseif dataset == "pm"
    disp("you didnt write any code for this dataset")
    return
    %file_end = '_task-resting_corrmat_Seitzman300.mat';
end

% Get the size of the 3D matrix.
[r, c] = size(mat);
n = numel(subjects);
    
% Initialize the matrix.
stack = zeros(r, c, n, 'like', mat);
    
    % Populate the matrix.
  
    for i = 1 : n
        if length(subjects(i).name) < 11
            file_name = [subjects(i).name file_end_pm];
    
        else
            file_name = [subjects(i).name file_end];
        end 
    
        corrmat_path = fullfile(root, subjects(i).name, file_name);
        mat_file = load(corrmat_path, 'corrmat');
        mat = mat_file.corrmat;
        if ~isequal(size(mat), [r,c])
            error('Size mismatch in %s', corrmat_path)
        end
        stack(:, :, i) = mat;
    end
    
    % save the variable to the nested folder
    save_file = fullfile(save_folder, 'within_3d_corrmat_sleepy.mat');
    save(save_file, 'stack')
    
    %% Awake subjects
    save_folder = fullfile("C:\Users", "tempu", "Downloads", "research", "labs", "gratton", "drive-download-20251224T235007Z-1-001", "corrmats_Seitzman300_awake");
    
    root_awake = fullfile("C:\Users", "tempu", "Downloads", "research", "labs", "gratton", "drive-download-20251224T235007Z-1-001", "corrmats_Seitzman300_awake"); % directory with all sleepy subjects
    subjects = dir(fullfile(root_awake, 'sub-*')); 
    subjects = subjects([subjects.isdir]);
    
    stack = zeros(r, c, n, 'like', mat);
    
    file_end_pm = '_task-resting_corrmat_Seitzman300.mat';
    file_end_inet = '_task-rest_corrmat_Seitzman300.mat';
    
    for i = 1 : n
        if length(subjects(i).name) < 11
            file_name = [subjects(i).name file_end_pm];
    
        else
            file_name = [subjects(i).name file_end];
        end 
    
        corrmat_path = fullfile(root, subjects(i).name, file_name);
        mat_file = load(corrmat_path, 'corrmat');
        mat = mat_file.corrmat;
    
        if ~isequal(size(mat), [r,c])
            error('Size mismatch in %s', corrmat_path)
        end
    
        stack(:, :, i) = mat;
    end
    
    % save the variable to the nested folder
    save_file = fullfile(save_folder, '3d_corrmat_awake.mat');
    save(save_file, 'stack')
    
    %% 1.7.2026 PK. Need to make a conn matrix with all subjects in one variable.
    
    % will append pm subj to the within_matrices array
    load("C:\Users\tempu\Arousal-Project\nbs\self_report_nbs\within\PM001_precision_matrices.mat");
    pm_A = PM01_matrices(:,:,2);
    pm_S = PM01_matrices(:,:,1);

    awake = load(fullfile(main_root, "\nbs\larger_sample_nbs\within_3d_corrmat_nsleepy.mat"));
    awake = awake.stack;
    awake = cat(3, awake, pm_A);

    sleepy = load(fullfile(main_root, "\nbs\larger_sample_nbs\within_3d_corrmat_sleepy.mat"));
    sleepy = sleepy.stack;
    sleepy = cat(3, sleepy, pm_S);
    
    matrices_all = cat(3, awake, sleepy);
    save_file = fullfile(save_folder, 'within_matrices.mat');
    save(save_file, 'matrices_all');
    
    %% 1.23.2026 PK. I need to split the 3d matrix into one for within subject analyses and one for between.
    
    load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\nbs\matrices_all.mat");
    
    % Based on final_sample.txt
    within_sub_idx = [1, 2, 3, 4, 12, 15, 19, 20, 24, 25, 28, 29, 31, 32, 35, 36, 37, 38, 46, 49, 53, 54, 58, 59, 62, 63, 65, 66];
    within_matrices = matrices_all_v2(:, :, within_sub_idx);
    
    between_sub_idx = 1:68;
    between_sub_idx(within_sub_idx) = [];
    between_matrices = matrices_all_v2(:, :, between_sub_idx);
    
    save_file = fullfile('C:', 'Users', 'tempu', 'Downloads', 'research', 'labs', 'gratton', 'Arousal-Project', 'nbs', 'within', 'within_matrices.mat');
    save(save_file, "within_matrices")
    save_file = fullfile('C:', 'Users', 'tempu', 'Downloads', 'research', 'labs', 'gratton', 'Arousal-Project', 'nbs', 'between', 'between_matrices.mat');
    save(save_file, "between_matrices")
    
    %% 1.28.2026 PK. I need to make the correct design matrices.
    
    r = size(awake, 3);
    
    design_matrix_within = zeros(r, 2);
    design_matrix_within(1:17, 1) = 1;
    design_matrix_within(18:34, 2) = 1;
    
    save_file = fullfile(main_root, 'nbs', 'larger_sample', 'within_design_matrix.mat');
    save(save_file, "design_matrix_within")
    
    design_matrix_between = zeros(146, 2);
    design_matrix_between(1:94, 1) = 1;
    design_matrix_between(95:146, 2) = 1;
    
    save_file = fullfile(main_root, 'nbs', 'larger_sample', 'full_design_matrix.mat');
    save(save_file, "design_matrix_between")
end