%% 12.24.2025 PK. I need to turn the matrices into a 286 x 286 x 35 array.

%% Sleepy subjects
% Extract corrmat variables and put them in an array. 

% Make a directory only for the files with data in it.
% Make the file path
save_folder = fullfile("C:\Users", "tempu", "Downloads", "research", "labs", "gratton", "drive-download-20251224T235007Z-1-001", "corrmats_Seitzman300_sleepy");

% Change to file path that contains the sleepy correlation matrices.
root_sleepy = fullfile("C:\Users", "tempu", "Downloads", "research", "labs", "gratton", "drive-download-20251224T235007Z-1-001", "corrmats_Seitzman300_sleepy"); % directory with all sleepy subjects
subjects = dir(fullfile(root_sleepy, 'sub-*')); % only keep files starting with sub-
subjects = subjects([subjects.isdir]); % only keep directories

% Get the size of the 3D matrix.
first_path = fullfile(root_sleepy, subjects(1).name, "sub-1_task-resting_corrmat_Seitzman300.mat");
mat_file = load(first_path, 'corrmat');
mat = mat_file.corrmat;
[r, c] = size(mat);
n = numel(subjects);

% Initialize the matrix.
stack = zeros(r, c, n, 'like', mat);

% Populate the matrix.
file_end_pm = '_task-resting_corrmat_Seitzman300.mat';
file_end_inet = '_task-rest_corrmat_Seitzman300.mat';

for i = 1 : n
    if length(subjects(i).name) < 11
        file_name = [subjects(i).name file_end_pm];

    else
        file_name = [subjects(i).name file_end_inet];
    end 

    corrmat_path = fullfile(root_sleepy, subjects(i).name, file_name);
    mat_file = load(corrmat_path, 'corrmat');
    mat = mat_file.corrmat;
    if ~isequal(size(mat), [r,c])
        error('Size mismatch in %s', corrmat_path)
    end
    stack(:, :, i) = mat;
end

% save the variable to the nested folder
save_file = fullfile(save_folder, '3d_corrmat_sleepy.mat');
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
        file_name = [subjects(i).name file_end_inet];
    end 

    corrmat_path = fullfile(root_awake, subjects(i).name, file_name);
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

%% 1.7.2026 PK. Need to make a conn matrix with all subject in one variable.

awake = load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\nbs\matrices_awake.mat");
awake = awake.stack;

sleepy = load("C:\Users\tempu\Downloads\research\labs\gratton\Arousal-Project\nbs\matrices_sleepy.mat");
sleepy = sleepy.stack;

matrices_all = cat(3, awake, sleepy);
save_file = fullfile('C:', 'Users', 'tempu', 'Downloads', 'research', 'labs', 'gratton', 'Arousal-Project', 'nbs', 'matrices_all.mat');
save(save_file, 'matrices_all');