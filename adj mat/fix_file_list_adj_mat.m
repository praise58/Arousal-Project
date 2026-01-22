%% 12.15.2025 Fixing the file lists

example_file_struct = readtable("/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/Arousal-Project/adj mat/AT_example.xlsx");

% Load the tables
awake_FC_PreProc_FIX = readtable("/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/Arousal-Project/adj mat/awake_FCPreProc.xlsx");
sleepy_FC_PreProc_FIX = readtable("/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/Arousal-Project/adj mat/sleepy_FCPreProc.xlsx");

%% Fixing the column order
% Check the column order
example_file_struct.Properties.VariableNames
awake_FC_PreProc_FIX.Properties.VariableNames
sleepy_FC_PreProc_FIX.Properties.VariableNames

% Make the column order
corr_order = ["folder", "subject", "task", "session", "run"];

% Fix the column order
awake_FC_PreProc_FIX = awake_FC_PreProc_FIX(:, corr_order);
sleepy_FC_PreProc_FIX = sleepy_FC_PreProc_FIX(:, corr_order);

% Check the tables for column order
head(awake_FC_PreProc_FIX, 10)
head(sleepy_FC_PreProc_FIX, 10)

%% Fixing the rows
% This is where the sleepyFCProc starts
idx_rm = 524;

% Remove the extra rows
awake_FC_PreProc = awake_FC_PreProc_FIX(1 : idx_rm - 1, :);
sleepy_FC_PreProc = sleepy_FC_PreProc_FIX(idx_rm : end, :);

% Check if the extra runs are gone
tail(awake_FC_PreProc, 10)
tail(sleepy_FC_PreProc, 10)

% It looks like the sleepy FC PreProc is wrong in the first place.
