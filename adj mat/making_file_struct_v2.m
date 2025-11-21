% Making file_struct_v2

sample = readtable("/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/brain masks/all_paths_v2.csv");
% turn into struct
filesStructv8 = table2struct(sample);
%rm rows column.
filesStructv8 = rmfield(filesStructv8, "Var1");

% Remove unavailable runs
files_to_remove = 686;
filesStructv8(files_to_remove) = [];

% Make a struct with FCProc file names.
pm_rows = filesStructv8(1:22);
inet_rows = filesStructv8(23:end);

% Define what you're replacing and with what.
old_seg_path = 'fmriprep-24.1.1';
new_seg_path = 'FCPreproc-24.1.1';

old_seg_name = 'space-MNI152NLin6Asym_res-2_desc-brain_mask';
new_seg_name = 'fmriprep_zmdt_resid_ntrpl_bpss_zmdt';

% Iterate through each row
for i = 1:length(pm_rows)
    current_path = pm_rows(i).path;
    current_name = pm_rows(i).name;

    new_path = strrep(current_path, old_seg_path, new_seg_path);
    new_name = strrep(current_name, old_seg_name, new_seg_name);

    % Store the updated paths and names back into the struct
    pm_rows(i).path = new_path;
    pm_rows(i).name = new_name;
end

% Now for INET.
for i = 1:length(inet_rows)
    current_path = inet_rows(i).path;
    current_name = inet_rows(i).name;

    new_path = strrep(current_path, old_seg_path, new_seg_path);
    new_name = strrep(current_name, old_seg_name, new_seg_name);

    % Store the updated paths and names back into the struct
    inet_rows(i).path = new_path;
    inet_rows(i).name = new_name;
end

file_struct_FCPreProc = [pm_rows; inet_rows];

% create_group_mask_v2 uses a xlsx file.
tableFCPreProc = struct2table(file_struct_FCPreProc);

% Write the updated table to an Excel file
writetable(tableFCPreProc, '/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/brain masks/FCPreProc_files.xlsx');



% Revise the table to be in the format that the function requires.

%----Add new variable names.----
subject = NaN(height(tableFCPreProc), 1);
task = repmat("resting", height(tableFCPreProc), 1);
session = NaN(height(tableFCPreProc), 1);
run = NaN(height(tableFCPreProc), 1);

tableFCPreProc = addvars(tableFCPreProc, subject, task, session, run, 'NewVariableNames', {'subject', 'task', 'session', 'run'});

%-------PM runs first.-------

%-------Extract subject, session, and run for PM runs.--------
for i = 1:22
    parts = split(tableFCPreProc.name(i), '_');

    curr_subject = sscanf(parts{1}, 'sub-%f');
    curr_session = sscanf(parts{2}, 'ses-%f');

    tableFCPreProc.subject(i) = curr_subject;
    tableFCPreProc.session(i) = curr_session;
end

%-------Then for INET runs.--------
for i = 23:height(tableFCPreProc)
    parts = split(tableFCPreProc.name(i), '_');

    curr_subject = sscanf(parts{1}, 'sub-INET%f');
    curr_subject = sprintf('%03d', curr_subject);
    curr_subject = string(curr_subject);
    curr_session = sscanf(parts{2}, 'ses-%f');
    curr_run = sscanf(parts{4}, 'run-%f');

    curr_subject = strcat("INET", curr_subject);

    tableFCPreProc.subject = string(tableFCPreProc.subject);
    tableFCPreProc.subject(i) = curr_subject;
    tableFCPreProc.session(i) = curr_session;
    tableFCPreProc.run(i) = curr_run;
end

%----Remove the names variable!----
tableFCPreProc = removevars(tableFCPreProc, "name");

% Save the table.
writetable(tableFCPreProc, '/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/AdjMat/tableFCPreProc_names.xlsx')

% Trying a different method to make a table.
path = [];
subject = [];
task = [];
session = [];
run = [];

% Make a new table for sleepy runs.
sleepy_FCPreProc = table(path, subject, task, session, run);


% Now we need to divide it up by sleepy and awake runs...
sleepy_sample_inet = readtable('/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/counting subs/inet_sleepy_runs_v5.csv');
sleepy_sample_pm = readtable('/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/counting subs/pm_sessions_sleepy.csv');

sleepy_sample_inet = removevars(sleepy_sample_inet, "Var1");
sleepy_sample_pm = removevars(sleepy_sample_pm, "Var1");

sleepy_sample_pm.subject = string(sleepy_sample_pm.subject);
sleepy_sample_pm.session = double(sleepy_sample_pm.session);


%----PM sleepy runs.----
cols_pm = {"subject", "session"};
cols_pm = cellstr(cols_pm); % Made cols as cell array but should be string array, which is indexed.

check_sleepy_runs = ismember(tableFCPreProc(1:22, cols_pm), sleepy_sample_pm(1:15, cols_pm), 'rows');

idx_sleepy_runs_pm = find(check_sleepy_runs);

matched_sleepy_runs_pm = innerjoin(tableFCPreProc(1:22, cols_pm), sleepy_sample_pm(1:15, cols_pm));

% 11.21.2025, fixed--it was the wrong path
folder = []; % make a new column for folder
folder = string(folder);
matched_sleepy_runs_pm = convertvars(matched_sleepy_runs_pm, {'session'}, "string"); % need to match type

for i=1 : height(matched_sleepy_runs_pm)
    sub_str = matched_sleepy_runs_pm.subject(i);
    sess_str = matched_sleepy_runs_pm.session(i);

    folder_str = "/Volumes/illinois-las-psych-gratton/networks-pm/replica/pm/derivatives/FCProc-24.1.1/sub-" + sub_str + "/ses-" + sess_str + "/func/";
    
    folder(end + 1, 1) = folder_str;
end

matched_sleepy_runs_pm.folder = folder;

matched_sleepy_runs_pm = addvars(matched_sleepy_runs_pm, repmat('resting', height(matched_sleepy_runs_pm), 1), 'NewVariableNames', 'task');
matched_sleepy_runs_pm = addvars(matched_sleepy_runs_pm, repmat('NaN', height(matched_sleepy_runs_pm), 1), 'NewVariableNames', 'run');

matched_sleepy_runs_pm.run = str2double(string(matched_sleepy_runs_pm.run));

%----INET sleepy runs.----

%-------which rows will you keep?-------

% These are the columns I need to decide which rows to keep or to remove.
cols = {"subject", "session", "run"};
cols = cellstr(cols);

check_sleepy_runs = ismember(tableFCPreProc(:, cols), sleepy_sample_inet(:, cols), 'rows');

idx_sleepy_runs_inet = find(check_sleepy_runs);

matched_sleepy_runs_inet = innerjoin(tableFCPreProc(:, cols), sleepy_sample_inet(:, cols));

% 11.21.2025, fixed--it was the wrong path
folder = []; % make a new column for folder
folder = string(folder);
matched_sleepy_runs_inet = convertvars(matched_sleepy_runs_inet, {'session', 'run'}, "string"); % need to match type

for i=1 : height(matched_sleepy_runs_inet)
    sub_str = matched_sleepy_runs_inet.subject(i);
    sess_str = matched_sleepy_runs_inet.session(i);

    folder_str = "/Volumes/illinois-las-psych-gratton/iNetworks/Nifti/derivatives/preproc_FCProc-24.1.1/sub-" + sub_str + "/ses-" + sess_str + "/func/";
    folder(end + 1, 1) = folder_str;
end

matched_sleepy_runs_inet.folder = folder;

matched_sleepy_runs_inet = addvars(matched_sleepy_runs_inet, repmat('resting', height(matched_sleepy_runs_inet), 1), 'NewVariableNames', 'task');

% vertical concatenation
sleepy_FCPreProc = [matched_sleepy_runs_pm; matched_sleepy_runs_inet];

% Save the FCPreProcs for the sleepy subjects!!!
writetable(sleepy_FCPreProc, '/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/AdjMat/sleepy_FCPreProc.xlsx')


% Now for awake runs....
awake_FCPreProc = table(path, subject, task, session, run);

awake_sample_inet = readtable('/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/counting subs/inet_awake_runs_v5.csv');
awake_sample_pm = readtable('/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/counting subs/pm_sessions_awake.csv');

awake_sample_inet = removevars(awake_sample_inet, "Var1");
awake_sample_pm = removevars(awake_sample_pm, "Var1");

awake_sample_pm.subject = string(awake_sample_pm.subject);
awake_sample_pm.session = double(awake_sample_pm.session);

%----PM awake runs.----
check_awake_runs = ismember(tableFCPreProc(1:22, cols_pm), awake_sample_pm(1:9, cols_pm), 'rows');

idx_awake_runs_pm = find(check_awake_runs);

matched_awake_runs_pm = innerjoin(tableFCPreProc(1:22, cols_pm), awake_sample_pm(1:9, cols_pm));

% 11.21.2025, fixed--it was the wrong path
folder = []; % make a new column for folder
folder = string(folder);

matched_awake_runs_pm = convertvars(matched_awake_runs_pm, {'session'}, "string"); % need to match type

for i=1 : height(matched_awake_runs_pm)
    sub_str = matched_awake_runs_pm.subject(i);
    sess_str = matched_awake_runs_pm.session(i);

    folder_str = "/Volumes/illinois-las-psych-gratton/networks-pm/replica/pm/derivatives/FCProc-24.1.1/sub-" + sub_str + "/ses-" + sess_str + "/func/";
    
    folder(end + 1, 1) = folder_str;
end

matched_awake_runs_pm.folder = folder;

matched_awake_runs_pm = addvars(matched_awake_runs_pm, repmat('resting', height(matched_awake_runs_pm), 1), 'NewVariableNames', 'task');
matched_awake_runs_pm = addvars(matched_awake_runs_pm, repmat('NaN', height(matched_awake_runs_pm), 1), 'NewVariableNames', 'run');

matched_awake_runs_pm.run = str2double(string(matched_awake_runs_pm.run));


%----INET awake runs.----
check_awake_runs = ismember(tableFCPreProc(:, cols), awake_sample_inet(:, cols), 'rows');

idx_awake_runs_inet = find(check_awake_runs);

matched_awake_runs_inet = innerjoin(tableFCPreProc(:, cols), awake_sample_inet(:, cols));


% 11.21.2025, fixed--it was the wrong path
folder = []; % make a new column for folder
folder = string(folder);

matched_awake_runs_inet = convertvars(matched_awake_runs_inet, {'session'}, "string"); % need to match type

for i=1 : height(matched_awake_runs_inet)
    sub_str = matched_awake_runs_inet.subject(i);
    sess_str = matched_awake_runs_inet.session(i);
    run_str = matched_awake_runs_inet.run(i);

    folder_str = "/Volumes/illinois-las-psych-gratton/iNetworks/Nifti/derivatives/preproc_FCProc-24.1.1/sub-" + sub_str + "/ses-" + sess_str + "/func/";
    
    folder(end + 1, 1) = folder_str;
end

matched_awake_runs_inet.folder = folder;

matched_awake_runs_inet = addvars(matched_awake_runs_inet, repmat('resting', height(matched_awake_runs_inet), 1), 'NewVariableNames', 'task');

% vertical concatenation
awake_FCPreProc = [matched_awake_runs_pm; matched_awake_runs_inet];

% Save the FCPreProcs for the sleepy subjects!!!
writetable(awake_FCPreProc, '/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/AdjMat/sleepy_FCPreProc.xlsx')

% SAVE IT!!!!! YAAAAAY.

writetable(awake_FCPreProc, '/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/AdjMat/awake_FCPreProc.xlsx')


% It's not a perfect split.
folder = [];
subject = [];
task = [];
session = [];
run = [];
repeated_runs = table(folder, subject, task, session, run);

% I'm checking which rows are duplicated.
for i = 1:height(awake_FCPreProc)
    curr_sub = awake_FCPreProc.subject(i);
    curr_sess = awake_FCPreProc.session(i);
    curr_run = awake_FCPreProc.run(i);
    curr_run = string(curr_run);

    if ismember(curr_sub, sleepy_FCPreProc.subject)
        if ismember(curr_sess, sleepy_FCPreProc.session)
            if ismember(curr_run, sleepy_FCPreProc.run)
                % Store row...
                row = awake_FCPreProc(i, :);
                % ...into repeated_runs
                repeated_runs = [repeated_runs; row];
            end
        end
    end
end

% all of the runs in repeated_runs are in BOTH the FCPreProc awake and
% sleepy folders. This should NOT be the case.

% 11/19/2025, I fixed it. The logic of the ismember() was wrong.

num_runs = height(matched_awake_runs_inet) + height(matched_awake_runs_pm) + height(matched_sleepy_runs_inet) + height(matched_sleepy_runs_pm);
if num_runs ~= height(tableFCPreProc)
    error('Mismatch in the number of runs');
end

%11/20/2025, the paths are wrong.

