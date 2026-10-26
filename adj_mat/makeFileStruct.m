function makeFileStruct(input_path, output_path)

% output_name is a string -- name of the .xlsx output table -- starts from
% Arousal-Project
% input_path used to find the input table and store the output table -- starts from
% Arousal-Project
main_path = '/Volumes/illinois-las-psych-gratton/member_directories/praisek2/Arousal-Project/';
table_path = fullfile(main_path, input_path);
df = readtable(table_path);

n = height(df);

folder = repmat('/Volumes/illinois-las-psych-gratton/iNetworks/Nifti/derivatives/preproc_FCProc-24.1.1', n, 1);
subject = erase(string(df.("sub")), "sub-");
task = erase(string(df.("task")), "task-");
session = erase(string(df.("ses")), "ses-");
run = erase(string(df.("run")), "run-");

df = table(folder, subject, task, session, run, ...
    'VariableNames', {'folder','subject','task','session','run'});


writetable(df, fullfile(main_path, output_path))

end