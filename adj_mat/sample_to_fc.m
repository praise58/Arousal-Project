df_sleepy_within = readtable('/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/splitting_sample/larger_sample/within_sleepy_df.csv');
df_nsleepy_within = readtable('/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/splitting_sample/larger_sample/within_nsleepy_df.csv');

% make_sleepy_FCpreproc.m
% Converts df_sleepy.csv into sleepy_FCpreproc.csv
% (columns: folder, subject, task, session, run)

outFolder = '/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/adj_mat/larger_sample/';
inFile  = '/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/splitting_sample/larger_sample/within_nsleepy_df.csv';
outFile = 'sleepy_FCpreproc.csv';
folder  = "/Volumes/illinois-las-psych-gratton/iNetworks/Nifti/derivatives/preproc_FCProc-24.1.1";

% Detect whether the file is tab- or comma-delimited
fid = fopen(inFile, 'r');
firstLine = fgetl(fid);
fclose(fid);
if contains(firstLine, sprintf('\t'))
    delim = '\t';
else
    delim = ',';
end

% Read only the columns we need, as text
opts = detectImportOptions(inFile, 'Delimiter', delim);
opts.VariableNamingRule = 'preserve';
opts.SelectedVariableNames = {'sub', 'ses', 'run'};
opts = setvartype(opts, {'sub', 'ses', 'run'}, 'string');
df = readtable(inFile, opts);

n = height(df);

% "sub-INET016" -> "INET016" (INET + 3-digit number)
digits  = regexp(df.sub, '(\d{3})', 'tokens', 'once');
subject = "INET" + string(cellfun(@(c) c{1}, digits, 'UniformOutput', false));

% "ses-1" -> 1, "run-05" -> 5
session = str2double(extractAfter(df.ses, "ses-"));
run     = str2double(extractAfter(df.run, "run-"));

% Build the new table in the required order and with the required names
out = table(repmat(folder, n, 1), subject, repmat("rest", n, 1), session, run, ...
    'VariableNames', {'folder', 'subject', 'task', 'session', 'run'});

writetable(out, outFile);
disp(head(out));