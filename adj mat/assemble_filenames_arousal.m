function [subjects, filesBySubject, outdirsBySubject, fullpathsBySubject, inputdirsBySubject] = assemble_filenames_arousal(T, outRoot, varargin)
%ASSEMBLE_FILENAMES_AROUSAL Build file names and per-row input/output paths from a table.
%
% Inputs
%   T        : table with variables: subject, task, session, run, folder
%              - T.folder is the dataset-specific root directory for that row.
%              - session can be numeric or text; run can be numeric or a list as text.
%   outRoot  : (optional) root output directory (default: "FCout")
%
% Name-Value pairs (optional)
%   'InferFuncFolder' (logical, default true)
%       If true, tries common subfolders when composing input paths:
%       <folder>/sub-<SUB>/ses-<SES>/func, then <folder>/sub-<SUB>/ses-<SES>, then <folder>
%   'ReturnDirsOnly' (logical, default false)
%       If true, the 4th output (fullpathsBySubject) returns only the input directory
%       (ending with filesep) instead of the full path (dir + filename).
%
% Outputs
%   subjects            : string array of unique subject IDs (order preserved)
%   filesBySubject      : cell array; each cell is a cellstr of filenames for that subject
%   outdirsBySubject    : cell array; each cell is a cellstr of *output* dirs for each file
%                         (format: outRoot/sub-<SUBJECT>/ses-<SESSION>)
%   fullpathsBySubject  : cell array; by default, *input* full paths (dir + filename).
%                         If 'ReturnDirsOnly'==true, returns directory-only strings
%                         that end with filesep (e.g., '/func/').
%   inputdirsBySubject  : cell array; directory-only strings (always ends with filesep)
%
% Filename pattern:
%   sub-<SUBJECT>_ses-<SESSION>_task-<TASK>_run-<RUN:02d>_fmriprep_zmdt_resid_ntrpl_bpss_zmdt.nii.gz

% -------- Robust defaults --------
if nargin < 2 || isempty(outRoot)
    outRoot = "FCout";
end
outRoot = string(outRoot);

InferFuncFolder = true; % default
ReturnDirsOnly  = false; % default
if ~isempty(varargin)
    for k = 1:2:numel(varargin)
        key = lower(string(varargin{k}));
        if k+1 > numel(varargin)
            error('Name-value arguments must come in pairs.');
        end
        switch key
            case "inferfuncfolder"
                InferFuncFolder = logical(varargin{k+1});
            case "returndirsonly"
                ReturnDirsOnly = logical(varargin{k+1});
            otherwise
                error('Unknown parameter: %s', varargin{k});
        end
    end
end

% -------- Validate required columns --------
req = ["subject","task","session","run","folder"];
missing = setdiff(req, string(T.Properties.VariableNames));
if ~isempty(missing)
    error("Table is missing required columns: %s", strjoin(missing, ", "));
end

% -------- Normalize data types --------
subj  = string(T.subject);
task  = string(T.task);
sess  = double(str2double(string(T.session))); % allow numeric or text
runcol = T.run; % may be numeric, char, string, or cellstr
fold  = string(T.folder);

% -------- Unique subjects (preserve table order) --------
[subjects, ~, g] = unique(subj, 'stable');
filesBySubject     = cell(numel(subjects), 1);
outdirsBySubject   = cell(numel(subjects), 1);
fullpathsBySubject = cell(numel(subjects), 1);
inputdirsBySubject = cell(numel(subjects), 1);

% -------- Iterate per subject --------
for si = 1:numel(subjects)
    idx  = (g == si);
    rows = find(idx);

    files  = strings(0,1);
    outdir = strings(0,1);
    fulls  = strings(0,1); % by default: INPUT full paths (dir + filename)
    indir  = strings(0,1); % INPUT directories (ending with filesep)

    for r = rows(:)'
        % Parse runs for this row
        runs_val = runcol(r);
        if iscell(runs_val), runs_val = runs_val{1}; end
        if isnumeric(runs_val)
            runs = runs_val(:)'; % already numeric
        else
            runs_str = string(runs_val);
            runs_str = regexprep(runs_str, '[,;]+', ' '); % commas/semicolons -> space
            parts    = split(strtrim(runs_str)); % split on whitespace
            runs     = str2double(parts)';
            runs(isnan(runs)) = []; % drop non-numeric
        end

        % Dataset-specific root folder (from the table)
        infolder = fold(r);

        % Build for each run
        for rn = runs
            fname = sprintf( ...
                "sub-%s_ses-%d_task-%s_run-%02d_fmriprep_zmdt_resid_ntrpl_bpss_zmdt.nii.gz", ...
                subj(r), sess(r), task(r), rn);

            % Output directory (unchanged from original behavior)
            thisOutDir = string(fullfile(outRoot, "sub-"+subj(r), "ses-"+string(sess(r))));

            % --- Input directory candidates (most specific first) ---
            candDirs = [ ...
                string(fullfile(infolder, "sub-"+subj(r), "ses-"+string(sess(r)), "func"));
                string(fullfile(infolder, "sub-"+subj(r), "ses-"+string(sess(r))));
                string(infolder)
            ];

            thisInDir = candDirs(1);
            if InferFuncFolder
                % Choose the first candidate that contains the file (if any)
                chosen = "";
                for c = 1:numel(candDirs)
                    if exist(fullfile(candDirs(c), fname), 'file') == 2
                        chosen = candDirs(c);
                        break;
                    end
                end
                if chosen ~= ""
                    thisInDir = chosen;
                end
            end

            % Ensure directory string ends with filesep
            if ~endsWith(thisInDir, filesep)
                thisInDir = thisInDir + filesep;
            end

            files(end+1,1)  = string(fname); %#ok<AGROW>
            outdir(end+1,1) = thisOutDir;     %#ok<AGROW>
            fulls(end+1,1)  = string(fullfile(thisInDir, fname)); %#ok<AGROW>
            indir(end+1,1)  = thisInDir;      %#ok<AGROW>
        end
    end

    % If requested, return directories only in the 4th output
    if ReturnDirsOnly
        fulls = indir; % directory-only (with trailing filesep)
    end

    filesBySubject{si}     = cellstr(files);
    outdirsBySubject{si}   = cellstr(outdir);
    fullpathsBySubject{si} = cellstr(fulls);
    inputdirsBySubject{si} = cellstr(indir);
end
end
