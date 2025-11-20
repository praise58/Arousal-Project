function make_AdjMat_volume(file)
% function fcimage_corrmat_volume()
% this function will make an ROI x ROI correlation matrix based on a set of
% volume ROIs and a list of files
%
% EXAMPLE: fcimage_corrmat_volume('1')
%
% CG - 03.26.2020
% AT - 30.06.2025: modify to use one input only
%%%%%%%%%%%%%%%%%

main_path = '/projects/illinois/las/psych/cgratton/networks-pm/arousal'; % Set the folder to put everything in.
file = sprintf('%s/AT_example.xlsx',main_path); % files: all sleepy or awake file runs 

data = readtable(file);
atlas_dir = '/projects/illinois/las/psych/cgratton/networks-pm/Atlases'; %Where are the atlases?
atlas = 'Seitzman300'; %What atlas will be used? WARNING: only Seitzman300-res1 available for 1mm data
FDtype = 'fFD';
minutes_included = 5; %Change this according to the amount of data you'd like to include

% Add paths to toolbox we need
addpath(genpath('/projects/illinois/las/psych/cgratton/networks-pm/software/GrattonLab-General-Repo'));
addpath(genpath('/projects/illinois/las/psych/cgratton/networks-pm/software/nifti'));
addpath(genpath('/projects/illinois/las/psych/cgratton/networks-pm/software/hline_vline'));
disp('toolboxes loaded')

%% ROI info
atlas_params = atlas_parameters_GrattonLab(atlas,atlas_dir);
roi_data = load_nii_wrapper(atlas_params.MNI_nii_file); %vox by 1

%% Loop through data, extract timecourses. Concatenate by chosen runs
%you need a list of the individual, unique subjects
fcproc = data.folder{1}; %assuming the preprocess data is all in the same folder 
[subjects, filesBySubject, ~, fullpathsBySubject] = assemble_filenames_arousal(data, fcproc, 'ReturnDirsOnly', true);

for i = 1:length(fullpathsBySubject)
    subject = subjects{i};

    outDir = [main_path '/corrmats_' atlas '/sub-' subject];
    if ~exist(outDir) 
        mkdir(outDir);
    end

    files = filesBySubject{i};
    paths = fullpathsBySubject{i};
    fmripaths = {};

    if contains(files{1,1}, "run-NaN_") %if it is the PM dataset
        for j=1:length(files)
            files(j) = erase(files(j), "run-NaN_");
        end
        fmripaths = cellstr( regexprep(string(paths), 'FCPreproc-24\.1\.1', 'fmriprep-24.1.1') );
        TR=0.594;
    else 
        files = cellstr( regexprep(string(files), 'resting', 'rest') );
        fmripaths = cellstr( regexprep(string(paths), 'preproc_FCProc-24\.1\.1', 'preproc_fmriprep-24.1.1') );
        TR=1.1;
    end

    roi_timeseries_concat = [];
    tmask_concat = [];

    for k = 1:numel(files) %read each file with the same (sess,task) and concatenate
        file_data = load_nii_wrapper(sprintf('%s/%s',paths{k}, files{k})); %vox by timepoints
        file_roi_timeseries{k} = roi_average_timecourse(file_data,roi_data);
        roi_timeseries_concat = [roi_timeseries_concat file_roi_timeseries{k}];
    
        % tmask file:
        file_info = extractBefore(files(k),'_fmriprep');
        tmaskFile = sprintf('%s/FD_outputs/%s_desc-tmask_%s.txt',fmripaths{k},file_info{1},FDtype);
        tmask{k} = table2array(readtable(tmaskFile));
        tmask_concat = [tmask_concat; tmask{k}];
    end

        %restric the data to the time limit
        num_vols = ceil((60*minutes_included)/TR);
        tmask_concat(find(cumsum(tmask_concat) >= num_vols, 1, 'first')+1:end) = 0;

        % apply tmask to timeseries and calculate correlations
        corrmat = paircorr_mod(roi_timeseries_concat(:,logical(tmask_concat))');

        file_info = regexprep(files(k), '_run-\d+.*$', ''); %no run-X in this string
        file_info = regexprep(file_info{1}, '_fmriprep_zmdt_resid_ntrpl_bpss_zmdt.nii.gz', ''); %no run-X in this string
        file_info = regexprep(file_info, '_ses-[0-9]+', ''); %no session info here
        fout_str = sprintf('%s/%s_corrmat_%s',outDir,file_info,atlas);
    
        figure_corrmat_GrattonLab(corrmat,atlas_params,-1,1);
        saveas(gcf,[fout_str '.tiff'],'tiff');
        close(gcf);

        % save out files
        save([fout_str '.mat'],'file_roi_timeseries','roi_timeseries_concat','tmask','tmask_concat','corrmat');
    end  
end


function roi_ts_avg = roi_average_timecourse(bold_data,roi_data)

nrois = unique(roi_data);
nrois = nrois(nrois>0); % assume 0 is not an ROI

for nr = 1:length(nrois)
    roi_vox = bold_data(roi_data==nrois(nr),:);
    roi_ts_avg(nr,:) = nanmean(roi_vox,1);
    num_nans = sum(isnan(roi_vox(:)));
    if num_nans>0
        warning(sprintf('ROI %03d contains nans',nrois));
    end
end

end

function [r p] = paircorr_mod(a,b)
%PAIRCORR Computes pairwise Pearson's linear correlation coefficient with
% optional significance. Returns r, a p1-by-p2 matrix containing the
% pairwise correlation coefficient between each pair of columns in the
% n-by-p1 and n-by-p2 matrices a and b. r is calculated as the dot
% product between two vectors divided by the product of their magnitudes.
% If a second output argument is provided, like so:
% [r p] = paircorr(a,b)
% then p is the two-tailed significance.
% TOL 03/01/11.
% Added single input functionality TOL, 04/01/12.

if nargin<2
    b = a;
end

a = bsxfun(@minus, a, mean(a));
b = bsxfun(@minus, b, mean(b));

mag_a = sqrt(sum(a.^2, 1));
mag_b = sqrt(sum(b.^2, 1));

r = (a' * b) ./ (mag_a' * mag_b);

if nargout > 1
    [n p1] = size(a);
    
    % calculate t-statistic
    t = r ./ sqrt((1 - r.^2)/(n - 2));
    % calculate significance, two-tailed
    p = 2 * tcdf(-abs(t), n - 2);
end
end

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
