function fcimage_corrmat_volume(sub)
% function fcimage_corrmat_volume()
% this function will make an ROI x ROI correlation matrix based on a set of
% volume ROIs and a list of files
%
% EXAMPLE: fcimage_corrmat_volume('1')
%
% CG - 03.26.2020
% AT - 30.06.2025: modify to use one input only
%%%%%%%%%%%%%%%%%

if nargin==1
    cfg = get_config(sub);
    
    FCdir = cfg.FCdir;
    atlas_dir = cfg.atlas_dir;
    atlas = cfg.atlas;
    FDtype = cfg.FDtype;
else
    error('check config file')x
end

% Add paths to toolbox we need
addpath(genpath(cfg.preproc_path));
addpath(genpath(cfg.niftiread_toolbox));
addpath(genpath(cfg.hline_vline));
disp('toolboxes loaded')

%% ROI info
atlas_params = atlas_parameters_GrattonLab(atlas,atlas_dir);
roi_data = load_nii_wrapper(atlas_params.MNI_nii_file); %vox by 1

%% track denoised files and organize per session 
files = dir(fullfile([FCdir, '/sub-', sub], '**', '*_fmriprep_zmdt_resid_ntrpl_bpss_zmdt.nii.gz'));

session_nums = regexp({files.name}, 'ses-(\d+)', 'tokens', 'once');
session_nums = [session_nums{:}];
unique_sessions = unique(session_nums);

%% Loop through data, extract timecourses. Concatenate by task per session to match the old code
outDir = [FCdir '/corrmats_' atlas '/sub-' sub];
if ~exist(outDir) 
    mkdir(outDir);
end

for i = 1:numel(unique_sessions) % for each session
    this_session = unique_sessions{i};

    fprintf('Subject %s, session %s\n',sub,this_session);

    idx_session = strcmp(session_nums, this_session);
    files_this_session = files(idx_session);

    task_names_this_session = regexp({files_this_session.name}, 'task-([a-zA-Z0-9]+)_', 'tokens', 'once');
    task_names_this_session = [task_names_this_session{:}]; 
    unique_tasks = unique(task_names_this_session);
    
    for j = 1:numel(unique_tasks) %for each different task identified in each session
        this_task = unique_tasks{j};
        idx_task = strcmp(task_names_this_session, this_task);
        files_this_task = files_this_session(idx_task);

        fprintf('Computing for: session %s task %s %s \n', num2str(i), num2str(j), this_task)
    
        roi_timeseries_concat = [];
        tmask_concat = [];

        for k = 1:numel(files_this_task) %read each file with the same (sess,task) and concatenate
            file_data = load_nii_wrapper(sprintf('%s/%s', files_this_task(k).folder,files_this_task(k).name)); %vox by timepoints
            file_roi_timeseries{k} = roi_average_timecourse(file_data,roi_data);
            roi_timeseries_concat = [roi_timeseries_concat file_roi_timeseries{k}];
        
            % tmask file:
            file_info = extractBefore(files_this_task(k).name,'_fmriprep');
            tmaskFile = sprintf('%s/FD_outputs/%s_desc-tmask_%s.txt',files_this_task(k).folder,file_info,FDtype);
            tmask{k} = table2array(readtable(tmaskFile));
            tmask_concat = [tmask_concat; tmask{k}];
        end

        % apply tmask to timeseries and calculate correlations
        corrmat = paircorr_mod(roi_timeseries_concat(:,logical(tmask_concat))');
        
        file_info = regexprep(files_this_task(k).name, '_run-\d+.*$', ''); %no run-X in this string
        fout_str = sprintf('%s/%s_corrmat_%s',outDir,file_info,atlas);
    
        figure_corrmat_GrattonLab(corrmat,atlas_params,-1,1);
        saveas(gcf,[fout_str '.tiff'],'tiff');
        close(gcf);

        % save out files
        save([fout_str '.mat'],'file_roi_timeseries','roi_timeseries_concat','tmask','tmask_concat','corrmat');
    end  
end

end %end the function

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