function make_fs_masks(sub)

%%%%%%%%%%%%%%%%%%%%%%
% This makes several erosions of WM and CSF masks, including no erosion.
% Input:
%       - sub: (str), subject number e.g., '1' (i.e., strip the 'sub-' from
%      the subject, so in this case the subject was 'sub-1')
% 
% Output:
%       - TODO: list all the different files and outputs
%
% The outputs (e.g., 'sub-INET003_space-MNI152NLin6Asym_label-WM_probseg_0.9mask_res-2_ero3.nii.gz')
% are written into the subject's overall 'anat' folder.
%
%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------
% Load parameters
if nargin == 1
    
    cfg = get_config(sub);

    fmriprepTopDir = cfg.fmriprepTopDir;
    space = cfg.space;
    voxdim = cfg.voxdim; 
    eroiterwm = cfg.eroiterwm; 
    GMprobseg_thresh = cfg.GMprobseg_thresh;
    WMprobseg_thresh = cfg.WMprobseg_thresh;
    eroitercsf = cfg.eroitercsf;
    CSFprobseg_thresh = cfg.CSFprobseg_thresh;
    include_brainstem_ventricles_masks = cfg.include_brainstem_ventricles_masks;
    maskDir = cfg.maskDir;

    labDir = cfg.labDir; %where the .sif files are
    singularity_cmd_start = cfg.singularity_cmd_start; 
    afni_sif = cfg.afni_sif; 
    templateflow_dir = cfg.templateflow_dir; 
    fsl_cmd_start = cfg.fsl_cmd_start; 
    ants_sif = cfg.ants_sif;
    disp('config files loaded')

else
    error('check config file');
end

% Add paths to toolbox we need
addpath(genpath(cfg.preproc_path));
addpath(genpath(cfg.bids_toolbox));
addpath(genpath(cfg.niftiread_toolbox));
disp('toolboxes loaded')
%---------------------------------------
%Define filenames for GM
GMprobseg = ['sub-' sub '_space-' space '_label-GM_probseg.nii.gz'];
GMmaskname = ['sub-' sub '_space-' space '_label-GM_probseg_' num2str(GMprobseg_thresh) 'mask.nii.gz'];

% Define filenames for WM
WMprobseg = ['sub-' sub '_space-' space '_label-WM_probseg.nii.gz'];
WMmaskname = ['sub-' sub '_space-' space '_label-WM_probseg_' num2str(WMprobseg_thresh) 'mask.nii.gz'];

% Define filenames for CSF
CSFprobseg = ['sub-' sub '_space-' space '_label-CSF_probseg.nii.gz'];
CSFmaskname = ['sub-' sub '_space-' space '_label-CSF_probseg_' num2str(CSFprobseg_thresh) 'mask.nii.gz'];

% Define anat directory
anat_dir = fullfile(fmriprepTopDir, ['sub-' sub], 'anat');
disp('names defined')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ensure anat directory exists
if exist(anat_dir, 'dir')
    cd(anat_dir);
else
    disp('No overall anat directory; assuming subject has 1 scan session with a T1')
    
    % Find session with anat dir
    sessions = dir(fullfile(fmriprepTopDir, ['sub-' sub], 'ses-*'));
    sessions = sessions([sessions(:).isdir]);
    sessions = {sessions.name};
    anat_ses = '';
    for s = 1:length(sessions)
        if exist(fullfile(fmriprepTopDir, ['sub-' sub], sessions{s}, 'anat'), 'dir')
            anat_ses = sessions{s};
            break
        end
    end
    if isempty(anat_ses)
        error('No session found with an anat directory')
    end
    
    % Locate existing T1; want the one in native space
    T1w_filenames = dir(fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', '*desc-preproc_T1w.nii.gz'));
    T1w_filenames = {T1w_filenames.name};
    T1w_filenames = T1w_filenames(cellfun(@(s) isempty(regexp(s, 'space')), T1w_filenames)); % eliminate any T1s in atlas space
    
    if isempty(T1w_filenames)
        error('No T1 images found in session anat folder; check T1s and retry')
    elseif length(T1w_filenames) > 1
        error('More than 1 usable T1 image found in session anat folder; consider linking files manually')
    else
        % Make anat dir, link necessary files to those in session with anat folder
        disp(['T1 in native space identified as: ' T1w_filenames{1}])
        disp('    Continuing...')
        mkdir(anat_dir); cd(anat_dir);
        system(['ln -s ' fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', T1w_filenames{1}) ' sub-' sub '_desc-preproc_T1w.nii.gz']);
        
        % Link transformation file
        h5_filename = dir(fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', ['*from-T1w_to-' space '_mode-image_xfm.h5']));
        if isempty(h5_filename)
            error('Transformation file from T1w to target space not found.')
        end
        h5_filename = h5_filename(1).name;
        system(['ln -s ' fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', h5_filename) ' sub-' sub '_from-T1w_to-' space '_mode-image_xfm.h5']);
        
        % Link WM_probseg
        WM_filename = dir(fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', '*WM_probseg.nii.gz'));
        if isempty(WM_filename)
            error('WM_probseg file not found.')
        end
        WM_filename = WM_filename(1).name;
        system(['ln -s ' fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', WM_filename) ' sub-' sub '_label-WM_probseg.nii.gz']);
        
        % Link CSF_probseg
        CSF_filename = dir(fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', '*CSF_probseg.nii.gz'));
        if isempty(WM_filename)
            error('CSF_probseg file not found.')
        end
        CSF_filename = CSF_filename(1).name;
        system(['ln -s ' fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', CSF_filename) ' sub-' sub '_label-CSF_probseg.nii.gz']);
        
        % Link other masks in the standard space (GM, CSF, brain mask)
        GM_filename = dir(fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', ['*_space-' space '_res-' voxdim '_*GM_probseg.nii.gz']));
        if isempty(GM_filename)
            error('GM_probseg file not found.')
        end
        GM_filename = GM_filename(1).name;
        system(['ln -s ' fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', GM_filename) ' sub-' sub '_space-' space '_res-' voxdim '_label-GM_probseg.nii.gz']);
        
        CSF_filename = dir(fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', ['*_space-' space '_res-' voxdim '_*CSF_probseg.nii.gz']));
        if isempty(CSF_filename)
            error('CSF_probseg file not found.')
        end
        CSF_filename = CSF_filename(1).name;
        system(['ln -s ' fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', CSF_filename) ' sub-' sub '_space-' space '_res-' voxdim '_label-CSF_probseg.nii.gz']);
        
        WM_filename = dir(fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', ['*_space-' space '_res-' voxdim '_*WM_probseg.nii.gz']));
        if isempty(WM_filename)
            error('WM_probseg file not found.')
        end
        WM_filename = WM_filename(1).name;
        system(['ln -s ' fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', WM_filename) ' sub-' sub '_space-' space '_res-' voxdim '_label-WM_probseg.nii.gz']);
        

        brainmask_filename = dir(fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', ['*_space-' space '_res-' voxdim '_*brain_mask.nii.gz']));
        if isempty(brainmask_filename)
            error('Brain mask file not found.')
        end
        brainmask_filename = brainmask_filename(1).name;
        system(['ln -s ' fullfile(fmriprepTopDir, ['sub-' sub], anat_ses, 'anat', brainmask_filename) ' sub-' sub '_space-' space '_res-' voxdim '_desc-brain_mask.nii.gz']);
    end
end

% Check if the template file exists
T1_templateLoc = fullfile(templateflow_dir, ['tpl-' space], ['tpl-' space '_res-0' voxdim '_T1w.nii.gz']); %DP has this set to res-01. Why?
if ~exist(T1_templateLoc, 'file')
    error(['Template file not found: ' T1_templateLoc]);
end

% Define input and output names for ANTs transformation
inNames = {['sub-' sub '_desc-preproc_T1w.nii.gz'], ['sub-' sub '_label-WM_probseg.nii.gz'], ['sub-' sub '_label-CSF_probseg.nii.gz'], ['sub-' sub '_label-GM_probseg.nii.gz']};
outNames = {['sub-' sub '_space-' space '_desc-preproc_T1w.nii.gz'], WMprobseg, CSFprobseg, GMprobseg};

% Apply transformations using ANTs
for tform = 1:length(inNames)
    if ~exist(outNames{tform}, 'file')
        command = sprintf('%ssingularity exec -B %s:%s,%s %s antsApplyTransforms --verbose -i %s -o %s -r %s -t sub-%s_from-T1w_to-%s_mode-image_xfm.h5', ...
            singularity_cmd_start, labDir, labDir, anat_dir, ants_sif, inNames{tform}, outNames{tform}, T1_templateLoc, sub, space);
        system(command);
    end
end

%%% threshold at GMprobseg_thresh and binarize %%%
system([fsl_cmd_start 'fslmaths ' GMprobseg  ' -thr ' num2str(GMprobseg_thresh) ' -bin ' GMmaskname]);

%%%%% Threshold and binarize WM mask %%%%%
system([fsl_cmd_start 'fslmaths ' WMprobseg ' -thr ' num2str(WMprobseg_thresh) ' -bin ' WMmaskname]);

%%%%% Erode WM mask to avoid possible gray matter contamination %%%%%
iter = 0;
% Resample WM mask to desired voxel size
wm_resampled = [WMmaskname(1:end-7) '_res-' voxdim '.nii.gz'];
command = sprintf('%ssingularity exec -B %s:%s,%s %s 3dresample -dxyz %s %s %s -prefix %s -input %s', ...
    singularity_cmd_start, labDir, labDir, anat_dir, afni_sif, voxdim, voxdim, voxdim, wm_resampled, WMmaskname);
system(command);

% Binarize the resampled WM mask
wm_ero0 = [WMmaskname(1:end-7) '_res-' voxdim '_ero0.nii.gz'];
system([fsl_cmd_start 'fslmaths ' wm_resampled ' -bin ' wm_ero0]);
% Mask out the brainstem and basal ganglia
if include_brainstem_ventricles_masks
    mask_brainstem(wm_ero0, 'brainstem', maskDir,voxdim)
end

% Perform erosions iteratively
for iter = 1:eroiterwm
    % Erode the WM mask
    system([fsl_cmd_start 'fslmaths ' WMmaskname ' -kernel 3D -ero ' WMmaskname]);
    
    % Resample the eroded WM mask
    system(sprintf('%ssingularity exec -B %s:%s,%s %s 3dresample -dxyz %s %s %s -prefix %s -input %s -overwrite', ...
        singularity_cmd_start, labDir, labDir, anat_dir, afni_sif, voxdim, voxdim, voxdim, wm_resampled, WMmaskname));
    
    % Binarize the resampled eroded WM mask
    wm_ero = [WMmaskname(1:end-7) '_res-' voxdim '_ero' num2str(iter) '.nii.gz'];
    system([fsl_cmd_start 'fslmaths ' wm_resampled ' -bin ' wm_ero]);
    mask_brainstem(wm_ero, 'brainstem', maskDir, voxdim)
end

%%%%% Remove unnecessary WM mask files %%%%%
system(['rm ' fullfile(anat_dir, WMmaskname) ' ' fullfile(anat_dir, wm_resampled)]);

%%%%% Threshold and binarize CSF mask %%%%%
system([fsl_cmd_start 'fslmaths ' CSFprobseg ' -thr ' num2str(CSFprobseg_thresh) ' -bin ' CSFmaskname]);

%%%%% Erode CSF mask to avoid contamination %%%%%
iter = 0;
% Resample CSF mask to desired voxel size
csf_resampled = [CSFmaskname(1:end-7) '_res-' voxdim '.nii.gz'];
command = sprintf('%ssingularity exec -B %s:%s,%s %s 3dresample -dxyz %s %s %s -prefix %s -input %s', ...
    singularity_cmd_start, labDir, labDir, anat_dir, afni_sif, voxdim, voxdim, voxdim, csf_resampled, CSFmaskname);
system(command);

% Binarize the resampled CSF mask
csf_ero0 = [CSFmaskname(1:end-7) '_res-' voxdim '_ero0.nii.gz'];
system([fsl_cmd_start 'fslmaths ' csf_resampled ' -bin ' csf_ero0]);
% Mask out the brainstem and basal ganglia
mask_brainstem(csf_ero0, 'ventricles', maskDir, voxdim)

% Perform erosions iteratively for CSF
for iter = 1:eroitercsf
    % Erode the CSF mask
    system([fsl_cmd_start 'fslmaths ' CSFmaskname ' -kernel 3D -ero ' CSFmaskname]);
    
    % Resample the eroded CSF mask
    system(sprintf('%ssingularity exec -B %s:%s,%s %s 3dresample -dxyz %s %s %s -prefix %s -input %s -overwrite', ...
        singularity_cmd_start, labDir, labDir, anat_dir, afni_sif, voxdim, voxdim, voxdim, csf_resampled, CSFmaskname));
    
    % Binarize the resampled eroded CSF mask
    csf_ero = [CSFmaskname(1:end-7) '_res-' voxdim '_ero' num2str(iter) '.nii.gz'];
    system([fsl_cmd_start 'fslmaths ' csf_resampled ' -bin ' csf_ero]);
    mask_brainstem(csf_ero, 'ventricles', maskDir, voxdim)

end

%%%%% Remove unnecessary CSF mask files %%%%%
system(['rm ' fullfile(anat_dir, CSFmaskname) ' ' fullfile(anat_dir, csf_resampled)]);

end
