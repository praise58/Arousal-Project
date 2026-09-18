%This script creates a group mask and the correct atlas parameters to be
%used to create the Adjacency Matrices
%%%%%%%%%%%%%%%%%% Variables to change start here %%%%%%%%%%%%%%%%%%%%%%%%%
path = '/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/brain masks'; % where my brain_masks are
atlas_dir = '/Volumes/illinois-las-psych-gratton/networks-pm/Atlases'; %Where are the atlases?
atlas = 'Seitzman300'; % What atlas will be used? WARNING: only Seitzman300-res1 available for 1mm data
image_size = [91, 109, 91]; % this is for 1mm, change for 2mm
savepath = sprintf('%s/group_mask_v2.mat', path); %where do you want your group masks saved?
savepath2 = sprintf('%s/atlas_params_v2.mat', path); %where do you want the new atlas params saved?
files = load("/Volumes/illinois-las-psych-gratton/networks-pm/arousal/Arousal-Project/Arousal-Project/brain masks/mask_paths.mat");

%%%%%%%%%%%%%%%%%% and end here %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addpath(genpath('/Volumes/illinois-las-psych-gratton/networks-pm/software/GrattonLab-General-Repo'));
addpath(genpath('/Volumes/illinois-las-psych-gratton/networks-pm/software/nifti'));
addpath(genpath('/Volumes/illinois-las-psych-gratton/networks-pm/software/hline_vline'));
disp('toolboxes loaded')

%arrange files
files = files.filesStructv8;

%create the group mask
mask = uint8(ones(image_size(1)*image_size(2)*image_size(3),1));
for i = 1:length(files)
    m = load_nii_wrapper(fullfile(files(i).path, files(i).name)); %if you are using a csv file, you'd need to modify what's inside the load_nii_wrapper
    mask = mask.*m;
end

%create the group atlas
atlas_params = atlas_parameters_GrattonLab(atlas,atlas_dir);
roi_data = load_nii_wrapper(atlas_params.MNI_nii_file); %vox by 1
mask = int16(mask);
group_mask = roi_data.*mask; %group atlas created!
save(savepath, 'group_mask') %save it, we will use it later

%which are the excluded regions?
excluded = setdiff(unique(roi_data), unique(group_mask));

%create the new atlas params for Seitzman-300
% load ROI information
[x y z rad netNum netLabel tmp] = textread(atlas_params.roi_file,'%f%f%f%f%d%s%f','headerlines',1);
x(excluded)=[]; y(excluded)=[]; z(excluded)=[]; %exclude those ROIs with no info
rad(excluded)=[]; netNum(excluded)=[]; netLabel(excluded)=[]; tmp(excluded)=[];
for n = 1:length(atlas_params.networks_fullnames)
    atlas_params.mods{n} = find(strcmp(netLabel,atlas_params.networks_fullnames{n}));
end
atlas_params = rmfield(atlas_params,'mods_array');
for nc = 1:length(atlas_params.mods)
    atlas_params.mods_array(atlas_params.mods{nc}) = nc;
end
atlas_params = rmfield(atlas_params,'mods_array_workbench');
atlas_params.mods_array_workbench = netNum; %numbers in workbench
atlas_params.num_rois = length(x);
switch atlas_params.sorted_by
    case 'structure'
        warning('sorting based on structure first, then net');
        % note that sorting by structure will do weird things with
        % transitions in subcortical
        atlas_params.transitions = find(diff(atlas_params.mods_array)) + 1;
        atlas_params.centers = compute_centers(atlas_params.mods_array);
        atlas_params.sorti = 1:atlas_params.num_rois;
    case 'network'
        warning('sorting by network first, then structure');
        [communities atlas_params.sorti] = sort(atlas_params.mods_array);
        atlas_params.transitions = find(communities(1:end-1) ~= communities(2:end));
        transitions_plusends = [1 atlas_params.transitions(:)' length(communities)];
        atlas_params.centers = transitions_plusends(1:end-1) + ((transitions_plusends(2:end) - transitions_plusends(1:end-1))/2);
end
atlas_params.MNI_nii_file = savepath;
atlas_params = rmfield(atlas_params, 'roi_file');
atlas_params = rmfield(atlas_params, 'anat_labels');
save(savepath2, 'atlas_params')