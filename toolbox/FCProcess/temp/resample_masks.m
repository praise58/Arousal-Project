function fnames = resample_masks(anat_string,QC,space,project_dir,singularity_cmd_start,afni_sif,res)

type_names = {'WM','CSF','WB','GREY'}; % AD - removed WM mask resampling; using make_fs_masks.m output {'WM','CSF','WB','GREY'}
types = {'label-WM_probseg','label-CSF_probseg','desc-brain_mask','label-GM_probseg'}; %{'label-WM_probseg','label-CSF_probseg','desc-brain_mask','label-GM_probseg'}

%system('module load singularity/latest');
currentDir = pwd;
cd(anat_string);

for t = 1:length(types)
    
    thisName = ['sub-' QC.subjectID '_space-' space '_' res '_' types{t} '.nii.gz'];
    thisName_orig = ['sub-' QC.subjectID '_space-' space '_' types{t} '.nii.gz'];
    fnames.([type_names{t} 'maskfile']) = [anat_string thisName];
    
   
    % only make them if they don't exist
    if ~exist(fnames.([type_names{t} 'maskfile']))
        if strcmp(res,'res-2')
            system([singularity_cmd_start 'singularity exec -B ' project_dir ':' project_dir ' ' afni_sif ' 3dresample -dxyz 2 2 2 -prefix ' thisName ' -input ' thisName_orig]);
        elseif strcmp(res,'res-1')
            system([singularity_cmd_start 'singularity exec -B ' project_dir ':' project_dir ' ' afni_sif ' 3dresample -dxyz 1 1 1 -prefix ' thisName ' -input ' thisName_orig]);
        else
            disp ('resolution not supported')
        end
    else
        fprintf('mask %s already exists \n', thisName)
    end
end

cd(currentDir);
