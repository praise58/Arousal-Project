function [og_mask] = mask_brainstem(og_mask, structure, project_dir, res)

    switch structure
        case 'brainstem'
            structure_mask = load_untouch_nii(sprintf('%s/brainstem_res-%s_mask.nii', project_dir,res));
        case 'ventricles'
            structure_mask = load_untouch_nii(sprintf('%s/ventricles_res-%s_mask.nii',project_dir, res));
        otherwise
            error('Structure not yet supported');
    end
    
    nii_file = load_untouch_nii(og_mask);
    structure_mask = single(structure_mask.img);

    masked_mask = nii_file.img.*structure_mask;
    nii_file.img = masked_mask;
    save_untouch_nii(nii_file, og_mask);
end