function [fcimg]=bolds2mat(bolds,trtot,trborders,GLMmask,WBmask_sub)

vox = nnz(GLMmask);
fcimg=zeros(vox,trtot);

temp = load_nii_wrapper([bolds '.nii.gz']);

%CG added, based on BK code, based on EMG code
temp1000 = mode1000norm(temp,WBmask_sub); % use the more sub specific mask for this

fcimg(:,trborders(1,1):trborders(1,2))=temp1000(logical(GLMmask),:);
clear temp temp1000;
