function bin_mat = cmd_bythresh(mat,group_assign,group_names,thresholds,atlas_params,varargin)
% mat is nodes x nodes x subjects
% group assignment is a vector with

figure('Position',[0 0 1000 800]);
for t = 1:length(thresholds)
    
    % First prep and binarize matrix
    if isempty(varargin)
        for m = 1:size(mat,3)
            bin_mat(:,:,m,t) = prep_matrix(mat(:,:,m),thresholds(t),atlas_params.dist_thresh,atlas_params.roi_file);
        end
    elseif strcmp(varargin{1},'binarized')
        bin_mat = mat;
    else
        error('Not an appropriate input');
    end
    
    subplot(5,6,t);
    cmdscale_mat(bin_mat(:,:,:,t),group_assign,'hamming');
    xlim([-0.15,0.15]); ylim([-0.15,0.15]);
    title(num2str(thresholds(t)));
    
end
set(findobj('Type','line'),'MarkerSize',10);

end