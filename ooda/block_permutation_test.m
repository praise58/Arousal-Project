function [passblocks,passblocks_full] = block_permutation_test(atlas_params,all_data,mean_data_mat,diffType,outDir,PDsubs,CTRLsubs)

% get info on blocks
[blockIds,blockId_labels] = getBlockIds(atlas_params);

% load passblocks - All Pass
blockIds_pass = blockIds;
goodBlocks = unique(blockIds_pass);
goodBlocks = goodBlocks(goodBlocks > 0); %remove the zero

% make a mask for the upper tri
mask = ones(size(mean_data_mat.diff));
mask = logical(triu(mask,1));

%%%% Better to actually do a permutation test - ok to do with unbalanced
%%%% groups?
nperms = 100;
rng('shuffle');
for i = 1:nperms
    perm_suborder = randperm(size(all_data,3));
    perm_all_data = all_data(:,:,perm_suborder);
    tmp = mean(perm_all_data(:,:,PDsubs),3) - mean(perm_all_data(:,:,CTRLsubs),3);
    perm_diffs.diff(i,:) = tmp(mask);
    perm_diffs.abs_diff(i,:) = abs(tmp(mask));   
    perm_diffs_mat.diff(i,:,:) = tmp;
    perm_diffs_mat.abs_diff(i,:,:) = abs(tmp);
    clear perm_suborder perm_all_data tmp;
end


% calc means for each block
for gB = 1:length(goodBlocks)
   switch diffType
       case 'abs'
           GBblockMean(gB) = mean(mean_data_mat.abs_diff(blockIds_pass == goodBlocks(gB)));
           GBblockMean_SIGNED(gB) = mean(mean_data_mat.diff(blockIds_pass == goodBlocks(gB)));
           %GBblockStd(gB) = std(mean_data_mat.abs_diff(blockIds_pass == goodBlocks(gB)));
           for i = 1:nperms
               tmp_mat = perm_diffs_mat.abs_diff(i,:,:);
               perm_GBblockMean(i,gB) = mean(tmp_mat(blockIds_pass == goodBlocks(gB)));
           end
           %[tsort tsorti] = sort(perm_GBblockMean(:,gB));
           GBblockPvals(gB) = sum(perm_GBblockMean(:,gB) > GBblockMean(gB))/nperms;
           GBblockPermZ(gB) = (GBblockMean(gB) - mean(perm_GBblockMean(:,gB)))/std(perm_GBblockMean(:,gB));
       case 'reg'
           GBblockMean(gB) = mean(mean_data_mat.diff(blockIds_pass == goodBlocks(gB)));
           %GBblockStd(gB) = std(mean_data_mat.diff(blockIds_pass == goodBlocks(gB)));
           for i = 1:nperms
               tmp_mat = perm_diffs_mat.diff(i,:,:);
               perm_GBblockMean(i,gB) = mean(tmp_mat(blockIds_pass == goodBlocks(gB)));
           end
           %[tsort tsorti] = sort(perm_GBblockMean(:,gB));
           GBblockPvals(gB) = sum(abs(perm_GBblockMean(:,gB)) > abs(GBblockMean(gB)))/nperms;
           GBblockPermZ(gB) = (GBblockMean(gB) - mean(perm_GBblockMean(:,gB)))/std(perm_GBblockMean(:,gB));
   end
end

GBblockPvals_FDR = FDR(GBblockPvals);

%GBblockMatZ = (GBblockMean - full_mat_mean)/full_mat_std;
%GBblockPermZ = (GBblockMean - perm_full_mat_mean)/perm_full_mat_std;

dispMat = zeros(size(blockIds));
dispMat2 = zeros(size(blockIds));
dispMat3 = zeros(size(blockIds));
dispMat4 = zeros(size(blockIds));
for gB = 1:length(goodBlocks)
    dispMat(blockIds_pass == goodBlocks(gB)) = GBblockPermZ(gB);
    dispMat2(blockIds_pass == goodBlocks(gB)) = GBblockMean(gB);    
    dispMat3(blockIds_pass == goodBlocks(gB)) = GBblockPvals_FDR(gB);
    if strcmp(diffType,'abs')
        dispMat4(blockIds_pass == goodBlocks(gB)) = GBblockMean_SIGNED(gB);
    end    
end

% make matrices
dispMat = dispMat + dispMat';
dispMat2 = dispMat2 + dispMat2';
dispMat3 = dispMat3 + dispMat3';

% thresholding matrix
passblocks_full = dispMat3<0.05;
passblocks = logical(triu(passblocks_full,1));

% and threshold other plots
dispMat(~passblocks_full) = 0;
dispMat2(~passblocks_full) = 0;
figure_corrmat_network_generic_NOsort(dispMat,atlas_params,-10,10);
save_fig(gcf,[outDir diffType '_zscoremat_FDRcorr.pdf']);
figure_corrmat_network_generic_NOsort(dispMat2,atlas_params,-0.1,0.1);
save_fig(gcf,[outDir diffType '_meanmat_FDRcorr.pdf']);
if strcmp(diffType,'abs')
    dispMat4 = dispMat4 + dispMat4';
    dispMat4(~passblocks_full) = 0;
    figure_corrmat_network_generic_NOsort(dispMat4,atlas_params,-0.1,0.1);
    save_fig(gcf,[outDir diffType '_SIGNEDmeanmat_FDRcorr.pdf']);
end    


save([outDir diffType 'diff_BlockwiseTest.mat'],'GBblockMean','GBblockPermZ','GBblockPvals','GBblockPvals_FDR','perm_diffs_mat','mean_data_mat');
save([outDir diffType 'diff_Passblocks.mat'],'passblocks','passblocks_full');

[GBBsort GBBsorti] = sort(GBblockPermZ,2,'descend');
fid = fopen([outDir diffType '_blockRankings_table.txt'],'w');
fprintf(fid,'Pval\tFDR-Pval\tSTDperm\tMean\tNames\n');
for i = 1:length(GBBsort)
    fprintf(fid,'%.03f\t%.03f\t%.02f\t%.04f\t%s\n',GBblockPvals(GBBsorti(i)),GBblockPvals_FDR(GBBsorti(i)),GBblockPermZ(GBBsorti(i)),GBblockMean(GBBsorti(i)),blockId_labels{GBBsorti(i)});
end
fclose(fid);
   


end

function [blockIds,blockId_labels] = getBlockIds(atlas_params)

blockIds = zeros(atlas_params.num_rois);
count = 1;
for m1 = 1:length(atlas_params.mods)
    for m2 = 1:length(atlas_params.mods)
        blockIds(atlas_params.mods{m1},atlas_params.mods{m2}) = count;
        blockId_labels_init{count} = [atlas_params.networks{m1} '-' atlas_params.networks{m2}];
        count = count+1;
    end
end

blockIds = blockIds(atlas_params.sorti,atlas_params.sorti);
blockIds = triu(blockIds,1);
gblocks = unique(blockIds); gblocks = gblocks(gblocks > 0);
blockId_labels = blockId_labels_init(gblocks);

end