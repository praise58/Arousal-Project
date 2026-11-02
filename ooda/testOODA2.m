%OODA_weighted(all_data,groups,group_names,outDir,'weighted',atlas_params)


outdir = outDir
fout = '/projects/illinois/las/psych/cgratton/PD/Nifti/derivatives/Datalists/forCR/OODA_300/OODA_weighted'
%function OODA_weighted(full_mat,group_assign,group_names,outdir,fout,atlas_params)
% mat is nodes x nodes x subjects x thresholds and is already binarized

outdir = [outdir 'OODA_weighted/'];
if ~exist(outdir)
    mkdir(outdir)
end

% clear out files in output directory so that wrong parameters aren't used
command = ['rm -f ' outdir '*'];
system(command);

% set parameters for OODA
numbootstraps = 1; %1000; %1; % FOR TESTING
combo_names = sprintf('%sv%s',group_names{1},group_names{2});

fout_final = sprintf('%s%sv%s',outdir,group_names{1},group_names{2});
[gAgB_diff_mat(:,:) groupA_gstar(:,:) groupB_gstar(:,:) pvalue] = weigat_lrtPvalue_wrapper(threshbin_mat(:,:,:),groups,numbootstraps,fout_final);

%plot out a quick figure with p-values and matrices
%figure('Position',[200 200 1200 1000]);

%subplot(2,2,1); % Difference mat
%imagesc(gAgB_diff_mat);
%set(gca,'YTick',1:size(roi_names),'YTickLabel',roi_names,'FontSize',6);
%xticklabel_rotate(1:size(roi_names),90,roi_names);
%colorbar();
%colormap(colormap_b2r(-1,1));
h = figure_corrmat_powernetwork(gAgB_diff_mat,atlas_params.colors);
title(['Difference G*, weighted unthresholded, p-value: ' num2str(pvalue)],'FontSize',10);
axis square;
fig_name = [outdir 'Diff_Gstar.png'];
saveas(gcf,fig_name);

%subplot(2,2,3); %group A
%imagesc(groupA_gstar);
%set(gca,'YTick',1:size(roi_names),'YTickLabel',roi_names,'FontSize',6);
%xticklabel_rotate(1:size(roi_names),90,roi_names);
%colorbar();
%colormap(flipud(gray));
h = figure_corrmat_powernetwork(groupA_gstar,atlas_params.colors);
title(['G* ' group_names{1}],'FontSize',10);
axis square;
fig_name = [outdir 'GAstar.png'];
saveas(gcf,fig_name);

%subplot(2,2,4); %group B
%imagesc(groupB_gstar);
%set(gca,'YTick',1:size(roi_names),'YTickLabel',roi_names,'FontSize',6);
%xticklabel_rotate(1:size(roi_names),90,roi_names);
%colorbar();
%colormap(flipud(gray));
h = figure_corrmat_powernetwork(groupB_gstar,atlas_params.colors);
title(['G* ' group_names{2}],'FontSize',10);
axis square;
fig_name = [outdir 'GBstar.png'];
saveas(gcf,fig_name);

%fig_name = [outdir 'plot_' combo_names '.png'];
%saveas(gcf,fig_name);

%store group matrices
fout = sprintf('%s%s_data.mat',outdir,fout);
save(fout,'gAgB_diff_mat','groupA_gstar','groupB_gstar','pvalue','combo_names');

% remove Rout files in this directory
command = 'rm -f *Rout';
system(command);

%end
