addpath('/projects/illinois/las/psych/cgratton/member_directories/croman/NIfTI_20140122/')
addpath('/projects/illinois/las/psych/cgratton/member_directories/croman/GrattonLab-General-Repo-master/FCProcess/')
addpath('/projects/illinois/las/psych/cgratton/member_directories/croman/OODA_CR')
addpath('/projects/illinois/las/psych/cgratton/Scripts/Scripts_general/FCprocessing/')


% directory information
topDir = '/projects/illinois/las/psych/cgratton/PD/Nifti/derivatives/Datalists/forCR/';

outDir = [topDir 'OODA_SSS_BOLD_PERMUTE/'];


% some variables
group_names = {'CTRL','PD'};
thresholds = [0.02:0.01:.30];
% Load subject information file
data = readtable(fullfile('/projects/illinois/las/psych/cgratton/PD/Nifti/derivatives/Datalists/forCR/NEW_DATA_TABLE/Within_Person_Comparison_OODA.csv'),'Delimiter','\t','ReadVariableNames',false);
sub_fname = extractBefore(data.Var1,',');
sub_group = extractAfter(data.Var1,',');
% Load ROI information file
atlas_params = atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases')

frames = [3014,2033,1862,3219,3244,2762,1602,1567,2022];

% Load data for each subject
for s = 1:length(sub_fname)
    %clear sub_data
    if string(sub_group(s)) == 'CTRL'
        folderNew = fullfile(topDir,'PD_Data//No_SSS/Alert_PD/Sess_Data/ALL_SESS_CONCAT_2/',['sub-' sub_fname{s}],['sub-' sub_fname{s} '.mat']);
        sub_data = load(folderNew);
        sub_data = sub_data.allExtracted;
        all_data(:,:,s) = paircorr_mod(sub_data(:,1:frames(s))');

    elseif string(sub_group(s)) == 'PD'
        folderNew = fullfile(topDir,'PD_Data/No_SSS/Sleepy_PD/Sess_Data/ALL_SESS_CONCAT_2/',['sub-' sub_fname{s}],['sub-' sub_fname{s} '.mat']);
        sub_data = load(folderNew);
        sub_data = sub_data.allExtracted;
        all_data(:,:,s) = paircorr_mod((sub_data(:,1:frames(s-9)))');

        all_data(:,:,s) = paircorr_mod((sub_data(:,:))');
        %sub_data = load(fullfile(topDir,"ANALYSIS2_SLEEPY/FIRSTEP/ALL_SESS_CONCAT2/MATS",[sub_fname{s} '.mat']))
        %sub_data = corrmat_all
    end
    
    %all_data(:,:,s) = sub_data;
    
    % get group names -
    % Note that groups should be continguous in list!
    switch sub_group{s}
        case 'CTRL'
            groups(s) = 1;
        case 'PD'
            groups(s) = 2;
        case 'ndPD'
            groups(s) = 2; % same as above
    end
end

% some useful matrices
data_groups.CTRL = all_data(:,:,groups==1);
data_groups.PD = all_data(:,:,groups==2);
[thresh_mat,threshbin_mat] = thresh_matrix(all_data,thresholds);
save([outDir 'correl_data.mat'],'data_groups',"all_data");

% Check group assignments
% figure; imagesc(groups);
% title('Check that groups are continguous');

%%%% CORRELATION MATRICES
% Make a quick figure of group average matrices and difference
%plot_correlation_matrices(data_groups,roi_names)
%fig_name = [outDir 'GroupAvg_correlationmatrices.png'];
%saveas(gcf,fig_name);
% plot_correlation_matrices_264(data_groups,atlas_params,outDir)
% plot_correlation_matrices_264_ttest(data_groups,atlas_params,outDir)

%%%% CMD PLOTS
% CMD plot of full data matrix
figure(); %('Position',[200 200 1000 800]);
%testNumbers = {"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"};
[y,e] = cmdscale_mat(all_data,groups,'euclidean',group_names); %MDS by group
title('MDS analysis, by group, weighted and unthresholded');
axis square;
fig_name = [outDir 'MDS_weighted_unthresholded.pdf'];
%% saveas(gcf,fig_name);

% Plot for thresholded, weighted data matrix
do_cmd_bythresh(thresholds,thresh_mat,groups,'euclidean')
fig_name = [outDir 'MDS_weigthed_thresholded.pdf'];
%% saveas(gcf,fig_name);

% Plot for thresholded, binarized data matrix
do_cmd_bythresh(thresholds,threshbin_mat,groups,'hamming')
fig_name = [outDir 'MDS_unweigthed_unthresholded.pdf'];
%% saveas(gcf,fig_name);

%group_names = {'PD_Sleepy','PD_Alert'};

%%%% OODA Analyses
% run for thresholded, binarized data matrix
%OODA_by_thresh(threshbin_mat,groups,group_names,thresholds,outDir,'threshbin',roi_names)
%%HERE***OODA_by_thresh(threshbin_mat,groups,group_names,thresholds,outDir,'threshbin',atlas_params)

% run for weighted, unthresholded matrix
%OODA_weighted(all_data,groups,group_names,outDir,'weighted',roi_names)
%%% HERE %%% THIS ONE *** OODA_weighted(all_data,groups,group_names,outDir,'weighted',atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'))

% run for weighted, thresholded matrix
%OODA_by_thresh(thresh_mat,groups,group_names,thresholds,outDir,'thresh',roi_names)
%%%HERE OODA_by_thresh(thresh_mat,groups,group_names,thresholds,outDir,'thresh',atlas_params)

%%  OODA_weighted_paired(all_data,groups,group_names,outDir,'weighted',atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'));

function [thresh_mat,threshbin_mat] = thresh_matrix(mat,thresholds)
% mat is nodes x nodes x subjects
% group assignment is a vector
% thresholds ar ethe set of thresholds to output

for t = 1:length(thresholds)
    for m = 1:size(mat,3)
        temp_mat = matrix_thresholder_new(mat(:,:,m),thresholds(t),'kden'); %Threshold the matrix
        thresh_mat(:,:,m,t) = temp_mat;
        threshbin_mat(:,:,m,t) = single(0 < temp_mat); %Binarize the matrix
    end
end

end


function plot_correlation_matrices_264_ttest(data_groups,atlas_params,outDir)

ft_data_PD = atanh(data_groups.PD);
%tmat_PD = mean(ft_data_PD,3)./(std(ft_data_PD,3)/sqrt(size(ft_data_PD,3));
[h,p,ci,stats_PD] = ttest(ft_data_PD,[],[],[],3);
ft_data_CTRL = atanh(data_groups.CTRL);
%tmat_CTRL = mean(ft_data_CTRL,3)./(std(ft_data_CTRL,3)/sqrt(size(ft_data_CTRL,3));
[h,p,ci,stats_CTRL] = ttest(ft_data_CTRL,[],[],[],3);
[h,p,ci,stats_DIFF] = ttest2(ft_data_PD,ft_data_CTRL,[],[],[],3);

h = figure_corrmat_GrattonLab(real(stats_CTRL.tstat),atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'),-15,15);
%h = figure_corrmat_powernetwork(stats_CTRL.tstat,atlas_params.colors,-15,15);
title('CTRL group average','FontSize',10); colorbar();
axis square;
fig_name = [outDir 'GroupAvg_tmatrices_ctrl.png'];
%% saveas(gcf,fig_name);

h = figure_corrmat_GrattonLab(real(stats_PD.tstat),atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'),-15,15);
%h = figure_corrmat_powernetwork(stats_PD.tstat,atlas_params.colors,-15,15);
title('PD group average','FontSize',10); colorbar();
axis square;
fig_name = [outDir 'GroupAvg_tmatrices_pd.png'];
%% saveas(gcf,fig_name);

h = figure_corrmat_GrattonLab(real(stats_DIFF.tstat),atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'),-15,15);
%h = figure_corrmat_powernetwork(stats_DIFF.tstat,atlas_params.colors,-5,5);
title('PD - CTRL','FontSize',10);  colorbar();
axis square;
fig_name = [outDir 'GroupAvg_tmatrices_diff.png'];
%% saveas(gcf,fig_name);


end

%plot_correlation_matrices_264(data_groups,atlas_params,outDir)

function plot_correlation_matrices_264(data_groups,atlas_params,outDir)

%h = figure_corrmat_powernetwork(squeeze(mean(atanh(data_groups.CTRL),3)),atlas_params.colors,-0.4,1);
%We may just skip :) 
h = figure_corrmat_GrattonLab(squeeze(real(mean(atanh(data_groups.CTRL),3))),atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'),-1,1)
title('CTRL group average','FontSize',10); colorbar();
axis square;
fig_name = [outDir 'GroupAvg_correlationmatrices_ctrl.png'];
%% saveas(gcf,fig_name);

%h =
%figure_corrmat_powernetwork(squeeze(mean(atanh(data_groups.PD),3)),atlas_params.colors,-0.4,1);
h = figure_corrmat_GrattonLab(squeeze(real(mean(atanh(data_groups.PD),3))),atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'),-1,1)
title('PD group average','FontSize',10); colorbar();
axis square;
fig_name = [outDir 'GroupAvg_correlationmatrices_pd.png'];
%% saveas(gcf,fig_name);

%h = figure_corrmat_powernetwork(squeeze(mean(atanh(data_groups.PD),3)-mean(atanh(data_groups.CTRL),3)),atlas_params.colors,-0.2,0.2);
h = figure_corrmat_GrattonLab(squeeze(real(mean(atanh(data_groups.PD),3)-mean(atanh(data_groups.CTRL),3))),atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'),-0.4,0.4)
title('PD - CTRL','FontSize',10);  colorbar();
axis square;
fig_name = [outDir 'GroupAvg_correlationmatrices_diff.png'];
%% saveas(gcf,fig_name);


end

function do_cmd_bythresh(thresholds,mat,group_assign,dist_type)

figure('Position',[0 0 1000 800]);
for t = 1:length(thresholds)
    subplot(5,6,t);
    %sub_fname = {"PM23016","PM23017","PM24020","PM24025","PM24026","PM24027","PM24029","PM24031","PM25035","PM25036","PM25041","PM23014","PM24019","PM24023","PM24028","PM24030","PM24033","PM25038","PM25039","PM25040"};
    cmdscale_mat(mat(:,:,:,t),group_assign,dist_type);%Here
    switch dist_type
        case 'euclidean' %weighted
            %xlim([-2,2]); ylim([-2,2]);
            xlim([-10,10]); ylim([-10,10]);
        case 'hamming' %binary!
            xlim([-0.15,0.15]); ylim([-0.15,0.15]);
    end
    title(num2str(thresholds(t)));

end
set(findobj('Type','line'),'MarkerSize',10);

end

%%---------------------------- OODA Start -------------------------------%%

function OODA_by_thresh(full_mat,group_assign,group_names,thresholds,outdir,fout,atlas_params)
% mat is nodes x nodes x subjects x thresholds and is already binarized

outdir = [outdir 'OODA_by_' fout '/'];
if ~exist(outdir)
    mkdir(outdir)
end

% clear out files in output directory so that wrong parameters aren't used
command = ['rm -f ' outdir '*'];
system(command);

% set parameters for OODA
numbootstraps = 1000; %1000; %1; % FOR TESTING
combo_names = sprintf('%sv%s',group_names{1},group_names{2});

%
%parpool(6);
for t = 1:length(thresholds)
    %fout_final = sprintf('%s%s_%sv%s_t%f',outdir,fout,group_names{1},group_names{2},thresholds(t));
    %fout_final2 = sprintf('%s%s_%svv%s_t%f',outdir,fout,group_names{1},group_names{2},thresholds(t));
    fout_final2 = sprintf('%s%s_%svv%s_t',outdir,fout,group_names{1},group_names{2});

    switch fout
        case 'threshbin'
            %[gAgB_diff_mat(t,:,:) groupA_gstar(t,:,:) groupB_gstar(t,:,:) pvalue(t)] = bingat_glrtPvalue_wrapper(full_mat(:,:,:,t),group_assign,numbootstraps,fout_final2);
            [gAgB_diff_mat(t,:,:) groupA_gstar(t,:,:) groupB_gstar(t,:,:) pvalue(t)] = bingat_glrtPvalue_wrapper(full_mat(:,:,:,:),group_assign,numbootstraps,fout_final2);
        case 'thresh'
            [gAgB_diff_mat(t,:,:) groupA_gstar(t,:,:) groupB_gstar(t,:,:) pvalue(t)] = weigat_lrtPvalue_wrapper(full_mat(:,:,:,t),group_assign,numbootstraps,fout_final2);
    end
end
% parpool close;
% % % % 
% % % % %plot out a quick figure with p-values and matrices
% % % % %figure('Position',[200 200 1200 1000]);
% % % % %subplot(2,2,1); %p-values across threshold
% % % % figure();
% % % % plot(thresholds,pvalue,'k');
% % % % xlabel('kden threshold');
% % % % ylabel('p-value'); ylim([-0.05,1.05]);
% % % % title(combo_names);
% % % % min_p_ind = find(pvalue == min(pvalue));
% % % % if length(min_p_ind) > 1
% % % %     min_p_ind = min_p_ind(ceil(length(min_p_ind)/2));
% % % % end
% % % % hold on;
% % % % plot(thresholds(min_p_ind),pvalue(min_p_ind),'ro');
% % % % axis square;
% % % % fig_name = [outdir 'P_values.png'];
% % % % saveas(gcf,fig_name);
% % % % 
% % % % %subplot(2,2,2); % Difference mat
% % % % %imagesc(squeeze(gAgB_diff_mat(min_p_ind,:,:)));
% % % % %set(gca,'YTick',1:size(roi_names),'YTickLabel',roi_names,'FontSize',6);
% % % % %xticklabel_rotate(1:size(roi_names),90,roi_names);
% % % % %colorbar();
% % % % %colormap(colormap_b2r(-1,1));
% % % % %h = figure_corrmat_powernetwork(squeeze(gAgB_diff_mat(min_p_ind,:,:)),atlas_params.colors);
% % % % h = figure_corrmat_GrattonLab(squeeze(gAgB_diff_mat(min_p_ind,:,:)),atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'));
% % % % title(['Difference G*, thresh = ' num2str(thresholds(min_p_ind)) ', p-value: ' num2str(pvalue(min_p_ind))],'FontSize',10);
% % % % axis square;
% % % % fig_name = [outdir 'DifferenceGstar.png'];
% % % % saveas(gcf,fig_name);
% % % % 
% % % % %subplot(2,2,3); %group A
% % % % %imagesc(squeeze(groupA_gstar(min_p_ind,:,:)));
% % % % %set(gca,'YTick',1:size(roi_names),'YTickLabel',roi_names,'FontSize',6);
% % % % %xticklabel_rotate(1:size(roi_names),90,roi_names);
% % % % %colorbar();
% % % % %colormap(flipud(gray));
% % % % %h = figure_corrmat_powernetwork(squeeze(groupA_gstar(min_p_ind,:,:)),atlas_params.colors);
% % % % h = figure_corrmat_GrattonLab(squeeze(groupA_gstar(min_p_ind,:,:)),atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'));
% % % % title(['G* ' group_names{1}],'FontSize',10);
% % % % axis square;
% % % % fig_name = [outdir 'GAstar.png'];
% % % % saveas(gcf,fig_name);
% % % % 
% % % % %subplot(2,2,4); %group B
% % % % %imagesc(squeeze(groupB_gstar(min_p_ind,:,:)));
% % % % %set(gca,'YTick',1:size(roi_names),'YTickLabel',roi_names,'FontSize',6);
% % % % %xticklabel_rotate(1:size(roi_names),90,roi_names);
% % % % %colorbar();
% % % % %colormap(flipud(gray));
% % % % %h = figure_corrmat_powernetwork(squeeze(groupB_gstar(min_p_ind,:,:)),atlas_params.colors);
% % % % h = figure_corrmat_GrattonLab(squeeze(groupB_gstar(min_p_ind,:,:)),atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'));
% % % % title(['G* ' group_names{2}],'FontSize',10);
% % % % axis square;
% % % % fig_name = [outdir 'GBstar.png'];
% % % % saveas(gcf,fig_name);
% % % % 
% % % % %fig_name = [outdir 'plot_' combo_names '.png'];
% % % % %saveas(gcf,fig_name);
% % % % 
% % % % %store group matrices
% % % % fout = sprintf('%s%s_data.mat',outdir,fout);
% % % % save(fout,'gAgB_diff_mat','groupA_gstar','groupB_gstar','pvalue','combo_names');
% % % % 
% % % % % remove Rout files in this directory
% % % % command = 'rm -f *Rout';
% % % % system(command);
% % % % end
end

function OODA_weighted(full_mat,group_assign,group_names,outdir,fout,atlas_params)
% mat is nodes x nodes x subjects x thresholds and is already binarized

outdir = [outdir 'OODA_weighted/'];
if ~exist(outdir)
    mkdir(outdir)
end

% clear out files in output directory so that wrong parameters aren't used
command = ['rm -f ' outdir '*'];
system(command);

% set parameters for OODA
numbootstraps = 1000; %1000; %1; % FOR TESTING
combo_names = sprintf('%sv%s',group_names{1},group_names{2});

fout_final = sprintf('%s%s_%sv%s',outdir,fout,group_names{1},group_names{2});
[gAgB_diff_mat(:,:) groupA_gstar(:,:) groupB_gstar(:,:) pvalue] = weigat_lrtPvalue_wrapper(full_mat(:,:,:),group_assign,numbootstraps,fout_final);

end 

function OODA_weighted_paired(full_mat,group_assign,group_names,outdir,fout,atlas_params)

outdir = [outdir 'OODA_weighted/'];
if ~exist(outdir)
    mkdir(outdir)
end

% clear out files in output directory so that wrong parameters aren't used
command = ['rm -f ' outdir '*'];
system(command);

% set parameters for OODA
numbootstraps = 1000; %1000; %1; % FOR TESTING
combo_names = sprintf('%sv%s',group_names{1},group_names{2});

fout_final = sprintf('%s%s_%sv%s',outdir,fout,group_names{1},group_names{2});

[g1g2_diff_mat group1_gstar group2_gstar pvalue] = OODA_weighted_pairedPvalue_wrapper(full_mat(:,:,:),group_assign,numbootstraps,fout_final);

end 
%plot out a quick figure with p-values and matrices
%figure('Position',[200 200 1200 1000]);

%subplot(2,2,1); % Difference mat
%imagesc(gAgB_diff_mat);
%set(gca,'YTick',1:size(roi_names),'YTickLabel',roi_names,'FontSize',6);
%xticklabel_rotate(1:size(roi_names),90,roi_names);
%colorbar();
%colormap(colormap_b2r(-1,1));

%h = figure_corrmat_powernetwork(gAgB_diff_mat,atlas_params.colors);
% % % h = figure_corrmat_GrattonLab(gAgB_diff_mat,atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'));
% % % title(['Difference G*, weighted unthresholded, p-value: ' num2str(pvalue)],'FontSize',10);
% % % axis square;
% % % fig_name = [outdir 'Diff_Gstar.png'];
% % % saveas(gcf,fig_name);

%subplot(2,2,3); %group A
%imagesc(groupA_gstar);
%set(gca,'YTick',1:size(roi_names),'YTickLabel',roi_names,'FontSize',6);
%xticklabel_rotate(1:size(roi_names),90,roi_names);
%colorbar();
%colormap(flipud(gray));

% % % %h = figure_corrmat_powernetwork(groupA_gstar,atlas_params.colors);
% % % h = figure_corrmat_GrattonLab(groupA_gstar,atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'));
% % % title(['G* ' group_names{1}],'FontSize',10);
% % % axis square;
% % % fig_name = [outdir 'GAstar.png'];
% % % saveas(gcf,fig_name);

%subplot(2,2,4); %group B
%imagesc(groupB_gstar);
%set(gca,'YTick',1:size(roi_names),'YTickLabel',roi_names,'FontSize',6);
%xticklabel_rotate(1:size(roi_names),90,roi_names);
%colorbar();
%colormap(flipud(gray));
%h = figure_corrmat_powernetwork(groupB_gstar,atlas_params.colors);

% % % h = figure_corrmat_GrattonLab(groupB_gstar,atlas_parameters_GrattonLab('Seitzman300','/projects/illinois/las/psych/cgratton/Atlases'));
% % % title(['G* ' group_names{2}],'FontSize',10);
% % % axis square;
% % % fig_name = [outdir 'GBstar.png'];
% % % saveas(gcf,fig_name);

%fig_name = [outdir 'plot_' combo_names '.png'];
%saveas(gcf,fig_name);

%store group matrices

% % % fout = sprintf('%s%s_data.mat',outdir,fout);
% % % save(fout,'gAgB_diff_mat','groupA_gstar','groupB_gstar','pvalue','combo_names');

% remove Rout files in this directory

% % % command = 'rm -f *Rout';
% % % system(command);
%end
