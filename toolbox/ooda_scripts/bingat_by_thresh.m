function bingat_by_thresh(full_mat,group_assign,fout,group_names,thresholds,outdir)
% mat is nodes x nodes x subjects x thresholds and is already binarized

% First, check that you're on the right machine. Skip if not.
[~, name] = system('hostname');

outdir = [outdir 'OODA/'];
if ~exist(outdir)
    mkdir(outdir)
end

% clear out files in output directory so that wrong parameters aren't used
command = ['rm -f ' outdir '*'];
system(command);

switch strtrim(name)
    case 'typhoon' %only proceed on typhoon
        
        numbootstraps = 1000; %1000; %1; % FOR TESTING
        group_nums = unique(group_assign);
        group_combos = nchoosek(group_nums,2);
        
        for gc = 1:size(group_combos,1)
            %fout = [fout '_comp' group_names(group_combos(gc,1)) 'v' group_names(group_combos(gc,2))];
            
            group_indA = find(group_assign == group_combos(gc,1));
            group_indB = find(group_assign == group_combos(gc,2));
            group_ind = union(group_indA,group_indB);
            combo_names{gc} = sprintf('%sv%s',group_names{group_combos(gc,1)},group_names{group_combos(gc,2)});
            
            matlabpool open 6;
            parfor t = 1:length(thresholds)
                fout_final = sprintf('%s%s_%sv%s_t%f',outdir,fout,group_names{group_combos(gc,1)},group_names{group_combos(gc,2)},thresholds(t));
                %[gAgB_diff_mat(gc,t,:,:) groupA_gstar(gc,t,:,:) groupB_gstar(gc,t,:,:) pvalue(gc,t)] = bingat_glrtPvalue_wrapper(full_mat(:,:,group_ind,t),group_assign(group_ind),numbootstraps,fout_final);
                [gAgB_diff_mat(gc,t,:,:) groupA_gstar(gc,t,:,:) groupB_gstar(gc,t,:,:) pvalue(gc,t)] = OODA_pairedPvalue_wrapper(full_mat(:,:,group_ind,t),group_assign(group_ind),numbootstraps,fout_final);
            end
            matlabpool close;
            
            %plot out a quick figure with p-values and matrices
            figure('Position',[0 0 800 800]);
            subplot(2,2,1); %p-values across threshold
            plot(thresholds,pvalue(gc,:),'k');
            xlabel('kden threshold');
            ylabel('p-value'); ylim([-0.05,1.05]);
            title(combo_names{gc});
            min_p_ind = find(pvalue(gc,:) == min(pvalue(gc,:)));
            if length(min_p_ind) > 1
                min_p_ind = min_p_ind(ceil(length(min_p_ind)/2));
            end
            hold on;
            plot(thresholds(min_p_ind),pvalue(gc,min_p_ind),'ro');
            
            subplot(2,2,2); % Difference mat
            imagesc(squeeze(gAgB_diff_mat(gc,min_p_ind,:,:)));
            colormap(colormap_b2r(-1,1));
            title(['Difference G*, thresh = ' num2str(thresholds(min_p_ind)) ', p-value: ' num2str(pvalue(gc,min_p_ind))]);
            
            subplot(2,2,3); %group A
            imagesc(squeeze(groupA_gstar(gc,min_p_ind,:,:)));
            colormap(flipud(gray));
            title(['G* ' group_names{group_combos(gc,1)}]);
            
            subplot(2,2,4); %group B
            imagesc(squeeze(groupB_gstar(gc,min_p_ind,:,:)));
            colormap(flipud(gray));
            title(['G* ' group_names{group_combos(gc,2)}]);
            
            fig_name = [outdir 'plot_' combo_names{gc} '.png'];
            save_fig(gcf,fig_name);
            
        end
        
        %store group matrices
        fout = sprintf('%s%s_data.mat',outdir,fout);
        save(fout,'gAgB_diff_mat','groupA_gstar','groupB_gstar','pvalue','combo_names');
        
        colors = distinguishable_colors(length(group_combos)); %{'r','g','b'};
        figure;
        for gc = 1:size(group_combos)
            plot(thresholds,pvalue(gc,:),'Color',[colors(gc,1),colors(gc,2),colors(gc,3)]); hold on;
        end
        legend(combo_names);
        xlabel('kden threshold');
        ylabel('p-value'); ylim([-0.05,1.05]);
        title('p-values for different comparisons');
        fig_name = [outdir 'plot_pvalues_all.png'];
        save_fig(gcf,fig_name);
        
    otherwise
        display('On wrong machine for bingat; not proceeding');
end

% remove Rout files in this directory
command = ['rm -f *Rout'];
system(command);
end