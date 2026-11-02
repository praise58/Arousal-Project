function cmdscale_subsets(full_mat,group_assign,group_assign_sub,fout,group_names,varargin)
% mat is nodes x nodes x subjects
% varargin: whether or not data is binarized
%   if binarized, plot by thresholds on polar plot
%   varargin{1} 'binarized' or []
%   varargin{2} thresholds list (if first is not [])
%   varargin{3} whether or not to plot the best dimensions

if isempty(varargin)
    binarized = 0;
else
    binarized = 1;
    thresholds = varargin{2};
    if length(varargin) > 2
        plot_best = 1;
    else
        plot_best = 0;
    end
end

group_nums = unique(group_assign);
group_combos = nchoosek(group_nums,2);

for gc = 1:size(group_combos,1)
    %fout = [fout '_comp' group_names(group_combos(gc,1)) 'v' group_names(group_combos(gc,2))];
    
    group_indA = find(group_assign == group_combos(gc,1));
    group_indB = find(group_assign == group_combos(gc,2));
    group_ind = union(group_indA,group_indB);
    
    
    group_names_here{1} = group_names{group_combos(gc,1)};
    group_names_here{2} = group_names{group_combos(gc,2)};
    
    h1 = figure('Position',[200 200 1000 800]);
    
    if binarized
        display(['MDS subsets polar ' group_names_here{1} ' vs ' group_names_here{2}]);
        h2 = figure('Position',[200 200 1000 800]);
        for t = 1:length(thresholds)
            %figure(h1);
            set(0,'CurrentFigure',h1);
            subplot(5,6,t);
            [y,e,diff_all_mean] = cmdscale_mat_polar(full_mat(:,:,group_ind,t),group_assign(group_ind),'hamming',{},plot_best);
            title(num2str(thresholds(t)));
            
            %figure(h2);
            set(0,'CurrentFigure',h2);
            subplot(5,6,t);
            [b,i] = sort(abs(diff_all_mean));
            plot(abs(diff_all_mean),'k'); hold on;
            if plot_best
                plot(i(end),abs(diff_all_mean(i(end))),'ro');
                plot(i(end-1),abs(diff_all_mean(i(end-1))),'go');
            else
                plot(1,abs(diff_all_mean(1)),'ro');
                plot(2,abs(diff_all_mean(2)),'go');
            end
            xlabel('MDS dims');
            ylabel('Abs distance');
            title(num2str(thresholds(t)));

        end
        if plot_best
            fout_final = sprintf('%s_%sv%s_binpolar_bestdims.png',fout,group_names{group_combos(gc,1)},group_names{group_combos(gc,2)});
            fout2_final = sprintf('%s_%sv%s_dimplot_bestdims.png',fout,group_names{group_combos(gc,1)},group_names{group_combos(gc,2)});
        else
            fout_final = sprintf('%s_%sv%s_binpolar.png',fout,group_names{group_combos(gc,1)},group_names{group_combos(gc,2)});
            fout2_final = sprintf('%s_%sv%s_dimplot.png',fout,group_names{group_combos(gc,1)},group_names{group_combos(gc,2)});
        end
        save_fig(h2,fout2_final);
    else

        subplot(1,2,1)
        %MDS plot
        [y,e] = cmdscale_mat(full_mat(:,:,group_ind),group_assign(group_ind),'euclidean',group_names_here);
        axis square;
        
        %for i = 1:(length(group_ind)/2)
        %    sub_names{i} = num2str(i);
        %end
        
        subplot(1,2,2)
        %MDS plot - subject
        [y,e] = cmdscale_mat(full_mat(:,:,group_ind),group_assign_sub(group_ind),'euclidean');%,sub_names);
        axis square;
        fout_final = sprintf('%s_%sv%s.png',fout,group_names{group_combos(gc,1)},group_names{group_combos(gc,2)});
    end
    
    save_fig(h1,fout_final);    
end


end