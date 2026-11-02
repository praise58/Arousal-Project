function [Y E] = cmdscale_mat_SETS(mat,groups,dist_type,set_list,varargin)
% This function performs multi-dimensional scaling on input matrices and
% displays a 2-dimensional plot of the relative positions of the input
% matrices colored by group. MDS is performed using euclidean distance.
% Inputs:
% mat - node x node x subject array of matrices from all subjects
% groups - identifies which subject belongs to which group
% dist_type - type of distance calculation (e.g., euclidan or hamming)
% varargin -
%   (1) group names can be specified by a third input, e.g.
%       {'group1';'group2'} and will be displayed in a legend.
% Outputs:
% Y - configuration matrix
% E - eigenvalues of Y*Y'
%
% TOL, 09/14
% CG edits:
%   took out figure plotting command; do this outside this script
%   changed group indexing so it can deal with nonconsecutively numbered groups

numnodes = size(mat,1);
group_vals = unique(groups); % CG - added this so we can do non consecutive groups
numgroups = length(group_vals); 
set_vals = unique(set_list);
numsets = length(set_vals);
colors = distinguishable_colors(numsets);
shapes = {'.','*'};
colors2 = distinguishable_colors(numgroups*numsets);
if nargin > 4
    groupnames_orig = varargin{1};
end

mask = ones(numnodes);
mask = triu(mask,1);
for s = 1:size(mat,3)
    temp = mat(:,:,s);
    mat_col(:,s) = temp(logical(mask));
end

% Multi-dimensional scaling
D = pdist(double(mat_col'),dist_type);
[Y E] = cmdscale(D);

% Display result
%figure('Color','white')    % CG - assumes figure has already been made outside
%subplot(2,3,[1,2,4,5]);
title('first three dimensions');
hold;
count = 1;
for c = 1:numgroups
    for s = 1:numsets
        inds = find((groups==group_vals(c)) .* (set_list == set_vals(s))); 
        %plot(Y(inds,1),Y(inds,2),shapes{s},'Color',colors(c,:),'MarkerSize',20);
        if length(inds) > 0
            plot3(Y(inds,1),Y(inds,2),Y(inds,3),'.','Color',colors2(count,:),'MarkerSize',20);
            groupnames{count} = [groupnames_orig{c} '-' num2str(set_vals(s))];
            count = count+1;
        end
    end
end
xlabel('dim1'); ylabel('dim2'); zlabel('dim3');
if nargin > 3
    legend(groupnames);
end

%subplot(2,3,2); hold;
%count = 1;
%for c = 1:numgroups
%    for s = 1:numsets
%        inds = find((groups==group_vals(c)) .* (set_list == set_vals(s))); 
%        A = Y(inds,:);
%        sub_meanA = mean(A,1);
%        [dimvals,dimind] = sort(abs(sub_meanA));
%        bestdims(count,:) = dimind(end-1:end); % give best 2 dimensions
%        bestvals(count,:) = dimvals(end-1:end);
%        plot(abs(sub_meanA),'Color',colors2(count,:));
%        count = count+1;
%    end
%end
%plot(mean(Y,1),'k.-','LineWidth',4);
%xlabel('dimensions');
%ylabel('|across subject mean coordinate|');
%legend(groupnames);
%[aa bb] = sort(bestvals(:));

% subplot(2,3,3); hold;
% count = 1;
% for c = 1:numgroups
%     for s = 1:numsets
%         inds = find((groups==group_vals(c)) .* (set_list == set_vals(s))); 
%         if length(inds) > 0        
%             A = Y(inds,:);
%             sub_varA = var(A,0,1);
%             [dimvals,dimind] = sort(abs(sub_varA));
%             vardims(count,:) = dimind(end-1:end); % give best 2 dimensions
%             varvals(count,:) = dimvals(end-1:end);
%             plot(sub_varA,'Color',colors2(count,:));
%             count = count+1;
%         end
%     end
% end
% xlabel('dimensions');
% ylabel('|across subject coordinate variance|');
% plot(var(Y,0,1),'k.-','LineWidth',2);
% legend(groupnames);

% subplot(2,3,7);
% [aa bb] = sort(bestvals(:));
% cc = bestdims(bb);
% title('top two dimensions based on mean val (any group)');
% hold;
% count = 1;
% for c = 1:numgroups
%     for s = 1:numsets        
%         inds = find((groups==group_vals(c)) .* (set_list == set_vals(s))); 
%                 if length(inds) > 0
% 
%                     plot(Y(inds,cc(end)),Y(inds,cc(end-1)),shapes{s},'Color',colors(c,:),'MarkerSize',20);
%         %plot3(Y(inds,1),Y(inds,2),Y(inds,3),shapes{s},'Color',colors(c,:),'MarkerSize',20);
%         count = count+1;
%     end
% end
% xlabel(num2str(bb(end)));
% ylabel(num2str(bb(end-1)));
% if nargin > 3
%     legend(groupnames);
% end
% 
% 
% subplot(2,3,8);
% [aa bb] = sort(varvals(:));
% cc = vardims(bb);
% title('top two dimensions based on variance (any group)');
% hold;
% count = 1;
% for c = 1:numgroups
%     for s = 1:numsets
%         inds = find((groups==group_vals(c)) .* (set_list == set_vals(s))); 
%         plot(Y(inds,cc(end)),Y(inds,cc(end-1)),shapes{s},'Color',colors(c,:),'MarkerSize',20);
%         %plot3(Y(inds,1),Y(inds,2),Y(inds,3),shapes{s},'Color',colors(c,:),'MarkerSize',20);
%         count = count+1;
%     end
% end
% xlabel(num2str(bb(end)));
% ylabel(num2str(bb(end-1)));
% if nargin > 3
%     legend(groupnames);
% end


