function [Y E] = cmdscale_mat(mat,groups,dist_type,varargin)
% varargin -
%   (1) group names can be specified by a third input, e.g.
%       {'group1';'group2'} and will be displayed in a legend.
%   (2) subject names/labels (optional cell array) - use 'none' or {} to skip
%   (3) 'labels' or 'nolabels' to control dot labeling (default: 'nolabels')

numnodes = size(mat,1);
group_vals = unique(groups);
numgroups = length(group_vals);
colors = distinguishable_colors(numgroups);

dolabels = 0;
show_dot_labels = 0;

if nargin > 3 && ~isempty(varargin{1})
    dolabels = 1;
    groupnames = varargin{1};
end

if nargin > 4 && ~isempty(varargin{2})
    subject_labels = varargin{2};
end

if nargin > 5
    if strcmp(varargin{3}, 'labels')
        show_dot_labels = 1;
    end
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
hold on;

for c = 1:numgroups
    inds = groups==group_vals(c);
    plot(Y(inds,1),Y(inds,2),'.','Color',colors(c,:),'MarkerSize',20);
end

% Add subject labels if requested
if show_dot_labels && nargin > 4 && ~isempty(varargin{2})
    for s = 1:size(mat,3)
        if iscell(subject_labels)
            label_text = subject_labels{s};
        else
            label_text = subject_labels(s);
        end
        text(Y(s,1), Y(s,2), label_text, ...
            'FontSize', 8, 'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom');
    end
end

xlabel('dim1');
ylabel('dim2');
set(gca,'FontWeight','bold','FontSize',14);

if dolabels
    legend(groupnames,'FontWeight','bold','FontSize',14);
end