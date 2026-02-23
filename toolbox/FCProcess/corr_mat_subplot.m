function h = corr_mat_subplot(matrix_orig, atlas_params, ax, varargin)
% Plots a correlation matrix into a provided subplot axes. Modified from
% CG's original code for plotting adjacency matrices
% Inputs:
%   matrix_orig: the matrix that will be plotted
%   atlas_params: structure with atlas info
%   ax: axes handle where the plot should go
%   varargin: optional color limits [low, high] and title

% Extract network info
networks = atlas_params.networks;
colors_new = atlas_params.colors;

% Sort matrix
matrix = matrix_orig(atlas_params.sorti, atlas_params.sorti);

% Select axes
axes(ax);

% Plot matrix
if nargin > 3 && ~isempty(varargin{1}) && ~isempty(varargin{2})
    climlow = varargin{1};
    climhigh = varargin{2};
    imagesc(matrix, [climlow climhigh]);
else
    imagesc(matrix);
end

% Load colormap
load better_jet_colormap.mat;
colormap(ax, better_jet_colormap_diff);

% Draw lines
vline_new(atlas_params.transitions, 'k', 3);
hline_new(atlas_params.transitions, 'k', 3);

% Tick positions
tickpos = atlas_params.centers;
ax_limits = axis;

% Set ticks
set(ax, 'XTick', tickpos, 'XLim', [ax_limits(1) ax_limits(2)], ...
        'YTick', tickpos, 'YLim', [ax_limits(3) ax_limits(4)], ...
        'XTickLabel', '', 'YTickLabel', '', ...
        'FontWeight', 'bold', 'FontSize', 10);

% Label networks
tx = text(tickpos, ones(1,length(tickpos))*(atlas_params.num_rois+1), networks, ...
          'Parent', ax, 'HorizontalAlignment', 'right', 'VerticalAlignment', 'top', 'Rotation', 45);
for i = 1:length(tx)
    set(tx(i), 'Color', colors_new(i,:), 'FontName', 'Helvetica', 'FontSize', 10, 'FontWeight', 'bold');
end

ty = text(-1*ones(1,length(tickpos)), tickpos-5, networks, ...
          'Parent', ax, 'HorizontalAlignment', 'right', 'VerticalAlignment', 'top');
for i = 1:length(ty)
    set(ty(i), 'Color', colors_new(i,:), 'FontName', 'Helvetica', 'FontSize', 10, 'FontWeight', 'bold');
end

colorbar(ax);
axis(ax, 'square');

% Optional title
if nargin > 5 && ~isempty(varargin{3})
    title(varargin{3}, 'FontWeight', 'bold', 'FontSize', 10);
end

% Return handle
h = ax;
end
