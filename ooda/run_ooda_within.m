function run_ooda_within(varargin)
% run_ooda_within
%
% Within-subject OODA: Alert vs Sleepy FC for the same participants.
% Each subject contributes one paired observation (mean alert FC vs mean
% sleepy FC). Only subjects with both conditions are included.
%
% Reads QC/sleep_table_adjudicated.csv to assign per-run conditions.
% Runs PD and Control participants separately using the demographics CSV.
% TP2 rows are excluded.
%
% Rule:
%   SSS < 4 alert, SSS > 4 sleepy; if missing, bold_sleep no = alert and
%   yes/questionable = sleepy.
%
% Example:
%   run_ooda_within('getConfigPath', '/path/to/get_config.m')
%   run_ooda_within('getConfigPath', '/path/to/get_config.m', ...
%       'treeFunctionsFile', '/path/to/Tree Functions.R', ...
%       'runR', true, 'numPerms', 1000)

opts = parse_inputs(varargin{:});

script_dir = fileparts(mfilename('fullpath'));
utilities_dir = fullfile(script_dir, 'utilities');
arousal_dir = fileparts(fileparts(script_dir));
default_sleep_table = fullfile(arousal_dir, 'QC', 'sleep_table_adjudicated.csv');
default_group_table = fullfile('/projects/illinois/las/psych/cgratton/PD/Nifti/derivatives/QC', ...
    'PM_DemogClinInfo_Deidentified_06022026.csv');

if isempty(opts.sleepTable)
    opts.sleepTable = cluster_path(default_sleep_table);
end
if isempty(opts.groupTable)
    opts.groupTable = cluster_path(default_group_table);
end
if isempty(opts.outRoot)
    opts.outRoot = cluster_path(script_dir);
end
if isempty(opts.treeFunctionsFile)
    opts.treeFunctionsFile = '/projects/illinois/las/psych/cgratton/Scripts/Scripts_general/OODA/Tree Functions.R';
end
opts.sleepTable = cluster_path(opts.sleepTable);
opts.groupTable = cluster_path(opts.groupTable);
opts.outRoot = cluster_path(opts.outRoot);
opts.treeFunctionsFile = cluster_path(opts.treeFunctionsFile);

setup_config_path(opts.getConfigPath);
addpath(fileparts(script_dir));
addpath(script_dir);
addpath(utilities_dir);

if ~isfile(opts.sleepTable)
    error('Missing sleep table: %s', opts.sleepTable);
end
if ~isfile(opts.groupTable)
    error('Missing group table: %s', opts.groupTable);
end

atlas_params = atlas_parameters_GrattonLab('Seitzman300', opts.atlasDir);

sleep_table = readtable(opts.sleepTable, 'TextType', 'string');
sleep_table = normalize_sleep_table(sleep_table);
sleep_table = filter_tp1_rows(sleep_table);
sleep_table = add_group_labels(sleep_table, opts.groupTable);

rules = {
    struct('name', 'within_state_effect_pt_sss', ...
           'scoreColumn', "sleep_rating", ...
           'alertScores', 1:3, ...
           'sleepyScores', 5:7)
};

% Filter to requested rules if 'onlyRules' was specified
if ~isempty(opts.onlyRules)
    keep = false(1, numel(rules));
    for r = 1:numel(rules)
        keep(r) = any(strcmpi(opts.onlyRules, rules{r}.name));
    end
    rules = rules(keep);
    if isempty(rules)
        error('No rules matched onlyRules filter: %s', strjoin(opts.onlyRules, ', '));
    end
end

groups_to_run = ["PD", "Control"];
for g = 1:numel(groups_to_run)
    group_table = sleep_table(sleep_table.diagnosis_group == groups_to_run(g), :);
    for r = 1:numel(rules)
        run_rule(group_table, rules{r}, opts, atlas_params, groups_to_run(g));
    end
end
end

function run_rule(sleep_table, rule, opts, atlas_params, diagnosis_group)
out_dir = fullfile(opts.outRoot, sprintf('%s_%s', rule.name, char(diagnosis_group)));
if ~exist(out_dir, 'dir')
    mkdir(out_dir);
end

subjects = unique(sleep_table.subject, 'stable');
alert_data = {};
sleepy_data = {};
kept_subjects = strings(0, 1);
manifest = table();

for s = 1:numel(subjects)
    sub = subjects(s);
    cfg = get_config(char(sub));

    sub_rows = sleep_table(sleep_table.subject == sub, :);
    score_column = char(rule.scoreColumn);
    sub_rows.condition = assign_methods_condition(sub_rows, rule);
    alert_rows = sub_rows(sub_rows.condition == "alert", :);
    sleepy_rows = sub_rows(sub_rows.condition == "sleepy", :);

    if isempty(alert_rows) || isempty(sleepy_rows)
        continue
    end

    [alert_mat, alert_used] = load_mean_corrmat_for_rows(cfg, sub, alert_rows, score_column, opts.minFrames);
    [sleepy_mat, sleepy_used] = load_mean_corrmat_for_rows(cfg, sub, sleepy_rows, score_column, opts.minFrames);

    if isempty(alert_mat) || isempty(sleepy_mat)
        continue
    end

    kept_subjects(end + 1, 1) = sub; %#ok<AGROW>
    alert_data{end + 1} = alert_mat; %#ok<AGROW>
    sleepy_data{end + 1} = sleepy_mat; %#ok<AGROW>

    manifest = [manifest; make_manifest_rows(sub, "alert", alert_used); ...
                       make_manifest_rows(sub, "sleepy", sleepy_used)]; %#ok<AGROW>
end

n = numel(kept_subjects);
if n == 0
    warning('%s: no subjects had both alert and sleepy data.', rule.name);
    return
end

all_data = zeros(size(alert_data{1}, 1), size(alert_data{1}, 2), 2 * n);
for i = 1:n
    all_data(:, :, i) = alert_data{i};
    all_data(:, :, n + i) = sleepy_data{i};
end

% Preserve raw r matrices for descriptive plots. When requested, all
% inferential whole-connectome analyses use Fisher-z values.
all_data_r = all_data;
if opts.useFisherZ
    all_data = fisher_z_transform(all_data_r);
    analysis_scale = "fisher_z";
else
    analysis_scale = "raw_r";
end

groups = [ones(1, n), 2 * ones(1, n)];
group_names = {'Alert', 'Sleepy'};
alert_idx  = 1:n;
sleepy_idx = n+1:2*n;
mean_data.Alert = mean(all_data(:, :, alert_idx),  3, 'omitnan');
mean_data.Sleepy = mean(all_data(:, :, sleepy_idx), 3, 'omitnan');
mean_data.diff     = mean_data.Sleepy - mean_data.Alert;
mean_data.abs_diff = abs(mean_data.diff);

writetable(manifest, fullfile(out_dir, 'run_manifest.csv'));
save(fullfile(out_dir, 'ooda_inputs.mat'), ...
    'all_data', 'all_data_r', 'analysis_scale', 'groups', 'group_names', ...
    'kept_subjects', 'mean_data', 'rule');
writelines(analysis_scale, fullfile(out_dir, 'analysis_scale.txt'));

write_network_ttests(all_data_r, atlas_params, out_dir);
plot_figure4E_within_networks(all_data_r, atlas_params, out_dir, kept_subjects, diagnosis_group);

if opts.writeEdgeStats
    write_basic_stats(all_data_r, groups, out_dir);
end

% ----- Block permutation test (paired, sign-flip) -----
fprintf('%s: running block permutation test (%d perms)...\n', rule.name, opts.numPerms);
block_permutation_test_paired(atlas_params, all_data, mean_data, 'abs', ...
    out_dir, alert_idx, sleepy_idx, opts.numPerms, 1);
write_ooda_paired_inputs(all_data, groups, group_names, out_dir, opts);
plot_ooda_gstar_matrices(out_dir, atlas_params, analysis_scale);

fprintf('%s: kept %d paired subjects. Output: %s\n', rule.name, n, out_dir);
end

function [corrmat, used] = load_mean_corrmat_for_rows(cfg, sub, rows, score_column, min_frames)
% Concatenate masked timeseries for only the requested runs, then compute
% one corrmat from the joint data.
%
% min_frames: minimum good-frame count required; if total frames < min_frames,
%             corrmat is returned empty and a warning is printed.
if nargin < 5 || isempty(min_frames)
    min_frames = 0;
end

corrmat = [];
used    = table();
ts_all  = [];   % will be 300 x totalFrames

for i = 1:height(rows)
    ses              = rows.ses(i);
    run_num          = rows.run(i);
    sss              = rows.(score_column)(i);
    method_condition = rows.condition(i);
    bold_sleep_val   = rows.bold_sleep(i);

    [ts_masked, src, n_frames] = load_run_timeseries(cfg, sub, ses, run_num);
    if isempty(ts_masked)
        continue
    end

    ts_all = [ts_all, ts_masked]; %#ok<AGROW>
    used = [used; table(sub, ses, run_num, string(score_column), sss, ...
        string(bold_sleep_val), string(method_condition), string(src), n_frames, ...
        'VariableNames', {'subject','ses','run','score_column','sss', ...
                          'bold_sleep','method_condition','source_file','n_frames'})]; %#ok<AGROW>
end

if isempty(ts_all)
    return
end

total_frames = size(ts_all, 2);
if min_frames > 0 && total_frames < min_frames
    fprintf('  EXCLUDED %s (condition %s): only %d frames, need >= %d (%.1f min at 800ms TR)\n', ...
        char(sub), char(rows.condition(1)), total_frames, min_frames, total_frames * 0.8 / 60);
    used = table();   % clear manifest so caller knows subject was rejected
    return
end

corrmat = paircorr_mod(ts_all');
end

function condition = assign_methods_condition(rows, rule)
% Replicate methods: SSS 1-3 = alert, SSS 5-7 = sleepy, SSS 4 excluded.
% If SSS is missing, use technician notes encoded in bold_sleep.
score_column = char(rule.scoreColumn);
sss = rows.(score_column);
condition = strings(height(rows), 1);

condition(ismember(sss, rule.alertScores)) = "alert";
condition(ismember(sss, rule.sleepyScores)) = "sleepy";

missing_sss = isnan(sss);
notes = lower(strtrim(string(rows.bold_sleep)));
condition(missing_sss & notes == "no") = "alert";
condition(missing_sss & ismember(notes, ["yes", "questionable", "definitive"])) = "sleepy";
end


function rows = make_manifest_rows(sub, condition, used)
if isempty(used)
    rows = table();
    return
end
rows = used;
rows.condition = repmat(condition, height(rows), 1);
rows = movevars(rows, 'condition', 'After', 'subject');
end

function write_basic_stats(all_data, groups, out_dir)
alert = all_data(:, :, groups == 1);
sleepy = all_data(:, :, groups == 2);
nsub = size(alert, 3);

z_alert = atanh(max(min(alert, 0.999999), -0.999999));
z_sleepy = atanh(max(min(sleepy, 0.999999), -0.999999));

mean_diff = tanh(mean(z_sleepy, 3, 'omitnan')) - tanh(mean(z_alert, 3, 'omitnan'));
if nsub < 2
    tstat = nan(size(mean_diff));
    p = nan(size(mean_diff));
    warning('Only %d paired subject available; paired_edge_stats.mat will have NaN tstat/p.', nsub);
else
    [~, p, ~, stats] = ttest(z_sleepy, z_alert, 'Dim', 3);
    tstat = stats.tstat;
end

save(fullfile(out_dir, 'paired_edge_stats.mat'), 'tstat', 'p', 'mean_diff', 'nsub');
end

function write_network_ttests(all_data, atlas_params, out_dir)
% Paper-style network follow-up: paired t-tests on mean network connectivity.
% Tests are performed on Fisher-z values; table means are back-transformed r.
nsub = size(all_data, 3) / 2;
alert = all_data(:, :, 1:nsub);
sleepy = all_data(:, :, nsub + 1:end);

z_alert = atanh(max(min(alert, 0.999999), -0.999999));
z_sleepy = atanh(max(min(sleepy, 0.999999), -0.999999));

network_names = string(atlas_params.networks(:));
nnet = numel(network_names);
rows = table();

for i = 1:nnet
    for j = i:nnet
        idx_i = atlas_params.mods{i};
        idx_j = atlas_params.mods{j};
        edge_mask = false(size(all_data, 1), size(all_data, 2));
        if i == j
            edge_mask(idx_i, idx_i) = triu(true(numel(idx_i)), 1);
        else
            edge_mask(idx_i, idx_j) = true;
        end

        alert_vals = squeeze(mean(reshape(z_alert(repmat(edge_mask, 1, 1, nsub)), [], nsub), 1, 'omitnan'))';
        sleepy_vals = squeeze(mean(reshape(z_sleepy(repmat(edge_mask, 1, 1, nsub)), [], nsub), 1, 'omitnan'))';

        [~, p, ~, stats] = ttest(sleepy_vals, alert_vals);
        alert_mean = tanh(mean(alert_vals, 'omitnan'));
        sleepy_mean = tanh(mean(sleepy_vals, 'omitnan'));
        row = table( ...
            network_names(i), network_names(j), ...
            string(network_names(i) + "-" + network_names(j)), ...
            nsub, sleepy_mean, alert_mean, sleepy_mean - alert_mean, ...
            stats.tstat, stats.df, p, ...
            'VariableNames', {'network1','network2','network_pair','n', ...
                              'sleepy_mean_r','alert_mean_r','sleepy_minus_alert_r', ...
                              't','df','p'});
        rows = [rows; row]; %#ok<AGROW>
    end
end

% Benjamini-Hochberg correction across all tested network pairs.
[p_sorted, p_order] = sort(rows.p);
m_tests = height(rows);
q_sorted = min(1, flipud(cummin(flipud(p_sorted .* m_tests ./ (1:m_tests)'))));
rows.q_bh = zeros(m_tests, 1);
rows.q_bh(p_order) = q_sorted;
rows.significant_fdr05 = rows.q_bh <= 0.05;
rows = sortrows(rows, 'p', 'ascend');
writetable(rows, fullfile(out_dir, 'network_pair_paired_ttests.csv'));

figure_networks = ["Visual","Auditory","FPN","SMd","SMl","DMN"];
keep = rows.network1 == rows.network2 & ismember(rows.network1, figure_networks);
fig_rows = rows(keep, :);
[~, order] = ismember(fig_rows.network1, figure_networks);
[~, sorti] = sort(order);
fig_rows = fig_rows(sorti, :);
writetable(fig_rows, fullfile(out_dir, 'figure4E_within_network_paired_ttests.csv'));
end

function write_ooda_paired_inputs(all_data, groups, group_names, out_dir, opts)
numnodes = size(all_data, 1);
mask = triu(true(numnodes), 1);
graph_col = zeros(nnz(mask), size(all_data, 3));
for s = 1:size(all_data, 3)
    tmp = all_data(:, :, s);
    graph_col(:, s) = tmp(mask);
end

stem = fullfile(out_dir, 'ooda_paired_alert_vs_sleepy');
writematrix(graph_col, char(string(stem) + ".txt"), 'Delimiter', 'tab');

nsub = size(all_data, 3) / 2;
if rem(size(all_data, 3), 2) ~= 0 || any(groups(1:nsub) ~= 1) || any(groups(nsub + 1:end) ~= 2)
    error('Paired OODA input must be ordered Alert subjects then Sleepy subjects.');
end

fid = fopen(char(string(stem) + ".r"), 'wt');
cleanup = onCleanup(@() fclose(fid));
fprintf(fid, '.libPaths(c("~/Rlibs_R451", .libPaths()))\n');
fprintf(fid, 'set.seed(20260818)\n');
if ~isempty(opts.treeFunctionsFile)
    fprintf(fid, 'source("%s")\n', opts.treeFunctionsFile);
end
fprintf(fid, 'library(HMPTrees)\n');
fprintf(fid, 'source("%s")\n', fullfile(fileparts(mfilename('fullpath')), 'utilities', 'OODA_weighted_paired_permutation.R'));
fprintf(fid, 'data <- read.table("%s.txt")\n', stem);
fprintf(fid, 'numperms <- %d\n', opts.numPerms);
fprintf(fid, 'groupsize <- %d\n', nsub);
fprintf(fid, 'bmp("%s_mdsplot.bmp")\n', stem);
fprintf(fid, 'values <- permutationPvalue(data,numperms,groupsize,plot=TRUE,enableMC=FALSE)\n');
fprintf(fid, 'dev.off()\n');
fprintf(fid, 'write.table(values$gstarG1,"%s_gstar_alert.txt",sep="\\t",col.names=FALSE,row.names=FALSE)\n', stem);
fprintf(fid, 'write.table(values$gstarG2,"%s_gstar_sleepy.txt",sep="\\t",col.names=FALSE,row.names=FALSE)\n', stem);
fprintf(fid, 'write.table(values$pvalue,"%s_pvalue.txt",sep="\\t",col.names=FALSE,row.names=FALSE)\n', stem);
clear cleanup

if opts.runR
    if nsub < 2
        warning('Only %d paired subject available; wrote R script but skipped R CMD BATCH.', nsub);
        return
    end
    if isempty(opts.treeFunctionsFile)
        warning('treeFunctionsFile was not provided; generated R script may fail if tree helpers are not already available.');
    end
    cmd = sprintf('env -u LD_LIBRARY_PATH /usr/bin/R CMD BATCH "%s.r" "%s.Rout"', stem, stem);
    status = system(cmd);
    if status ~= 0
        warning('R CMD BATCH failed for %s.r', stem);
    end
end
end

function T = normalize_sleep_table(T)
required = ["subject", "ses", "run", "bold_sleep", "pt_sss", "obs_sss"];
for i = 1:numel(required)
    if ~ismember(required(i), string(T.Properties.VariableNames))
        error('sleep table missing required column: %s', required(i));
    end
end

T.subject = upper(strtrim(string(T.subject)));
T.bold_sleep = lower(strtrim(string(T.bold_sleep)));
T.ses = parse_numeric_column(T.ses);
T.run = parse_numeric_column(T.run);
T.pt_sss = parse_numeric_column(T.pt_sss);
T.obs_sss = parse_numeric_column(T.obs_sss);
if ismember("sleep_rating", string(T.Properties.VariableNames))
    T.sleep_rating = parse_numeric_column(T.sleep_rating);
else
    T.sleep_rating = T.pt_sss;
end
T = T(~isnan(T.ses) & ~isnan(T.run), :);
T.ses = double(T.ses);
T.run = double(T.run);
T.pt_sss = double(T.pt_sss);
T.obs_sss = double(T.obs_sss);
T.sleep_rating = double(T.sleep_rating);
end

function T = filter_tp1_rows(T)
% Exclude any TP2 subject/session rows before PD filtering and OODA grouping.
before_n = height(T);
before_sub = numel(unique(T.subject));

keep = ~endsWith(upper(string(T.subject)), "TP2");
if ismember("redcap_event_name", string(T.Properties.VariableNames))
    keep = keep & ~contains(lower(string(T.redcap_event_name)), "tp2");
end
if ismember("BIDS_session", string(T.Properties.VariableNames))
    keep = keep & ~contains(lower(string(T.BIDS_session)), "tp2");
end
if ismember("session", string(T.Properties.VariableNames))
    keep = keep & ~contains(lower(string(T.session)), "tp2");
end

T = T(keep, :);
fprintf('TP1 filter: kept %d/%d rows and %d/%d subjects\n', ...
    height(T), before_n, numel(unique(T.subject)), before_sub);
end

function T = add_group_labels(T, group_table_file)
% Keep PD and Control participants and add diagnosis_group.
try
    G = readtable(group_table_file, 'TextType', 'string', 'VariableNamingRule', 'preserve');
catch
    G = readtable(group_table_file, 'TextType', 'string');
end

names = string(G.Properties.VariableNames);
id_col = find(strcmpi(names, "participant_ID"), 1);
dx_col = find(strcmpi(names, "neurological diagnosis"), 1);
if isempty(id_col) || isempty(dx_col)
    error('Group table must contain participant_ID and neurological diagnosis columns: %s', group_table_file);
end

group_subject = normalize_pm_id(G.(names(id_col)));
dx = upper(strtrim(string(G.(names(dx_col)))));
is_pd = ismember(dx, ["PD", "IPD"]);
is_ctrl = ismember(dx, ["CONTROL", "CTRL", "HC", "HEALTHY CONTROL"]);

group_labels = strings(size(group_subject));
group_labels(is_pd) = "PD";
group_labels(is_ctrl) = "Control";

before_n = height(T);
before_sub = numel(unique(T.subject));
keep_subjects = group_subject(is_pd | is_ctrl);
T = T(ismember(T.subject, keep_subjects), :);
T.diagnosis_group = strings(height(T), 1);
for i = 1:height(T)
    idx = find(group_subject == T.subject(i), 1);
    if ~isempty(idx)
        T.diagnosis_group(i) = group_labels(idx);
    end
end
fprintf('PD/Control filter: kept %d/%d rows and %d/%d subjects using %s\n', ...
    height(T), before_n, numel(unique(T.subject)), before_sub, group_table_file);
end

function sub = normalize_pm_id(value)
sub = upper(regexprep(string(value), '[^A-Z0-9]', ''));
end

function x = parse_numeric_column(value)
if isnumeric(value)
    x = double(value);
else
    value = string(value);
    x = nan(size(value));
    for i = 1:numel(value)
        tok = regexp(value(i), '\d+', 'match', 'once');
        if ~isempty(tok)
            x(i) = str2double(tok);
        end
    end
end
end

function setup_config_path(get_config_path)
if ~isempty(get_config_path)
    get_config_path = cluster_path(get_config_path);
    if ~isfile(get_config_path)
        error('Missing get_config.m: %s', get_config_path);
    end
    repo_dir = fileparts(get_config_path);
    addpath_if_dir(repo_dir);
    addpath_if_dir(fullfile(repo_dir, 'QC', 'utilities'));
    addpath_if_dir(fullfile(repo_dir, 'FCProcess'));
    addpath_if_dir(fullfile(repo_dir, 'Utilities'));
end

if isempty(which('get_config'))
    error('get_config.m is not on the MATLAB path. Pass run_ooda_within(''getConfigPath'', fullfile(..., ''get_config.m'')).');
end
end

function opts = parse_inputs(varargin)
p = inputParser;
addParameter(p, 'sleepTable', '', @(x) ischar(x) || isstring(x));
addParameter(p, 'groupTable', '', @(x) ischar(x) || isstring(x));
addParameter(p, 'outRoot', '', @(x) ischar(x) || isstring(x));
addParameter(p, 'getConfigPath', '', @(x) ischar(x) || isstring(x));
addParameter(p, 'treeFunctionsFile', '', @(x) ischar(x) || isstring(x));
addParameter(p, 'atlasDir', '/projects/illinois/las/psych/cgratton/Atlases', ...
    @(x) ischar(x) || isstring(x));
addParameter(p, 'runR', false, @islogical);
addParameter(p, 'numPerms', 1000, @isnumeric);
addParameter(p, 'writeEdgeStats', false, @islogical);
addParameter(p, 'minFrames', 1500, @isnumeric);  % 20 min × 60 s ÷ 0.8 s/TR = 1500 frames
addParameter(p, 'useFisherZ', false, @islogical);
addParameter(p, 'onlyRules', {}, @(x) ischar(x)||isstring(x)||iscell(x));
parse(p, varargin{:});
opts = p.Results;
opts.sleepTable        = char(opts.sleepTable);
opts.groupTable        = char(opts.groupTable);
opts.outRoot           = char(opts.outRoot);
opts.getConfigPath     = char(opts.getConfigPath);
opts.treeFunctionsFile = char(opts.treeFunctionsFile);
opts.atlasDir          = char(opts.atlasDir);
if ischar(opts.onlyRules) || isstring(opts.onlyRules)
    opts.onlyRules = cellstr(opts.onlyRules);
end
end

function z = fisher_z_transform(r)
% Apply Fisher's atanh transform to off-diagonal correlation values.
r = max(min(double(r), 0.999999), -0.999999);
z = atanh(r);
for s = 1:size(z, 3)
    slice = z(:, :, s);
    slice(1:size(slice,1)+1:end) = 0;
    z(:, :, s) = slice;
end
end

function addpath_if_dir(p)
if exist(p, 'dir')
    addpath(p);
end
end

function p = cluster_path(p)
% Convert local SMB mount paths to the equivalent cluster path.
p = char(p);
p = strrep(p, ...
    '/Volumes/illinois-las-psych-gratton', ...
    '/projects/illinois/las/psych/cgratton');
end
