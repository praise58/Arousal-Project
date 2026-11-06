function T = ooda_network_ttests(z_ns, z_sl, net_idx, net_names, out_dir)
% ooda_network_ttests  Paired t-tests (sleepy vs nsleepy) on mean network connectivity.
%
%   T = ooda_network_ttests(z_ns, z_sl, net_idx, net_names, out_dir)
%
%   z_ns, z_sl  n x n x nSubjects Fisher-z matrices (nsleepy, sleepy), same subject order
%   net_idx     n x 1 network number (1..nNetworks) for each node, in the matrices' node order
%   net_names   nNetworks x 1 network names
%   out_dir     results folder
%
% For each network pair (including within-network), each subject's mean edge
% value is computed in each condition, then tested with a paired t-test on
% (sleepy - nsleepy). Means are reported back-transformed to r.
% Benjamini-Hochberg FDR is applied across all network pairs.
% Needs base MATLAB only (no Statistics Toolbox).

net_names = string(net_names(:));
n_nodes = size(z_ns, 1);
nsub = size(z_ns, 3);
nnet = numel(net_names);

zs_ns = reshape(z_ns, n_nodes * n_nodes, nsub);
zs_sl = reshape(z_sl, n_nodes * n_nodes, nsub);
upper = triu(true(n_nodes), 1);

max_rows = nnet * (nnet + 1) / 2;
name1 = strings(max_rows, 1);
name2 = strings(max_rows, 1);
n_edges = nan(max_rows, 1);
mean_ns_r = nan(max_rows, 1);
mean_sl_r = nan(max_rows, 1);
tval = nan(max_rows, 1);
dfval = nan(max_rows, 1);
pval = nan(max_rows, 1);
k = 0;

for i = 1:nnet
    for j = i:nnet
        in_i = net_idx(:) == i;
        in_j = net_idx(:) == j;
        if i == j
            mask = (in_i & in_i') & upper;
        else
            mask = ((in_i & in_j') | (in_j & in_i')) & upper;
        end
        idx = find(mask(:));
        if isempty(idx)
            continue
        end

        v_ns = mean(zs_ns(idx, :), 1)';   % one value per subject
        v_sl = mean(zs_sl(idx, :), 1)';
        d = v_sl - v_ns;

        n = numel(d);
        sd = std(d);
        df = n - 1;
        if n < 2 || sd == 0
            t = NaN; p = NaN;
        else
            t = mean(d) / (sd / sqrt(n));
            p = betainc(df / (df + t^2), df / 2, 0.5);   % two-sided p, base MATLAB
        end

        k = k + 1;
        name1(k) = net_names(i);
        name2(k) = net_names(j);
        n_edges(k) = numel(idx);
        mean_ns_r(k) = tanh(mean(v_ns));
        mean_sl_r(k) = tanh(mean(v_sl));
        tval(k) = t;
        dfval(k) = df;
        pval(k) = p;
    end
end

rng_ = 1:k;
T = table(name1(rng_), name2(rng_), name1(rng_) + "-" + name2(rng_), ...
    repmat(nsub, k, 1), n_edges(rng_), mean_sl_r(rng_), mean_ns_r(rng_), ...
    mean_sl_r(rng_) - mean_ns_r(rng_), tval(rng_), dfval(rng_), pval(rng_), ...
    'VariableNames', {'network1', 'network2', 'network_pair', 'n_subjects', 'n_edges', ...
                      'sleepy_mean_r', 'nsleepy_mean_r', 'sleepy_minus_nsleepy_r', ...
                      't', 'df', 'p'});

T.q_bh = bh_fdr(T.p);
T.significant_fdr05 = T.q_bh <= 0.05;
T = sortrows(T, 'p', 'ascend');

if ~exist(out_dir, 'dir')
    mkdir(out_dir);
end
writetable(T, fullfile(out_dir, 'network_pair_paired_ttests.csv'));
fprintf('Network t-tests: %d network pairs tested, %d significant at FDR q <= 0.05.\n', ...
    height(T), nnz(T.significant_fdr05));
end

function q = bh_fdr(p)
% Benjamini-Hochberg adjusted p-values (NaNs are left as NaN).
p = p(:);
q = nan(size(p));
ok = ~isnan(p);
pv = p(ok);
m = numel(pv);
[ps, ord] = sort(pv);
qs = ps .* m ./ (1:m)';
qs = min(1, flipud(cummin(flipud(qs))));
tmp = zeros(m, 1);
tmp(ord) = qs;
q(ok) = tmp;
end
