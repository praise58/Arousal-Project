function [r_data, manifest] = ooda_load_matrices(x, subjects, condition_name, n_nodes)
% ooda_load_matrices  Load one precomputed correlation matrix per subject.
%
%   [r_data, manifest] = ooda_load_matrices(cond_dir, subjects, condition_name, n_nodes)
%
%   cond_dir        folder holding the sub-INETxxx subfolders for ONE condition
%   subjects        string array, e.g. ["INET016"; "INET017"]
%   condition_name  label for messages/manifest, e.g. 'nsleepy'
%   n_nodes         expected matrix size (286)
%
%   r_data    n_nodes x n_nodes x nSubjects, raw r, same order as 'subjects'
%   manifest  table: subject, condition, n_frames, file

subjects = string(subjects(:));
nsub = numel(subjects);
r_data = nan(n_nodes, n_nodes, nsub);
n_frames = nan(nsub, 1);
files = strings(nsub, 1);

for s = 1:nsub
    sub_dir = fullfile(cond_dir, ['sub-' char(subjects(s))]);
    hits = dir(fullfile(sub_dir, '*_corrmat_Seitzman300.mat'));
    if numel(hits) ~= 1
        error('%s %s: expected exactly 1 corrmat .mat file in %s, found %d.', ...
            condition_name, subjects(s), sub_dir, numel(hits));
    end
    f = fullfile(hits(1).folder, hits(1).name);

    vars = whos('-file', f);
    var_names = {vars.name};
    if ~ismember('corrmat', var_names)
        error('%s %s: no variable "corrmat" in %s', condition_name, subjects(s), f);
    end

    m = load(f, 'corrmat');
    if ~isequal(size(m.corrmat), [n_nodes n_nodes])
        error('%s %s: corrmat is %dx%d, expected %dx%d (%s)', condition_name, subjects(s), ...
            size(m.corrmat, 1), size(m.corrmat, 2), n_nodes, n_nodes, f);
    end
    r_data(:, :, s) = double(m.corrmat);

    % Record how many good frames went into this matrix (quality check only).
    if ismember('tmask_concat', var_names)
        t = load(f, 'tmask_concat');
        n_frames(s) = sum(t.tmask_concat(:) ~= 0);
    end
    files(s) = string(f);
end

if any(isnan(r_data(:)))
    warning('%s: some loaded matrices contain NaN values.', condition_name);
end

manifest = table(subjects, repmat(string(condition_name), nsub, 1), n_frames, files, ...
    'VariableNames', {'subject', 'condition', 'n_frames', 'file'});

fprintf('%s: loaded %d subjects. Good frames per subject: min %g, max %g\n', ...
    condition_name, nsub, min(n_frames, [], 'omitnan'), max(n_frames, [], 'omitnan'));
end
