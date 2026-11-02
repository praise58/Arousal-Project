for s = 1:length(sub_fname)
    %clear sub_data
    %sub_data = load([dataDir sub_fname{s} '_NP705_canonical_expanded_seeds.txt']);
    %sub_data = load([dataDir sub_fname{s} '_seed_correlation_matrix.txt']);
    %sub_data = load([dataDir sub_fname{s} '_BigBrain264TimOrder_corr.txt']);
    if string(sub_group(s)) == 'CTRL'
        folderNew = fullfile(topDir,"ANALYSIS2_ALERT/FIRSTEP/ALL_SESS_CONCAT_2/MATS/",[sub_fname{s} '.mat']);
        sub_data = load(folderNew)
        %sub_data = load([folderNew,sub_fname{s} '.mat'],'corrmat_all');
        %sub_data = load(fullfile(topDir,"ANALYSIS2_ALERT/FIRSTEP/ALL_SESS_CONCAT2/MATS",[sub_fname{s} '.mat']));
        sub_data = sub_data.corrmat_all;
        alldata(:,:,s)= sub_data;
    elseif string(sub_group(s)) == 'PD'
        folderNew = fullfile(topDir,"ANALYSIS2_SLEEPY/FIRSTEP/ALL_SESS_CONCAT_2/MATS/",[sub_fname{s} '.mat']);
        sub_data = load(folderNew);
        sub_data = sub_data.corrmat_all;
        all_data(:,:,s) = sub_data;
        %sub_data = load(fullfile(topDir,"ANALYSIS2_SLEEPY/FIRSTEP/ALL_SESS_CONCAT2/MATS",[sub_fname{s} '.mat']))
        %sub_data = corrmat_all
    end
    
end 