function LSD_dependence_check()

outDir = ['/data/cn5/caterina/Scripts/OODA/LSD_dependence_check_PDdata/'];
mkdir(outDir);

dataDir = ['/data/cn5/caterina/PDgrant/OODA_newParcelsPlus_Oct2016_0.00subt/'];
load([dataDir 'all_data_corrmat.mat']);

% get blockIds
[blockIds,blockId_labels] = getBlockIds(atlas_params);

% arrange data
subInd.CTRL = logical((tp == 1) .* strcmp(group,'CTRL')); %CTRLs, first visit data only so no repeats
subInd.PD = logical((tp == 1) .* strcmp(group,'PD') .* (CDR<1)); %PD, first visit data only, so no repeats, CDR < 1
nPD = sum(subInd.PD);
nCTRL = sum(subInd.CTRL);
all_data(:,:,1:nPD) = all_data_orig(atlas_params.sorti,atlas_params.sorti,subInd.PD);
all_data(:,:,nPD+1:nPD+nCTRL) = all_data_orig(atlas_params.sorti,atlas_params.sorti,subInd.CTRL);
groups = [ones(nPD,1); 2*ones(nCTRL,1)];
data_groups.PD = all_data(:,:,groups==1);
data_groups.CTRL = all_data(:,:,groups==2);


nsubs = [5,10,20,30,40,50,75,100];
niters = 10;
rng('shuffle');

for ns = 1:length(nsubs)

    for niter = 1:niters;
    % select subjects randomly from each group
    rindPD = randperm(nPD);
    rindCTRL = randperm(nCTRL);
    rPD = data_groups.PD(:,:,rindPD(1:nsubs(ns)));
    if nsubs(ns) < nCTRL
        rCTRL = data_groups.CTRL(:,:,rindCTRL(1:nsubs(ns)));
    else
        rCTRL = data_groups.CTRL;
    end
    all_data_SAMPLE(:,:,1:nsubs(ns)) = rPD;
    all_data_SAMPLE(:,:,nsubs(ns)+1:nsubs(ns)+size(rCTRL,3)) = rCTRL;
    groups_SAMPLE = [ones(nsubs(ns),1); 2*ones(size(rCTRL,3),1)];
    
    [M2, Score2, MatrixbyEdge2, MeanMatrixbyEdge2] = OODA_LSD_wrapper(all_data_SAMPLE,groups_SAMPLE,atlas_params.transitions, atlas_params.networks, 'TRUE', [outDir 'LSD_absolute']);
    LSDthresholds = [0.95]; %[0.99 0.975 0.95 0.9 0.85 0.5];
    for t = 1:length(LSDthresholds)
        if isnan(M2)
            total_numblocks(ns,niter,t) = nan;
        else
            passblocks = (MatrixbyEdge2 <(-1*LSDthresholds(t)) | MatrixbyEdge2 > LSDthresholds(t));
            blockIds_pass = blockIds;
            blockIds_pass(~passblocks) = 0;
            goodBlocks = unique(blockIds_pass);
            goodBlocks = goodBlocks(goodBlocks > 0); %remove the zero
            total_numblocks(ns,niter,t) = length(goodBlocks);
        end
        clear goodBlocks passblocks;
    end
    
    % because things keep erroring, so I can save some output
    save([outDir 'numblocks_tmp_ns' num2str(nsubs(ns)) '_ni' num2str(niter) '.mat'],'total_numblocks');
    
    clear all_data_SAMPLE groups_SAMPLE rindPD rindCTRL rPD rCTRL;
    end
end

mean_numblocks = squeeze(nanmean(total_numblocks,2)); % average over iterations
std_numblocks = squeeze(nanstd(total_numblocks,[],2));

figure;
plot(nsubs,mean_numblocks);
xlabel('num subs per group');
ylabel('num blocks < thresh: LSD');

figure;
shadedErrorBar(nsubs,mean_numblocks,std_numblocks);
xlabel('num subs per group');
ylabel('num blocks < thresh: LSD');


end


function [blockIds,blockId_labels] = getBlockIds(atlas_params)

blockIds = zeros(atlas_params.num_rois);
count = 1;
for m1 = 1:length(atlas_params.mods)
    for m2 = 1:length(atlas_params.mods)
        blockIds(atlas_params.mods{m1},atlas_params.mods{m2}) = count;
        blockId_labels{count} = [atlas_params.networks{m1} '-' atlas_params.networks{m2}];
        count = count+1;
    end
end

blockIds = blockIds(atlas_params.sorti,atlas_params.sorti);
blockIds = triu(blockIds,1);

end