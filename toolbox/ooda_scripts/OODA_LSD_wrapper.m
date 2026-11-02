function  [M, Score, MatrixbyEdgeFin, MeanMatrixbyEdge] = OODA_LSD_wrapper(graphs,groups,communitiesblocks, communitiesLabels, DoAbsolute, stemname)
%OODA_LSD_wrapper(mat(SI,SI,:),Groups,[find(S==2, 1, 'last'), find(S==3, 1,'last'), find(S==5, 1,'last'), find(S==7,1, 'last'), find(S==9,1, 'last'), find(S==10, 1,'last')], N(ismember(A, afile)), 'FALSE', 'test')

% remove any text file that is already labeled LSD in the directory
system(['rm ' stemname '*.txt']);

numnodes = size(graphs,1);

% Write out edges from upper triangle of input matrix
mask = ones(numnodes);
mask = tril(mask,-1);
for s = 1:size(graphs,3)
    temp = graphs(:,:,s);
    graph_col(:,s) = temp(logical(mask));
end

% number of subjects in each group (assumed to be tot/2)
%nsubXgroup = size(graphs,3)/2;
%if rem(size(graphs,3),2) ~= 0
%    error('Must have an even number of subjects, paired in each group.')
%end

%group1_num = nnz(groups==1); %CG
%group2_num = nnz(groups==2); %CG
%CG modified slightly so I don't need to use only groups 1 and 2 as labels
group_ids = unique(groups);
if length(group_ids) > 2 || length(group_ids) < 2
    error('incorrect number of groups');
end
%group1_num = nnz(groups == group_ids(1));
%group2_num = nnz(groups == group_ids(2));

dlmwrite([stemname 'group1.txt'],graph_col(:,groups==group_ids(1)),'\t');
dlmwrite([stemname 'group2.txt'],graph_col(:,groups==group_ids(2)),'\t');

% Double check that these are ordered into 2 sets and that they're the same
% size
%if length(find(diff(groups))) > 1
%    error('two sets are intermixed?');
%end



% Write R script
fid = fopen([stemname '.r'],'wt');
fprintf(fid,'library(bingat)\n');
fprintf(fid,'library(gplots)\n');
fprintf(fid,'library(agricolae)\n');
fprintf(fid,'library(HMPTrees)\n');
fprintf(fid,'library(lme4)\n');

fprintf(fid, 'setwd(".")\n');
fprintf(fid, 'source("/data/cn4/haoxin/LSA_Script/Graph Functions.R")\n');
fprintf(fid, 'source("/data/cn4/haoxin/LSA_Script/Tree Functions.R")\n');
fprintf(fid,'source("/data/cn5/caterina/Scripts/OODA/Connectome LSD Functions_NEW.R")\n');


fprintf(fid,'group1 <- read.table("%s.txt")\n',[stemname 'group1']);
fprintf(fid,'group2 <- read.table("%s.txt")\n',[stemname 'group2']);

fprintf(fid, 'gstar1 <- getMLEandLoglike(group1)$mleTree\n');
fprintf(fid, 'gstar2 <- getMLEandLoglike(group2)$mleTree\n');
sep = '\t';
fprintf(fid,'write.table(gstar1,"%s_gstarG1.txt",sep="%s",col.names=FALSE,row.names=FALSE)\n',stemname,sep);
fprintf(fid,'write.table(gstar2,"%s_gstarG2.txt",sep="%s",col.names=FALSE,row.names=FALSE)\n',stemname,sep);

%HS: the jankiest part, don't have a good way to fprintf an array of strings or integers
temp = '%d';
for i = 2:numel(communitiesblocks)
    temp = [temp, ', %d'];
end
fprintf(fid, ['blockCuts <- c(', temp,')\n'], communitiesblocks);
temp2 = '"%s"';
for i = 2:numel(communitiesLabels)
    temp2 = [temp2, ', "%s"'];
end
fprintf(fid, ['labels <- c(', temp2, ' )\n'], communitiesLabels{1:end});

%fprintf(fid, 'labels <- NULL\n');
sep = '\t';
fprintf(fid, 'doAbsolute <- %s\n', DoAbsolute);
fprintf(fid, 'blocksRes <- findBlocks(gstar1, gstar2, blockCuts, labels, doAbsolute,useLSD=TRUE)\n'); %set LSD method to false, due to bug
%fprintf(fid, 'blocksRes$lsdGroups\n');

fprintf(fid, 'write.table(blocksRes$blockByEdge,"%s_blockbyEdge.txt",sep="%s",col.names=FALSE,row.names=FALSE)\n',stemname,sep);
fprintf(fid, 'lapply(blocksRes$blockIds, write, "%s_blockIds.txt", append=TRUE, ncolumns=1000)\n', stemname);
fprintf(fid, 'lsd <- matrix(c(blocksRes$lsdGroups[[2]], blocksRes$lsdGroups[[4]]), ncol = 2)\n');
fprintf(fid, 'write.table(lsd,"%s_LSD.txt",sep="%s",col.names=FALSE,row.names=FALSE)\n',stemname,sep);
%fprintf(fid, 'blocksRes$lsdGroups\n');

%make PDF
outDir_plots = [stemname '_plots/'];
mkdir(outDir_plots);
fprintf(fid,'location <- "%s"\n',outDir_plots);
fprintf(fid, 'plotHeatmap(blocksRes,location)\n');
fprintf(fid, 'plotHeatmapBB(blocksRes,FALSE,location)\n'); %TRUE/FALSE indicates whether to reorder by clustering
fprintf(fid, 'plotBarchart(blocksRes,location)\n');
fprintf(fid, 'plotDensity(blocksRes,location)\n');
fclose(fid);

% Run R script
evalc(['!chmod u+wrx ' stemname '.r']);
evalc(['!R CMD BATCH ' stemname '.r']);

%pull out variables
try 
    [M, Score] = textread([stemname, '_LSD.txt'], '%f%f');

vecMatrixbyEdge = textread([stemname, '_blockbyEdge.txt']);
MatrixbyEdge = zeros(numnodes);
MatrixbyEdge(logical(mask)) = vecMatrixbyEdge;

BlockIds = textread([stemname, '_blockIds.txt']);
MatrixbyEdgeFin = zeros(numnodes);
for i = 1:size(BlockIds,1)
    MatrixbyEdgeFin(ismember(MatrixbyEdge, nonzeros(BlockIds(i,:)))) = sign(M(i))*(1-Score(i));
end
MatrixbyEdgeFin = MatrixbyEdgeFin + MatrixbyEdgeFin';

MeanMatrixbyEdge = zeros(numnodes);
for i = 1:size(BlockIds,1)
    MeanMatrixbyEdge(ismember(MatrixbyEdge, nonzeros(BlockIds(i,:)))) = M(i);
end
MeanMatrixbyEdge = MeanMatrixbyEdge + MeanMatrixbyEdge';

% a text file for easy naming output
fid = fopen([stemname '_LSD_table.txt'],'w');
fprintf(fid,'M\tScore\tNames\n');
for i = 1:size(BlockIds,1)
    fprintf(fid,'%.04f\t%.04f',M(i),Score(i));
    bbs = nonzeros(BlockIds(i,:));
    for bb = 1:length(bbs)
        [inda indb] = find(MatrixbyEdge == bbs(bb)); %just really care about the first index, since all should be the same
        aa = find(communitiesblocks > inda(1));
        bb = find(communitiesblocks > indb(1));
        if length(aa) == 0 % happens for the last case - prob a more elegant way to handle this
            net1 = communitiesLabels{end};
        else
            net1 = communitiesLabels{min(aa)};
        end
        if length(bb) == 0
            net2 = communitiesLabels{end};
        else
            net2 = communitiesLabels{min(bb)};
        end
        fprintf(fid,'\t%s-%s',net1,net2);
    end
    fprintf(fid,'\n');
end
fclose(fid);

catch
    warning('error on this LSD run');
    M = nan;
    Score = nan;
    MatrixbyEdgeFin = nan;
    MeanMatrixbyEdge = nan;
end

%%% Block name extraction:
% 	# Set up a id to name mapping
% 	blockByEdge3 <- blocksRes$blockByEdge
% 	for(i in 1:numBlocks)
% 		blockByEdge3[blocksRes$blockByEdge %in% blocksRes$blockIds[[i]]] <-  i
% 



%delete([stemname, '_blockbyEdge.txt'])
%delete([stemname, '_blockIds.txt'])
%delete([stemname, '_gstarG1.txt'])
%delete([stemname, '_gstarG2.txt'])
%delete([stemname, '_LSD.txt'])
%delete([stemname, '_gstarG2.txt'])




