
cd ('/projects/illinois/las/psych/cgratton/PD/Nifti/derivatives/Datalists/forCR/OODA_300/OODA_weighted/')
stemname = '/projects/illinois/las/psych/cgratton/PD/Nifti/derivatives/Datalists/forCR/OODA_300/OODA_weighted/weighted_CTRLvPD'
numbootstraps = 1000 ;
graphs = threshbin_mat(:,:,:); 

%sprintf('%s%s_%sv%s',outdir,fout,group_names{1},group_names{2});
numnodes = size(graphs,1);
% Write out edges from upper triangle of input matrix
mask = ones(numnodes);
mask = triu(mask,1);
for s = 1:size(graphs,3)
    temp = graphs(:,:,s);
    graph_col(:,s) = temp(logical(mask));
end

%REVERT BACK %dlmwrite([stemname '.txt'],graph_col,'\t')
writematrix(graph_col,[stemname '.txt'],"Delimiter",'tab')
%group1_num = nnz(groups==1);
%group2_num = nnz(groups==2);
%CG modified slightly so I don't need to use only groups 1 and 2 as labels
group_ids = unique(groups);
if length(group_ids) > 2 || length(group_ids) < 2
    error('incorrect number of groups');
end
group1_num = nnz(groups == group_ids(1));
group2_num = nnz(groups == group_ids(2));
sep = '\t';

% Write R script
fid = fopen([stemname '.r'],'wt');
fprintf(fid,'.libPaths(c("~/Rlibs_R451", .libPaths()))\n');
fprintf(fid,'source("%s")\n','/projects/illinois/las/psych/cgratton/Scripts/Scripts_general/OODA/Tree Functions.R');
% Load R packages
fprintf(fid,'library(HMPTrees)\n');
fprintf(fid,'library(bingat)\n');
fprintf(fid,'library(foreach)\n');
% Read in Data
fprintf(fid,'data <- read.table("%s.txt")\n',stemname);
% Read in Group labels
%fprintf(fid,'groups <- c(rep(0,%f),rep(1,%f))\n',group1_num,group2_num);
fprintf(fid,'numbootstraps <- %d\n',numbootstraps);
fprintf(fid,'mleG1 <- getMLEandLoglike(data[,1:%d])\n',group1_num);
fprintf(fid,'mleG2 <- getMLEandLoglike(data[,%d:%d])\n',group1_num+1,group2_num+group1_num);
fprintf(fid,'gstar1 <- mleG1$mleTree\n');
fprintf(fid,'gstar2 <- mleG2$mleTree\n');

%fprintf(fid,'values <- glrtPvalue(data,"adjmatrix",groups,FALSE,numbootstraps)\n');
%fprintf(fid,'write.table(values$b1.Differences,"%s_b1_diff.txt",sep="%s",col.names=FALSE,row.names=FALSE)\n',stemname,sep);
fprintf(fid,'write.table(gstar1,"%s_b0_covs0.txt",sep="%s",col.names=FALSE,row.names=FALSE)\n',stemname,sep);
fprintf(fid,'write.table(gstar2,"%s_b0b1_covs1.txt",sep="%s",col.names=FALSE,row.names=FALSE)\n',stemname,sep);
fprintf(fid,'pval <- compareTwoDataSets(data[,1:%d], data[,%d:%d],numbootstraps,enableMC=FALSE,cores=1)\n',group1_num,group1_num+1,group1_num+group2_num);
fprintf(fid,'write.table(pval,"%s_pvalue.txt",sep="%s",col.names=FALSE,row.names=FALSE)\n',stemname,sep);
%fprintf(fid,'pval <- compareTwoDataSets(data[,1:%d], data[,%d:%d],numbootstraps,enableMC=TRUE,cores=16)\n',group1_num,group1_num+1,group1_num+group2_num);
% Run R script
% Initialize module system + load R
% system('source /etc/profile.d/modules.sh; module load R/4.2.3; which R');
% evalc(['!chmod u+wrx ' stemname '.r']);
% evalc(['!R CMD BATCH ' stemname '.r']);

% Load R in MATLAB environment
%cmd = sprintf('R CMD BATCH "%s.r" "%s.Rout"', stemname, stemname);
[status, cmdout] = system('Rscript /projects/illinois/las/psych/cgratton/PD/Nifti/derivatives/Datalists/forCR/OODA_300/OODA_weighted/weighted_CTRLvPD.r')
%[status,out] = system(cmd,'-echo');
disp(status)

% So parfor doesn't crap out
ispvalfile = exist([stemname '_pvalue.txt']);
while ispvalfile == 0
    pause(10)
    ispvalfile = exist([stemname '_pvalue.txt']);
end

mask = ones(numnodes);
mask = triu(mask,1);


% b1_diff = load([stemname '_b1_diff.txt']);
% matrix=zeros(numnodes);
% matrix(logical(mask)) = b1_diff;
% b1_diff_mat = matrix+matrix'; % Re-symmetrify matrix


b0_covs0 = load([stemname '_b0_covs0.txt']);
matrix=zeros(numnodes);
matrix(logical(mask)) = b0_covs0;
group1_gstar = matrix+matrix'; % Re-symmetrify matrix

b0b1_covs1 = load([stemname '_b0b1_covs1.txt']);
matrix=zeros(numnodes);
matrix(logical(mask)) = b0b1_covs1;
group2_gstar = matrix+matrix'; % Re-symmetrify matrix

pvalue = load([stemname '_pvalue.txt']);

g1g2_diff_mat = group2_gstar-group1_gstar; % Create difference of groups graph

evalc(['!rm ' stemname '.r'])