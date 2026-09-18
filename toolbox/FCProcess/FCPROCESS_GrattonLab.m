function FCPROCESS_GrattonLab(file)
% This script is the fcprocessing script, originally from the Petersen
% lab, now for the Gratton lab.
% 
% We presume the BIDS file structure for fMRI data
%
% Input:
%       - file: (str), name of the file to preprocess
% 
% Output:
%       - TODO: list all the different files and outputs
%
%
% IMPORTANT: fcp_process removes the /newtargetdir/sub/sess folder when it
% begins processing a new session, so DO NOT set the outputdir to the directory
% where your 333 BOLD data exist - use a new directory, specific to FC
% data. 
%
% for now, force to always use tmask for processing, but should be able to
% change code to use 'ones' = just skip frames at the start of each run,
% but don't do scrubbing
%
% nuisance regressor toggle: 'fmriprep' or 'recalc'
% fmriprep: nuisance regressors are taken from fmriprep output
% recalc: recalculate nuisance regressors in the script (default since
% we've had intermittent issues with fmriprep global signal regressor)
%
% The processing order is:
%    demean/detrend (mask)
%    extract nuisance signals
%    multiple regression (mask)
%    *interpolate* (mask) %% this step takes a while, but faster now
%    temporal filter (butter1 filtfilt low-pass)
%    demean/detrend (mask)
%    [spatial blur (gauss_4dfp)] - NO LONGER DONE
%
% Not yet set up for computing task residuals
%
% originally written by: jdp 2/22/2012
% CG 2017: working off of T. Laumann's FCPROCESS_MSC.m version Editing to work with task residuals data
% CG 2019: editing to work at NU and with iNetworks data (rest)
%       example call: FCPROCESS_GrattonLab('EXAMPLESUB_DATALIST.xlsx','/projects/b1081/iNetworks/Nifti/derivatives/preproc_FCProc/','defaults2')
% AT 2025: add new parameters for more flexibility, e.g. choose the level
% of erosion for WM and CSF, choose the resolution to work with
% AT 2025: change so that each run is preprocessed in parallel. Left the
% run borders for historical purposes, in case someone wants to go back to
% the -per-subject prepro

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%% IMPORTANT CONFIG %%%%%%%%%%%%%%%%%%%%%%%%
if nargin==1
    sub = extractBetween(file, "sub-", "_");
    if isempty(sub)
        error('Subject ID not found in the file name.');
    end
    sub = sub{1};
    cfg = get_config(sub);

    outputDir = cfg.outputDir;
    tmasktype = cfg.tmasktype;
    space = cfg.space;
    res = cfg.res;
    GMthresh = cfg.GMthresh;
    WMthresh = cfg.WMthresh; 
    CSFthresh = cfg.CSFthresh;  
    WMerode = cfg.WMerode; 
    CSFerode = cfg.CSFerode; 
    denoise_switches = cfg.denoise_switches; 
    QCmat = cfg.QCmat; %TODO- is it needed?
    TR = cfg.TR;
    derivsDir = cfg.derivsDir;
    dropFR = cfg.dropFr;
    fmriprep = cfg.fmriprep;
    FDtype = cfg.FDtype;
    residuals = cfg.residuals;
    
    project_dir = cfg.labDir;
    singularity_cmd_start = cfg.singularity_cmd_start;
    afni_sif = cfg.afni_sif;
    templateflow_dir = cfg.templateflow_dir;
else
    error('check config file')
end

set(0, 'DefaultFigureVisible', 'off'); % puts figures in the background while running

% Add paths to toolbox we need
addpath(genpath(cfg.preproc_path));
addpath(genpath(cfg.bids_toolbox));
addpath(genpath(cfg.niftiread_toolbox));
addpath(genpath(cfg.hline_vline));
disp('toolboxes loaded')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% PREPARE THE DATA

QC.subjectID = sub; 
sess = extractBetween(file,"ses-","_");
QC.session = sess{1}; 
task = extractBetween(file,"task-","_");
QC.condition = task{1}; 
QC.TR = TR; 
QC.TRskip = dropFR; 
QC.topDir = derivsDir; 
QC.dataFolder = fmriprep; 
QC.confoundsFolder = fmriprep; 
QC.FDtype = FDtype; 
QC.space = space;
QC.res = res;
QC.GMthresh = GMthresh;
QC.WMthresh = WMthresh;
QC.CSFthresh = CSFthresh;
QC.WMerode = WMerode; 
QC.CSFerode = CSFerode;

% to address potential residuals field (to indicate residuals for task FC).
% This is a left-over from the past code. AT: I have not seen it in use
% (not even in HUBS), but I will leave it because it could become useful in
% the future.

if ~strcmp(residuals, 'None')
    QC.residuals = residuals;
else
    QC.residuals = 0; % assume these are not residuals 
end


%% CHECK TEMPORAL MASKS
fprintf('CHECKING DATA, CONFOUNDS, AND TEMPORAL MASKS EXIST\n');

% check that data, confounds, and tmask files with right names all exist

data_fstring1 = sprintf('%s/%s/sub-%s/ses-%s/func/',QC.topDir,QC.dataFolder,QC.subjectID,QC.session);
conf_fstring1 = sprintf('%s/%s/sub-%s/ses-%s/func/',QC.topDir,QC.confoundsFolder,QC.subjectID,QC.session);
all_fstring2 = extractBefore(file,"_space");

if QC.residuals == 0 % the typical case
    bolddata_fname = [data_fstring1 all_fstring2 '_space-' space '_' res '_desc-preproc_bold.nii.gz'];
else %if these are task FC that have been residualized
    bolddata_fname = [data_fstring1 all_fstring2 '_space-' space '_' res '_desc-preproc_bold_residuals.nii.gz'];
end

boldavg_fname = [conf_fstring1 all_fstring2 '_space-' space '_' res '_boldref.nii.gz']; %referent for alignment
boldmask_fname = [conf_fstring1 all_fstring2 '_space-' space '_' res '_desc-brain_mask.nii.gz']; %fmriprep mask
confounds_fname = [conf_fstring1 all_fstring2 '_desc-confounds_timeseries.tsv']; %if using the fmriprep regressor
tmask_fname = [conf_fstring1 'FD_outputs/' all_fstring2 '_desc-tmask_' QC.FDtype '.txt']; %assume this is in confounds folder

boldnii = bolddata_fname;
boldavgnii = boldavg_fname;
boldmasknii = boldmask_fname;
boldconf = confounds_fname;
boldtmask = tmask_fname;
boldmot_folder = [conf_fstring1 'FD_outputs']; % in this case, just give path/start so I can load different versions

if ~exist(bolddata_fname)
    error(['Data does not exist. Check paths and FMRIPREP output for: ' bolddata_fname]);
end

if ~exist(boldavg_fname)
    error(['Bold ref does not exist. Check paths and FMRIPREP output for: ' boldref_fname]);
end

if ~exist(boldmask_fname)
    error(['Bold mask does not exist. Check paths and FMRIPREP output for: ' boldmask_fname]);
end

if ~exist(confounds_fname)
    error(['Confounds file does not exist. Check paths and FMRIPREP output for: ' confounds_fname]);
end

if ~exist(boldmot_folder)
    error(['FD folder does not exist for: ' boldmot_folder]);
end
    

switch tmasktype
    case 'ones'
    otherwise
        if ~exist(tmask_fname)
            error(['Tmasks do not exist. Run FDcalc script for: ' tmask_fname]);
        end
end


% there is only one anatomy target across all runs and sessions
mprnii = sprintf('%s/%s/sub-%s/anat/sub-%s_space-%s_desc-preproc_T1w.nii.gz',QC.topDir,QC.dataFolder,QC.subjectID,QC.subjectID,space);

if ~exist(mprnii)
    error(['MPRAGE does not exist. Check paths and FMRIPREP output for: ' mprnii]);
end

 

%% SET SWITCHES
if strcmp(denoise_switches,'defaults2')
    switches.doregression=1;
    switches.regressiontype=1;
    switches.regress_source='calc'; %'calc' (recalc here) or 'fmriprep' (take from fmriprep output
    switches.motionestimates=2;
    switches.WM=1;
    switches.V=1;
    switches.GS=1;
    switches.dointerpolate=1;
    switches.dobandpass=1;
    switches.temporalfiltertype=3;
    switches.lopasscutoff=.08;
    switches.hipasscutoff=.009;
    switches.order=1;
    switches.doblur=0; %smooth on the surface
    switches.blurkernel=0;
    plot_silent = 1;
else
    error('no switches selected, go to source code and input them. Sorry :(') %TODO?: have them in the cfg struct
end

if switches.doblur
    blursize=2*log(2)/pi*10/switches.blurkernel;
    fprintf('gauss_4dfp set to blur at %d; default is 1.1032 for data in 222\n',blursize);
end

fprintf('\n\n\n\n*** HERE ARE YOUR SETTINGS ***:\n')
fprintf('switches.doregression (1=yes;0=no): %d\n',switches.doregression);
fprintf('switches.regressiontype (1=freesurfer): %d\n',switches.regressiontype);
fprintf('switches.regress_source : %s\n',switches.regress_source);
fprintf('switches.motionestimates (0=no; 1=R,R`; 2=FRISTON; 20=R,R`,12rand): %d\n',switches.motionestimates);
fprintf('switches.WM (1=regress;0=no): %d\n',switches.WM);
fprintf('switches.V (1=regress;0=no): %d\n',switches.V);
fprintf('switches.GS (1=regress;0=no): %d\n',switches.GS);
fprintf('switches.dointerpolate (1=yes;0=no): %d\n',switches.dointerpolate);
fprintf('switches.dobandpass (1=yes;0=no): %d\n',switches.dobandpass);
fprintf('switches.temporalfiltertype (1=lowpass;2=hipass;3=bandpass): %d\n',switches.temporalfiltertype);
fprintf('switches.lopasscutoff (in Hz; 0.08 is typical): %g\n',switches.lopasscutoff);
fprintf('switches.hipasscutoff (in Hz; 0.009 is typical): %g\n',switches.hipasscutoff);
fprintf('switches.order (1 is typical): %g\n',switches.order);
fprintf('switches.doblur (1=yes;0=no): %d\n',switches.doblur);
fprintf('switches.blurkernel (in mm; 4 is typical for data in 222): %d\n',switches.blurkernel);

%% LINKING TO BOLD DATA

% prepare output directory
if ~exist(outputDir)
    mkdir(outputDir)
end
fprintf('PREPARING OUTPUT DIRECTORIES\n');
fprintf('LINKING BOLD DATA\n');
    
% prepare target subject directory
QC.subdir_out = sprintf('%s/sub-%s/',outputDir,QC.subjectID);
if ~exist(QC.subdir_out)
    mkdir(QC.subdir_out); %make the directory, but don't remove previous if it exists as you may be running sessions/runs separately
end

% prepare target session directory
QC.sessdir_out=sprintf('%sses-%s/func/',QC.subdir_out,QC.session);
if ~exist(QC.sessdir_out) %only make it if it doesn't exist to account for running different task types
    mkdir(QC.sessdir_out);
else
    warning('Session output folder already existed; new results will be added to this folder and mixed');
end

% make links to atlas and seed data
QC.subatlasdir_out=[QC.subdir_out 'anat/']; %directory with anatomical info CG = changed to BIDS-like
if ~exist(QC.subatlasdir_out)
    mkdir(QC.subatlasdir_out);
end

% set symbolic link to MPRAGE data
tmprnii = [QC.subatlasdir_out 'sub-' QC.subjectID '_space-' space '_desc-preproc_T1w.nii.gz'];
% only 1 anatomy target per subject, so only link if not yet linked
if ~exist(tmprnii, 'file')
    system([ 'ln -s ' mprnii ' ' tmprnii]);
end


% CG: keep structure more akin to BIDS
% prepare and enter targetsubbolddir
sub_string = split(all_fstring2, "_");
if QC.residuals ~= 0
    if length(sub_string)>3
        all_fstring = sprintf('sub-%s_ses-%s_task-%s_residuals_%s',QC.subjectID,QC.session,QC.condition,sub_string{4});
    else
        all_fstring = sprintf('sub-%s_ses-%s_task-%s_residuals',QC.subjectID,QC.session,QC.condition);
    end
    QC.naming_str = all_fstring; % keep a record of this string
    QC.naming_str_allruns = sprintf('sub-%s_ses-%s_task-%s_residuals',QC.subjectID,QC.session,QC.condition);
else
    all_fstring = all_fstring2;
    QC.naming_str = all_fstring; % keep a record of this string
    QC.naming_str_allruns = sprintf('sub-%s_ses-%s_task-%s',QC.subjectID,QC.session,QC.condition);
end
 
tboldnii = [QC.sessdir_out all_fstring '_space-' space '_' res '_desc-preproc_bold.nii.gz'];
tboldavgnii = [QC.sessdir_out all_fstring '_space-' space '_' res '_boldref.nii.gz'];
tboldmasknii = [QC.sessdir_out all_fstring '_space-' space '_' res '_desc-brain_mask.nii.gz'];
tboldconf = [QC.sessdir_out all_fstring '_desc-confounds_timeseries.tsv'];
tboldmot_folder = [QC.sessdir_out 'FD_outputs'];

system(['ln -s ' boldnii ' ' tboldnii]);
system(['ln -s ' boldmasknii ' ' tboldmasknii]);
system(['ln -s ' boldconf ' ' tboldconf]);
system(['ln -s ' boldavgnii ' ' tboldavgnii]); 

% only 1 FD_outputs folder per session (not per run), so only link this if it is not yet linked. Somehow this was previously 
% creating infinite FD_outputs folders linked within each other
if ~exist(tboldmot_folder, 'dir')
    system(['ln -s ' boldmot_folder ' ' tboldmot_folder]);
end


%% CHECK FOR NUISANCE SEEDS
% CG - changing to point to ouputs to fmriprep
% May need to edit if we aren't happy with those timeseries
% CG2 - this could be where we choose to potentially load a design matrix
% as well for task data

needtostop=0;
switch switches.regressiontype 
    case {0,1,9} % freesurfer masks of WM and V
        
        % set basic names

        anat_string = [QC.topDir '/' QC.confoundsFolder '/sub-' QC.subjectID '/anat/'];
        
        % CG - usually would be a general mask across subjects, but
        % this mask below seems overly conservative. I made a less
        % conservative one by dilating this one 3x using AFNI:
        % singularity run ../singularity_images/afni_latest.sif 3dmask_tool -input tpl-MNI152NLin6Asym_res-02_desc-brain_mask.nii.gz -prefix tpl-MNI152NLin6Asym_res-02_desc-brain_mask_dilate3.nii.gz -dilate_input 3
        QC.GLMmaskfile = [templateflow_dir 'tpl-' space '/tpl-' space '_' res '_desc-brain_mask_dilate3.nii.gz']; %CG = primary mask we will use
        %This mask is pre-made, so no this line is just reading it

        % need to resample the maskfiles to the correct resolution space 
        QC.WMmaskfile = [anat_string 'sub-' QC.subjectID '_space-' space '_label-WM_probseg_' num2str(WMthresh) 'mask_' res '_ero' WMerode '.nii.gz']; %AD - replacing probseg file with output of make_fs_masks.m 
        QC.CSFmaskfile = [anat_string 'sub-' QC.subjectID '_space-' space '_label-CSF_probseg_' num2str(CSFthresh) 'mask_' res '_ero' CSFerode '.nii.gz']; %JC - replacing CSF probseg file with output of make_fs_masks.m 
        QC.WBmaskfile = [anat_string 'sub-' QC.subjectID '_space-' space '_' res '_desc-brain_mask.nii.gz']; 
        QC.GREYmaskfile = [anat_string 'sub-' QC.subjectID '_space-' space '_label-GM_probseg_' num2str(GMthresh) 'mask.nii.gz'];
            
        % check for existence of mask files
        fprintf('CHECKING NUISANCE SEEDS\t%d\t%s\n',i,QC.subjectID);
        disp('remember to visually check if nuisance masks at set thresholds look OK.');
        needtostop=0;
        if ~exist([QC.GLMmaskfile])
            fprintf('No GLMmask found: %s\n',QC.GLMmaskfile);
            needtostop=1;
        end            
        if ~exist(QC.WBmaskfile)
            fprintf('WBmaskfile: %s missing!\n',QC.WBmaskfile);
            needtostop=1;
        end
        if ~exist(QC.GREYmaskfile)
            fprintf('GREYmaskfile: %s missing!\n',QC.GREYmaskfile);
            needtostop=1;
        end
        if ~exist(QC.WMmaskfile)
            fprintf('WMmaskfile: %s missing! Check make_fs_masks output.\n',QC.WMmaskfile);
            needtostop=1;
        end
        if ~exist(QC.CSFmaskfile)
            fprintf('CSFmaskfile: %s missing!\n',QC.CSFmaskfile);
            needtostop=1;
        end
        if needtostop
            error('Fix the BRAIN masks.\n');
        end
        
        % ensure masks contain something, relax erosions if not
        tmpmask=load_nii_wrapper(QC.GLMmaskfile);
        QC.GLMMASK=~~tmpmask;
        
        tmpmask = load_nii_wrapper(QC.WBmaskfile);
        QC.WBMASK=~~tmpmask;
        
        tmpmask = load_nii_wrapper(QC.GREYmaskfile);
        QC.GMMASK=~~tmpmask;
        QC.GMthresh = GMthresh;
        
        tmpmask = load_nii_wrapper(QC.WMmaskfile);
        QC.WMMASK=~~tmpmask;
        QC.WMthresh = WMthresh;
        
        tmpmask = load_nii_wrapper(QC.CSFmaskfile);
        QC.CSFMASK=~~tmpmask;
        QC.CSFthresh = CSFthresh;
        
    case 2 % TODO:external 4dfp of regressor ROIs
        
    case 3 % TODO:external txt file
        
    otherwise
end


%% CALCULATE SUBJECT MOVEMENT
    
fprintf('LOADING MOTION\tsub-%s\tsess-%s\t%s\n',QC.subjectID,QC.session,QC.naming_str);

if QC.residuals ~= 0
    % load motion and alignment estimates from FD folder
    mot_fstring = QC.naming_str;
    mvm = table2array(readtable([tboldmot_folder '/' mot_fstring '_desc-mvm.txt']));        
    mvm_filt = table2array(readtable([tboldmot_folder '/' mot_fstring '_desc-mvm_filt.txt']));
    FD = table2array(readtable([tboldmot_folder '/' mot_fstring '_desc-FD.txt']));        
    fFD = table2array(readtable([tboldmot_folder '/' mot_fstring '_desc-fFD.txt']));
else
    % load motion and alignment estimates from FD folder
    mvm = table2array(readtable([tboldmot_folder '/' QC.naming_str '_desc-mvm.txt']));        
    mvm_filt = table2array(readtable([tboldmot_folder '/' QC.naming_str '_desc-mvm_filt.txt']));
    FD = table2array(readtable([tboldmot_folder '/' QC.naming_str '_desc-FD.txt']));        
    fFD = table2array(readtable([tboldmot_folder '/' QC.naming_str '_desc-fFD.txt'])); 
end

% get diffed and detrended mvm params for nuisance regression
d = size(mvm);
ddt_mvm = [zeros(1,d(2)); diff(mvm)]; % put 0 at the start by default
mvm_detrend = demean_detrend(mvm')'; 
ddt_mvm_detrend = demean_detrend(ddt_mvm')';

ddt_mvm_filt = [zeros(1,d(2)); diff(mvm_filt)]; % put 0 at the start by default
mvm_filt_detrend = demean_detrend(mvm_filt')'; 
ddt_mvm_filt_detrend = demean_detrend(ddt_mvm_filt')';

% STORE TOTAL DATA FOR EACH FILE

% store the total movement data

QC.MVM=mvm;
QC.ddtMVM=ddt_mvm; 
QC.DTMVM=mvm_detrend; 
QC.ddtDTMVM=ddt_mvm_detrend; 
QC.FD=FD;

QC.MVM_filt=mvm_filt;
QC.ddtMVM_filt=ddt_mvm_filt; 
QC.DTMVM_filt=mvm_filt_detrend; 
QC.ddtDTMVM_filt=ddt_mvm_filt_detrend; 
QC.fFD=fFD;

QC.switches=switches;

%% DEFINE RUN BORDERS
trpos=0;
tr.tot=numel(QC.FD);
tr.runtrs=numel(FD);
tr.start=[trpos+1 trpos+tr.runtrs];
QC.runborders = [1 tr.start(1,1:2)]; %AT: This is not really needed, but I'll leave it here for historical reasons. 
%If  at any point, someone would like to go back to preprocess all the session from a
%subject at once, then this will be useful.
%dlmwrite([QC.sessdir_out 'runborders.txt'],QC.runborders,'\t'); 

%% ASSEMBLE TEMPORAL MASKS
switch tmasktype
    case 'ones'
        QC.runtmask=ones(size(FD,1),1);
        QC.runtmask(1:QC.TRskip)=0;
        QC.tmask=QC.runtmask;
    otherwise
        fprintf('GETTING TMASK FILES\t%s',QC.naming_str);
        QC.runtmask=table2array(readtable(boldtmask));
        QC.tmask=QC.runtmask; %AT:silly, but avoids changing variables downstream
end

%% FUNCTIONAL CONNECTIVITY PROCESSING -- DENOISING

bigstuff=1; % this saves voxelwise timecourses over processing.
skipvox=15; % downsample grey matter voxels for visuals.
set(0, 'DefaultFigureVisible', 'off');
    
fprintf('FCPROCESSING FILE %s\n',file);

%Select voxels in glmmask
QC.CSFMASK_glmmask = QC.CSFMASK(logical(QC.GLMMASK));
QC.WMMASK_glmmask = QC.WMMASK(logical(QC.GLMMASK));
QC.GMMASK_glmmask = QC.GMMASK(logical(QC.GLMMASK));
QC.WBMASK_glmmask = QC.WBMASK(logical(QC.GLMMASK));
QC.GLMMASK_glmmask = QC.GLMMASK(logical(QC.GLMMASK));

%%%
% THE PROCESSING BEGINS
%%%

stage=1;
ending= 'fmriprep'; 
allends = ending;
bolds = [];

LASTIMG{1,stage} = tboldnii(1:end-7); %remove .nii.gz
bolds = tboldnii(1:end-7);


% obtain the raw images (and mode 1000 normalize them)
tempimg = bolds2mat(bolds,tr.tot,tr.start,QC.GLMMASK,QC.WBMASK);

% save out average raw image for SNR mask later
tempimg_avg = zeros(size(QC.GLMMASK));
tempimg_avg(logical(QC.GLMMASK)) = squeeze(mean(tempimg,2));
outSNR = [QC.sessdir_out QC.naming_str '_desc-mode1000_mean.nii.gz']; %AT: SNR per image
outfile = load_nii([bolds '.nii.gz']); % for header info
img_dims = size(outfile.img);
img_dims(4) = 1; % this is only a mask, no temporal data
outfile.img = reshape(tempimg_avg,img_dims);
outfile.prefix = outSNR;   
outfile.hdr.dime.dim(2:5) = img_dims;
save_nii(outfile,outSNR);
    
QC = nuissignals(QC,tboldconf);

QC.process{stage}=ending;

if bigstuff
    tmptcs=single(tempimg(QC.GMMASK_glmmask,:));
    QC.GMtcs(:,:,stage)=tmptcs(1:skipvox:end,:);
    QC.WMtcs(:,:,stage)=single(tempimg(QC.WMMASK_glmmask,:));
    QC.CSFtcs(:,:,stage)=single(tempimg(QC.CSFMASK_glmmask,:));
end

makepictures_vCG(QC,stage,[700:200:1300],[0:50:100],200);    
saveas(gcf,[QC.sessdir_out QC.naming_str '_stage-' num2str(stage) '-' allends '.tiff'],'tiff');
close(gcf);


%%%%%%%%%%%%%%%%%%%%%%%%
%%% 0-mean, detrend %%%
%%%%%%%%%%%%%%%%%%%%%%%%

stage=stage+1;
ending='zmdt';
allends=[allends '_' ending];

LASTIMG{1,stage}=[ LASTIMG{1,stage-1} '_' ending ];

% for each BOLD run
fprintf('\tDEMEAN DETREND\t%s\n',QC.naming_str);
temprun=tempimg(:,QC.runborders(1,2):QC.runborders(1,3)); %AT: temprun=tempimg; see comment about historical reasons
temprun=demean_detrend(temprun,QC.runtmask);
tempimg(:,QC.runborders(1,2):QC.runborders(1,3))=temprun;

QC.process{stage}=ending;
if bigstuff
    tmptcs=single(tempimg(QC.GMMASK_glmmask,:));
    QC.GMtcs(:,:,stage)=tmptcs(1:skipvox:end,:);
    QC.WMtcs(:,:,stage)=single(tempimg(QC.WMMASK_glmmask,:));
    QC.CSFtcs(:,:,stage)=single(tempimg(QC.CSFMASK_glmmask,:));
end

makepictures_vCG(QC,stage,[-20:20:20],[0:50:100],200);    
saveas(gcf,[QC.sessdir_out QC.naming_str '_stage-' num2str(stage) '-' allends '.tiff'],'tiff');
close(gcf);

%%%%%%%%%%%%%%%%%%%%%%%%
%%% MULTIPLE REGRESSION %%%
%%%%%%%%%%%%%%%%%%%%%%%%

% load the image in question, including all BOLD runs
fprintf('\tNUISANCE REGRESSION\n');
stage=stage+1;
ending='resid';
allends=[allends '_' ending];
LASTIMG{1,stage}=[ LASTIMG{1,stage-1} '_' ending ];

if switches.doregression
    
    % get the movement-based regressors
    switch switches.motionestimates
        case 0 %
            QC.mvmregs=[];
            QC.mvmlabels={''};
        case 1 % R,R`                   LAB CLASSIC
            QC.mvmregs=[QC.DTMVM QC.ddtDTMVM];
            QC.mvmlabels={'trans_x','trans_y','trans_z','rot_x','rot_y','rot_z',...
                'trans_x_ddt','trans_y_ddt','trans_z_ddt','rot_x_ddt','rot_y_ddt','rot_z_ddt'};
        case 2 % R,R^2,R-1,R-1^2       FRISTON
            frist1=circshift(QC.DTMVM,[1 0]);
            frist1(1,:)=0;
            QC.mvmregs=[QC.DTMVM (QC.DTMVM.^2) frist1 frist1.^2 ];
            QC.mvmlabels={'X','Y','Z','rot_x','rot_y','rot_z',...
                'sqrX','sqrY','sqrZ','sqrrot_x','sqrrot_y','sqrrot_z',...
                'Xt-1','Yt-1','Zt-1','rot_xz-1','rot_yz-1','rot_zz-1',...
                'sqrXt-1','sqrYt-1','sqrZt-1','sqrrot_xt-1','sqrrot_yt-1','sqrrot_zt-1'};
    end
    
    
    % get the signal regressors
    QC.sigregs=[];
    QC.siglabels=[];
    switch switches.regressiontype
        case {0,1}
            if switches.GS
                if strcmp(switches.regress_source,'fmriprep')
                    sig = QC.global_signal;
                elseif strcmp(switches.regress_source,'calc')
                    sig = mean(tempimg(QC.GLMMASK_glmmask,:))';
                else
                    error('do no recognize regress_source switch');
                end
                QC.sigregs=[QC.sigregs sig];
                QC.siglabels=[QC.siglabels {'WB'}];
            end
            if switches.WM
                if strcmp(switches.regress_source,'fmriprep')
                    sig = QC.white_matter;
                elseif strcmp(switches.regress_source,'calc')
                    sig=mean(tempimg(QC.WMMASK_glmmask,:))';
                else
                    error('do no recognize regress_source switch');
                end
                    QC.sigregs=[QC.sigregs sig];
                    QC.siglabels=[QC.siglabels {'WM'}];
                
            end
            if switches.V
                if strcmp(switches.regress_source,'fmriprep')
                    sig = QC.csf;
                elseif strcmp(switches.regress_source,'calc')
                    sig=mean(tempimg(QC.CSFMASK_glmmask,:))';
                else
                    error('do no recognize regress_source switch');
                end
                    QC.sigregs=[QC.sigregs sig];
                    QC.siglabels=[QC.siglabels {'V'}];
            end
            if ~isempty(QC.sigregs)
                QC.sigregs=[QC.sigregs [repmat(0,[1 size(QC.sigregs,2)]); diff(QC.sigregs)]];
                kk=numel(QC.siglabels);
                for k=1:kk
                    QC.siglabels{k+kk}=[ QC.siglabels{k} '`'];
                end
            end
    end
    
    QC.nuisanceregressors=[QC.mvmregs QC.sigregs];
    QC.nuisanceregressorlabels=[QC.mvmlabels QC.siglabels];
    dlmwrite([QC.sessdir_out QC.naming_str '_total_nuisance_regressors.txt'],QC.nuisanceregressors,'\t');
    
    figure('Visible','Off');
    subplot(8,1,8);
    imagesc(zscore(QC.nuisanceregressors)',[-2 2]); ylabel('REGS');
    saveas(gcf,[QC.sessdir_out QC.naming_str '_total_nuisance_regressors.tiff'],'tiff');
    
    % write the correlations of the nuisance regressors
    clf;
    h=imagesc(triu(corrcoef(QC.nuisanceregressors),1),[-.5 1]);
    colorbar;
    saveas(gcf,[QC.sessdir_out QC.naming_str '_total_nuisance_regressors_correlations.tiff'],'tiff');
    close;
    dlmwrite([QC.sessdir_out QC.naming_str '_total_nuisance_regressors_correlations.txt'],corrcoef(QC.nuisanceregressors),'\t');
    close;
    
    [tempimg zb regsz]=regress_nuisance(tempimg,QC.nuisanceregressors,QC.tmask);
    
    QC.nuisanceregressors_ZSCORE=regsz;
    
    QC.process{stage}=ending;
    
    if bigstuff
        tmptcs=single(tempimg(QC.GMMASK_glmmask,:));
        QC.GMtcs(:,:,stage)=tmptcs(1:skipvox:end,:);
        QC.WMtcs(:,:,stage)=single(tempimg(QC.WMMASK_glmmask,:));
        QC.CSFtcs(:,:,stage)=single(tempimg(QC.CSFMASK_glmmask,:));
    end
    makepictures_vCG(QC,stage,[-20:20:20],[0:50:100],200);    
    saveas(gcf,[QC.sessdir_out QC.naming_str '_stage-' num2str(stage) '-' allends '.tiff'],'tiff');
    close(gcf);
    
end

%%%%%%%%%%%%%%%%%%%%%%%%
% INTERPOLATION
%%%%%%%%%%%%%%%%%%%%%%%%

if switches.dointerpolate
    
    stage=stage+1;
    ending='ntrpl';
    allends=[allends '_' ending];
    
    LASTIMG{1,stage}=[ LASTIMG{1,stage-1} '_' ending ];
    
    % for each BOLD run
    fprintf('\tINTERPOLATE\t%s\n',QC.naming_str);

    temprun=tempimg(:,QC.runborders(1,2):QC.runborders(1,3));
    ofac=8;
    hifac=1;
    TRtimes=([1:size(temprun,2)]')*QC.TR;

    if numel(TRtimes)<150
        voxbinsize=5000;
    elseif (numel(TRtimes)>=150 && numel(TRtimes)<500)
        voxbinsize=500;
    elseif numel(TRtimes)>=500
        voxbinsize=50;
    end
    fprintf('INTERPOLATION VOXBINSIZE: %d\n',voxbinsize);
    voxbin=1:voxbinsize:size(temprun,1);
    voxbin=[voxbin size(temprun,1)];

    temprun=temprun';
    tempanish=zeros(size(temprun,1),size(temprun,2));

    % gotta bin by voxels: 5K is ~15GB, 15K is ~40GB at standard
    % run lengths. 5K is ~15% slower but saves 2/3 RAM, so that's
    % the call.
    % CG: could consider adding parfor loops here
    for v=1:numel(voxbin)-1 % this takes huge RAM if all voxels
%<BAS> Added code from Gaurav Patel's coding wiz: speeds up interpolation
%      25x. Answer is the same as the original (getTransform) within
%      rounding error.
        tempanish(:,voxbin(v):voxbin(v+1))=LSTransform(TRtimes(~~QC.runtmask),temprun(~~QC.runtmask,voxbin(v):voxbin(v+1)),TRtimes,QC.TR,ofac,hifac);
%</BAS>
    end

    tempanish=tempanish';
    temprun=temprun';

    temprun(:,~QC.runtmask)=tempanish(:,~QC.runtmask);
    tempimg(:,QC.runborders(1,2):QC.runborders(1,3))=temprun;

    
    QC.process{stage}=ending;
    
    if bigstuff
        tmptcs=single(tempimg(QC.GMMASK_glmmask,:));
        QC.GMtcs(:,:,stage)=tmptcs(1:skipvox:end,:);
        QC.WMtcs(:,:,stage)=single(tempimg(QC.WMMASK_glmmask,:));
        QC.CSFtcs(:,:,stage)=single(tempimg(QC.CSFMASK_glmmask,:));
    end
    
    makepictures_vCG(QC,stage,[-20:20:20],[0:50:100],200);    
    saveas(gcf,[QC.sessdir_out QC.naming_str '_stage-' num2str(stage) '-' allends '.tiff'],'tiff');
    close(gcf);
end

%%%%%%%%%%%%%%%%%%%%%%%%
%%% TEMPORAL FILTER %%%
%%%%%%%%%%%%%%%%%%%%%%%%

if switches.dobandpass
    
    stage=stage+1;
    ending='bpss';
    allends=[allends '_' ending];

    LASTIMG{1,stage}=[ LASTIMG{1,stage-1} '_' ending ];
    
    filtorder=switches.order;
    switch switches.temporalfiltertype
        case 1
            lopasscutoff=switches.lopasscutoff/(0.5/QC.TR); % since TRs vary have to recalc each time
            [butta buttb]=butter(filtorder,lopasscutoff,'low');
        case 2
            hipasscutoff=switches.hipasscutoff/(0.5/QC.TR); % since TRs vary have to recalc each time
            [butta buttb]=butter(filtorder,hipasscutoff,'high');
        case 3
            lopasscutoff=switches.lopasscutoff/(0.5/QC.TR); % since TRs vary have to recalc each time
            hipasscutoff=switches.hipasscutoff/(0.5/QC.TR); % since TRs vary have to recalc each time
            [butta buttb]=butter(filtorder,[hipasscutoff lopasscutoff]);
    end
    
    aa = tempimg;
    fprintf('\tTEMPORAL FILTER\t%s\n',QC.naming_str);
    tic;
    temprun=tempimg(:,QC.runborders(1,2):QC.runborders(1,3));
    temprun=temprun';
    size_temprun = size(temprun);
    pad = 1000;
    temprun = cat(1, zeros(pad, size_temprun(2)), temprun, zeros(pad, size_temprun(2))); % AT: This is the the most RAM-consuming part
    temprun(isnan(temprun)) = 0; % added per AD to fix crash
    [temprun]=filtfilt(butta,buttb,double(temprun));
    temprun = temprun(pad+1:end-pad, 1:size_temprun(2));
    temprun=temprun';
    tempimg(:,QC.runborders(1,2):QC.runborders(1,3))=temprun;
    toc;
    
    QC.process{stage}=ending;
    
    if bigstuff
        tmptcs=single(tempimg(QC.GMMASK_glmmask,:));
        QC.GMtcs(:,:,stage)=tmptcs(1:skipvox:end,:);
        QC.WMtcs(:,:,stage)=single(tempimg(QC.WMMASK_glmmask,:));
        QC.CSFtcs(:,:,stage)=single(tempimg(QC.CSFMASK_glmmask,:));
    end
    
    makepictures_vCG(QC,stage,[-20:20:20],[0:50:100],200);    
    saveas(gcf,[QC.sessdir_out QC.naming_str '_stage-' num2str(stage) '-' allends '.tiff'],'tiff');
    close(gcf);
    
    % create temporal mask based on filter properties - 08/2022 no longer needed, just keeping in QC.mat
    filtertrim=15; % TRs at beginning and end of run to ignore due to IIR zero-phase filter
    QC.filtertmask=[];
    
    QC.runfiltertmask=QC.runtmask;
    QC.runfiltertmask(1:filtertrim)=0;
    QC.runfiltertmask(end-filtertrim+1:end)=0;
    QC.runfiltertmask=QC.runfiltertmask & QC.runtmask;
    QC.filtertmask=[QC.filtertmask; QC.runfiltertmask];
    
end

%%%%%%%%%%%%%%%%%%%%%%%%
%%% 0-mean, detrend %%%
%%%%%%%%%%%%%%%%%%%%%%%%

stage=stage+1;
ending='zmdt';
allends=[allends '_' ending];

LASTIMG{1,stage}=[ LASTIMG{1,stage-1} '_' ending ];


% for each BOLD run
fprintf('\tDEMEAN DETREND\t%s\n',QC.naming_str);
temprun=tempimg(:,QC.runborders(1,2):QC.runborders(1,3));
if switches.dobandpass
    temprun=demean_detrend(temprun,QC.runfiltertmask);
else
    temprun=demean_detrend(temprun,QC.runtmask);
end
tempimg(:,QC.runborders(1,2):QC.runborders(1,3))=temprun;

QC.process{stage}=ending;

if bigstuff
    tmptcs=single(tempimg(QC.GMMASK_glmmask,:));
    QC.GMtcs(:,:,stage)=tmptcs(1:skipvox:end,:);
    QC.WMtcs(:,:,stage)=single(tempimg(QC.WMMASK_glmmask,:));
    QC.CSFtcs(:,:,stage)=single(tempimg(QC.CSFMASK_glmmask,:));
end

makepictures_vCG(QC,stage,[-20:20:20],[0:50:100],200);    
saveas(gcf,[QC.sessdir_out QC.naming_str '_stage-' num2str(stage) '-' allends '.tiff'],'tiff');
close(gcf);

%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPATIAL BLUR %%%
%%%%%%%%%%%%%%%%%%%%%%%%

% CG has commented the code out and frankly, we do not do that, so for the
% code's sake AT deleted it. If you needed, go back in history to 2024 GIT
% commits

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CONCATENATE INTO SINGLE IMAGE %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%


fprintf('\tCONCATENATE AND CLEANUP\n');

tempimg_out = zeros(size(QC.GLMMASK,1),size(tempimg,2)); %Put back in volume space
tempimg_out(logical(QC.GLMMASK),:) = tempimg;
tmpavg = load_nii(tboldavgnii); 
d = size(tmpavg.img);
dims_bold = [d(1) d(2) d(3) size(tempimg,2)];
tempimg_out = reshape(tempimg_out,dims_bold);
clear tempimg tmpavg d;

outdat = load_nii(tboldnii);
outdat.img = tempimg_out(:,:,:,tr.start(1,1):tr.start(1,2));
out_fname = [QC.sessdir_out QC.naming_str '_' allends '.nii.gz'];
outdat.fileprefix = out_fname;
save_nii(outdat,out_fname);
clear outdat;

% save QC file per session
if cfg.QCmat == 1
    QC_outname = [QC.sessdir_out QC.naming_str '_QC.mat'];
    QCsub = QC;
    save(QC_outname,'QCsub','-v7.3');
end
clear QCsub;

fprintf('\tFCPROC finished!\n');


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [fcimg]=bolds2mat(bolds,trtot,trborders,GLMmask,WBmask_sub)

vox = nnz(GLMmask);
fcimg=zeros(vox,trtot);

temp = load_nii_wrapper([bolds '.nii.gz']);

%CG added, based on BK code, based on EMG code
temp1000 = mode1000norm(temp,WBmask_sub); % use the more sub specific mask for this

fcimg(:,trborders(1,1):trborders(1,2))=temp1000(logical(GLMmask),:);
clear temp temp1000;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function bolddat1000 = mode1000norm(bolddat,bmask)

bolddat_masked = double(bolddat(bmask,:));
bolddat_masked = bolddat_masked(bolddat_masked > 0); %note: EMG code had an additional mask > 100 applied. Took out since didn't seem needed?
[counts,edges] = histcounts(bolddat_masked,1000);
[~,maxind] = max(counts);

upper_75 = prctile(bolddat_masked, 75);%upper_75 = edges(maxind+250); %since 1000 bins  %
lower_25 = prctile(bolddat_masked, 25); %lower_25 = edges(maxind-250); %%

% add a range normalization step for NU to make it look more like MSC
bolddat_norm = (bolddat - lower_25)/(upper_75 - lower_25) .* 200; %MSC range seemed ~between 900 and 1200

% recalculate mode after normalization
bolddat_norm_masked = double(bolddat_norm(bmask,:));
bolddat_norm_masked = bolddat_norm_masked(bolddat_masked > 0); % use original mask for 0s
[counts,edges] = histcounts(bolddat_norm_masked,1000);
[~,maxind] = max(counts);
modeval = mean([edges(maxind) edges(maxind+1)]);

% change bold data to have mode 1000
bolddat1000 = bolddat_norm + (1000 - modeval);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [tempbold tempbetas] = demean_detrend(img,varargin)

if ~isnumeric(img)
    %[tempbold]=read_4dfpimg_HCP(img); % read image
    tempbold = load_nii_wrapper(img);
    %tempbold = tempbold.img;
else
    [tempbold]=img;
    clear img;
end
[vox ts]=size(tempbold);

if ~isempty(varargin)
    tmask=varargin{1,1};
else
    tmask=ones(ts,1);
end

linreg=[repmat(1,[ts 1]) linspace(0,1,ts)'];
tempboldcell=num2cell(tempbold(:,logical(tmask))',1);
linregcell=repmat({linreg(logical(tmask),:)},[1 vox]);
tempbetas = cellfun(@mldivide,linregcell,tempboldcell,'uniformoutput',0);
tempbetas=cell2mat(tempbetas);
tempbetas=tempbetas';
tempintvals=tempbetas*linreg';
tempbold=tempbold-tempintvals;

if nargin==3
    outname=varargin{1,2};
    error('not changed to nii yet'); % CG added
    write_4dfpimg(tempbold,outname,'bigendian');
    write_4dfpifh(outname,size(tempbold,2),'bigendian');
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [tempimg zb newregs] = regress_nuisance(tempimg,totregs,tot_tmask)

[vox ts]=size(tempimg);
zlinreg=totregs(logical(tot_tmask),:); % only desired data
[zlinreg DMDTB]=demean_detrend(zlinreg'); % obtain fits for desired data
zlinreg=zlinreg';
zstd=std(zlinreg); % calculate std
zmean=mean(zlinreg);
zlinreg=(zlinreg-repmat(zmean,[size(zlinreg,1) 1]))./(repmat(zstd,[size(zlinreg,1) 1])); % zscore

linreg=[repmat(1,[ts 1]) linspace(0,1,ts)'];
newregs=DMDTB*linreg'; % predicted all regressors demean/detrend
newregs=totregs-newregs'; % these are the demeaned detrended regressors
newregs=(newregs-repmat(zmean,[size(newregs,1) 1]))./(repmat(zstd,[size(newregs,1) 1])); % zscore

% now we have z-scored, detrended good and all regressors.

% demean and detrend the desired data
zmdtimg=tempimg(:,logical(tot_tmask));
[zmdtimg zmdtbetas]=demean_detrend(zmdtimg);

% calculate betas on the good data
tempboldcell=num2cell(zmdtimg',1);
zlinregcell=repmat({zlinreg},[1 vox]);
zb = cellfun(@mldivide,zlinregcell,tempboldcell,'uniformoutput',0);
zb=cell2mat(zb);

% demean and detrend all data using good fits
[zmdttotimg]=zmdtbetas*linreg';
zmdttotimg=tempimg-zmdttotimg;

% calculate residuals on all the data
zb=zb';
tempintvals=zb*newregs';
tempimg=zmdttotimg-tempintvals;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function QC = nuissignals(QC,tboldconf)
% CG - added in to draw confound signals from fmriprep output

% relevant confounds to carry forward:
conf_names = {'csf','csf_derivative1','csf_power2','csf_derivative1_power2',...
    'white_matter','white_matter_derivative1','white_matter_power2','white_matter_derivative1_power2',...
    'global_signal','global_signal_derivative1','global_signal_power2','global_signal_derivative1_power2',...
    'std_dvars','dvars'};

%prep structure with empty arrays
for cn = 1:length(conf_names)
    QC.(conf_names{cn}) = [];
end

% load confounds signals from fmriprep
run_confounds = bids.util.tsvread(tboldconf);
for cn = 1:length(conf_names)
    temprun_confounds=demean_detrend(run_confounds.(conf_names{cn})');
    
    QC.(conf_names{cn}) = [QC.(conf_names{cn}); temprun_confounds'];
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function makepictures_vCG(QC,stage,rightsignallim,leftsignallim,FDmult)

% constants
numpts=numel(QC.FD);
rylimz=[min(rightsignallim) max(rightsignallim)];
lylimz=[min(leftsignallim) max(leftsignallim)];
%FDmult = 10; %multiplier to get FD in range of DVARS values

figure('Position',[1 1 1700 1200],'Visible','Off');

% subplot1 = mvm
subplot(10,1,1:2);
pointindex=1:numpts;
plot(pointindex,QC.MVM);
xlim([0 numpts]);
ylim([-1 1]);
ylabel('mvm');

% subplot2 = GS
subplot(10,1,3)
plot(pointindex,QC.global_signal,'g');
hline(0,'k');
xlim([0 numpts]);
ylim(rylimz);
ylabel('G:GS');

% subplot3 = FD
subplot(10,1,4:5)
plot([1:numpts],QC.FD,'Color',[1 0.8 0.8],'LineWidth',0.1); hold on;
plot([1:numpts],QC.fFD,'r','LineWidth',1.5);
hline(0.1,'k');
xlim([0 numpts]);
ylim([0 1])
ylabel('mm, R:fFD, M=FD');

% subplots 3-4: 
subplot(10,1,6:9);
imagesc(QC.GMtcs(:,:,stage),rylimz); colormap(gray); ylabel('GRAY');
subplot(10,1,10);
imagesc([QC.WMtcs(:,:,stage);QC.CSFtcs(:,:,stage)],rylimz); ylabel('WM CSF');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function save_out_maskfile(input_template,out_data,outname)
outfile = load_nii(input_template); % for header info
img_dims = size(outfile.img);
outfile.img = reshape(out_data,img_dims);
outfile.prefix = outname;
save_nii(outfile,outname);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fnames = resample_masks(anat_string,QC,space,project_dir,singularity_cmd_start,afni_sif,res)

type_names = {'WM','CSF','WB','GREY'}; % AD - removed WM mask resampling; using make_fs_masks.m output {'WM','CSF','WB','GREY'}
types = {'label-WM_probseg','label-CSF_probseg','desc-brain_mask','label-GM_probseg'}; %{'label-WM_probseg','label-CSF_probseg','desc-brain_mask','label-GM_probseg'}

%system('module load singularity/latest');
currentDir = pwd;
cd(anat_string);

for t = 1:length(types)
    
    thisName = ['sub-' QC.subjectID '_space-' space '_' res '_' types{t} '.nii.gz'];
    thisName_orig = ['sub-' QC.subjectID '_space-' space '_' types{t} '.nii.gz'];
    fnames.([type_names{t} 'maskfile']) = [anat_string thisName];
    
   
    % only make them if they don't exist
    if ~exist(fnames.([type_names{t} 'maskfile']))
        if strcmp(res,'res-2')
            system([singularity_cmd_start 'singularity exec -B ' project_dir ':' project_dir ' ' afni_sif ' 3dresample -dxyz 2 2 2 -prefix ' thisName ' -input ' thisName_orig]);
        elseif strcmp(res,'res-1')
            system([singularity_cmd_start 'singularity exec -B ' project_dir ':' project_dir ' ' afni_sif ' 3dresample -dxyz 1 1 1 -prefix ' thisName ' -input ' thisName_orig]);
        else
            disp ('resolution not supported')
        end
    else
        fprintf('mask %s already exists \n', thisName)
    end
end

cd(currentDir);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [H,f,s,c,tau,w] = LSTransform(t,h,TH,Tr,ofac,hifac)
%LSTRANSFORM  Fills up missing samples in time-series with estimates.
%   [H, F, S, C, TAU, W] = LSTRANSFORM(T, H, TH, TR, OFAC, HIFAC) takes
%   the signal in H and samples new data points given the frequecy content
%   of H at points TH. TR is the sampling resolution (this argument is
%   currently not used), OFAC is the oversampling factor, and HIFAC is the
%   highest allowed frequency.
%
%   H must be NrOfTimePoints-by-NrOfSamples matrix.
%
%   Example:
%
%   rdata = randn(512, 200);
%   allidx = (1:size(rdata, 1))';
%   badidx = ceil(numel(allidx) .* rand(10, 1));
%   goodidx = allidx;
%   goodidx(badidx) = [];
%   repdata = LSTRANSFORM(goodidx, rdata(goodidx, :), allidx, 1, 4, 1);

%Input t is a column vector listing the time points for which observations
%are present.  Input h is a matrix with observations in columns and the
%number of rows equals the number the time points.  For our purposes number
%of voxels = number of columns.  Ofac = oversampling frequency (generally
%>=4), hifac = highest frequency allowed.  hifac = 1 means 1*nyquist limit
%is highest frequency sampled.  
%Lasted edited:  Anish Mitra, October 25 2012

% double precision
D = double(h);

% number of time points
N = size(D, 1);

% total time span
t = t(:);
T = max(t) - min(t);

% calculate sampling frequencies
f = (1 / (T * ofac) : 1 / (T * ofac) : hifac * N / (2 * T))';

% angular frequencies and constant offsets
w = 2 * pi * f;
wt = w * t';
tau = atan2(sum(sin(2 .* wt), 2), sum(cos(2 * wt), 2)) ./ (2 * w);
wtau = wt - repmat(w .* tau, 1, length(t));

% spectral power sin and cosine terms
cterm = cos(wtau);
sterm = sin(wtau);

% compute numerator and denominator for cosines
numerator = cterm * D;
denominator = sum(cterm .* cterm,2);
c = diag(1 ./ denominator) * numerator;

% repeat the above for Sine term
numerator = sterm * D;
denominator = sum(sterm .* sterm,2);
s = diag(1 ./ denominator) * numerator;

% the inverse function to re-construct the original time series
prod = TH(:) * w';
H = sin(prod) * s + cos(prod) * c;

% normalize the reconstructed spectrum, needed when ofac > 1
H = H * diag(std(h) ./ std(H));
