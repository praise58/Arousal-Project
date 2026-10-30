full_3d_corrmat_sleepy.mat
Used to be a 286 x 286 x 64 matrix of subjects who are sleepy.
It got cut down to 286 x 286 x 52 after removing subjects w/ less than 5 minutes of data.

full_3d_corrmat_nsleepy.mat
286 x 286 x 94 matrix of subjects who were awake.

within_3d_corrmat_(n)sleepy.mat
[both files]
Contain 286 x 286 x 17 matrices of either awake or sleepy subjects that have >=20 minutes of data.
Will use for precision analyses.

full_matrices.mat
286 x 286 x 146 matrix, where :x:x[1-94] contains the awake matrices, and :x:x[95-146] contains the sleepy matrices.

within_matrices.mat
286 x 286 x 34 matrix, where first 17 284 x 284 matrices are awake, and 18:34 matrices are sleepy.
Needed for the NBS.

full_matrices_id-rm.mat
286 x 286 x 146 matrix, where :x:x[1-94] contains the awake matrices, and :x:x[95-146] contains the sleepy matrices.
Needed for the NBS.
Removed identity (main diagonal) for NBS prep.

within_matrices_id-rm.mat
286 x 286 x 34 matrix, where first 17 284 x 284 matrices are awake, and 18:34 matrices are sleepy.
Needed for the NBS.matrices, and :x:x[95-146] contains the sleepy matrices.
Needed for the NBS.
Removed identity (main diagonal) for NBS prep.

full_SA_NBS.mat
This is the result for the NBS with the following parameters:
T-Test for Sleepy > Awake
Threshold 4.5
5000 permutations
Significance <0.05
Component size = Extent
There were two significant networks:
    Network 1: p-val < 0.001, 5 edges, 6 nodes; 5 VIS nodes *all* in the RH connected to the same SMN-D node in the LH. SO INTERESTING!!
    Network 2: p-val = 0.010, 2 edges, 3 nodes; 2 SMN-D nodes connected to the same AUD node, all in the RH.

full_AS_NBS.mat
This is the result for the NBS with the following parameters:
T-Test for Awake > Sleepy
Threshold 4.5
5000 permutations
Significance <0.05
Component size = Extent
There were FOUR!! significant networks:
	Network 1: p-val = 0.001, 4 edges, 5 nodes
	Left VAN node connected with 2 LH nodes and 2 RH nodes. 
		One LH node is inferior temporal lobe to unassigned network. The other LH node is SMN-D.
		The 2 RH nodes are both SMN-D. 
		It seems like the VAN node has some hubby properties that activate as humans transition from sleepy to awake.
		
	Network 2: p-val = 0.014, 2 edges, 3 nodes
	LH Reward network is connected to LH SMN-D and midline-but-RH "ParietoMedial" network. *What is that??*
	Network 3: p-val = 0.002, 3 edges, 4 nodes
	LH SMN-D connected to 3 RH nodes.
		2 RH nodes are AUD. 1 RH node is very anterior, part of the CON.
	Network 4: p-val = 0.002, 3 edges, 4 nodes
	SALIENCE!! connected with 3 RH nodes all in the DMN.
In terms of interpretation, I would just say that Awake > Sleepy is just the networks that are active when we are awake... So, not SUPER interesting, and might have a lot of sampling error. We should check what the precision sample looks like.
ANDDD I forgot to save the .mat file. lol.	

**No significant results were found with Sleepy > Awake for within subjects.** We must use OODA.