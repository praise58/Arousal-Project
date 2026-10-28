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