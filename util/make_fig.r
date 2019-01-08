#!/bin/sh

echo $PATH

cd ~/Dropbox/MS/LES_MS/LMApsModel/fig_code/

nohup Rscript Fig_GL.r &
nohup Rscript Fig_PA.r &
nohup Rscript Fig_box_mean.r &
nohup Rscript Fig_cov_simple.r &
nohup Rscript r2.r &

