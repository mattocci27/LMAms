nohup R --vanilla --slave < ~/Dropbox/MS/LES_MS/LMApsModel/model/GL_rand_across.r > ~/Dropbox/LES/GL_rand_across.log&

nohup R --vanilla --slave < ~/Dropbox/MS/LES_MS/LMApsModel/model/GL_obs.r > ~/Dropbox/LES/GL_obs.log&


R --vanilla --slave < ~/Dropbox/MS/LES_MS/LMApsModel/model/GL_rand_within.r > ~/Dropbox/LES/GL_rand_within.log
