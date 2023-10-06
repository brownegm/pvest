
  
roco1<-pv_dat%>%filter(species=="roco" & leaf=="1")

data<-pv_dat%>%filter(species=="roco"& leaf=="4")

data<-pv_dat%>%filter(species=="alma"& leaf=="2")

fw.index = 5; wp.index = 4; dm.index = 3; n_pts = T

# ROCO 1 & 4 , ALMA 2 problems as of 20230719

i<-unique_ids[1]

max_row=nrow(data)
plot(leaf_estimate$relative.water.deficit, leaf_estimate$inv.water.potential)

ggplot(data=leaf_estimate, mapping = aes(x=relative.water.deficit, y=inv.water.potential))+
  geom_point(color="red",fill="blue", size=5, shape=21)+
  labs(y="Inverse Water Potential (1/Psi)", 
       x="Relative Water Deficit (%)")+
  geom_smooth()+
  theme_bw()

vignette("ggplot2-specs")

leaf_estimate

t<-check_n_pts(leaf_estimate, wp.index="inv.water.potential", wm.index="relative.water.deficit")#$cv10


pv_params_fil<-pv_params%>%filter(is.na(modulus))

plot

kl