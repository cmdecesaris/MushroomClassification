source("Preprocessing.R")

#additional tables of the codes and keys of the categorical datasets
#tables of variable descriptions


mush3=mush2
mush3$classtype = ifelse(mush2$class==1, "Inedible","Edible")

p1 = mush3%>%ggplot(aes(x=factor(classtype),y=cap.diameter_max, fill=factor(classtype)))+
  geom_boxplot(position="dodge")+
  labs(title="Max Cap Diameter", x="Edibility", y="Centimeters", fill="Edibility")+
  theme(legend.position = "none")

p2 = mush3%>%ggplot(aes(x=factor(classtype),y=cap.diameter_min, fill=factor(classtype)))+
  geom_boxplot(position="dodge")+
  labs(title="Min Cap Diameter", x="Edibility", y="Centimeters", fill="Edibility")+
  theme(legend.position = "none")


p3 = mush3%>%ggplot(aes(x=factor(classtype),y=stem.width_max, fill=factor(classtype)))+
  geom_boxplot(position="dodge")+
  labs(title="Max Stem Width", x="Edibility", y="Milimeters", fill="Edibility")+
  theme(legend.position = "none")

p4 = mush3%>%ggplot(aes(x=factor(classtype),y=stem.width_min, fill=factor(classtype)))+
  geom_boxplot(position="dodge")+
  labs(title="Min Stem Width", x="Edibility", y="Milimeters", fill="Edibility")+
  theme(legend.position = "none")

p5 = mush3%>%ggplot(aes(x=factor(classtype),y=stem.height_max, fill=factor(classtype)))+
  geom_boxplot(position="dodge")+
  labs(title="Max Stem Height", x="Edibility", y="Centimeters", fill="Edibility")+
  theme(legend.position = "none")

p6 = mush3%>%ggplot(aes(x=factor(classtype),y=stem.height_min, fill=factor(classtype)))+
  geom_boxplot(position="dodge")+
  labs(title="Min Stem Height", x="Edibility", y="Centimeters", fill="Edibility")+
  theme(legend.position = "none")

grid.arrange(p1,p3, p5,nrow = 1)
grid.arrange(p2,p4,p6, nrow = 1)


var_og= c("family",               
          "name" ,               
          "class",                
          "cap.diameter" ,       
          "cap.shape",            
          "Cap.surface"  ,       
          "cap.color",            
          "does.bruise.or.bleed",
          "gill.attachment",      
          "gill.spacing" ,       
          "gill.color",           
          "stem.height"  ,       
          "stem.width",           
          "stem.root"  ,         
          "stem.surface",         
          "stem.color" ,         
          "veil.type",            
          "veil.color" ,         
          "has.ring",             
          "ring.type"  ,         
          "Spore.print.color",    
          "habitat",          
          "season")

description_og= c("Taxomic Family",               
                  "Species Name" ,               
                  "Edibility Status",                
                  "Diameter of Mushroom Cap (cm)" ,       
                  "Mushroom Cap Shape",            
                  "Cap Surface Texture"  ,       
                  "Color of Cap",            
                  "Does Mushroom Bruise or Bleed (t/f)",
                  "Gill Attachment Present",      
                  "Gill Spacing Type" ,       
                  "Gill Color",           
                  "Stem Height (cm)"  ,       
                  "stem.width (mm)",           
                  "Stem Root Type"  ,         
                  "Stem Surface Type",         
                  "Stem Color" ,         
                  "Veil Type",            
                  "Veil Color" ,         
                  "Is a Ring Present (t/f)",             
                  "Type of Ring"  ,         
                  "Color of Spores",    
                  "Native Habitat",          
                  "Growing Season")



colors = color_codes=c("n", "b","g", "r","p","u","e", "w", "y","l","o","k")
color_names=c("brown","buff","gray","green","pink","purple","red","white","yellow","blue","orange","black")

habitat = c("grasses","leaves","meadows","paths","heaths","urban","waste","woods","","","","")

habitat_code=c("g","l","m","p","h","u","w","d","","","","")

seasons = c("Spring","Summer","Autumn","Winter","","","","","","","","")
seasons_code = c("s","u","a","w","","","","","","","","")

shape = c("bell", "conical", "convex", "flat","sunken", "spherical", "others","","","","","")


shape_code = c("b","c","x","f","s","p","o","","","","","")

ring = c("cobwebby","evanescent","flaring","grooved","large","pendant","sheathing","zone","scaly","movable","none","unknown")

ring_code = c("c","e","r","g","l","p","s","z","y","m","f","?")

oy=do.call("cbind",list(color_names, color_codes,ring, ring_code,habitat, habitat_code,shape,shape_code, seasons,seasons_code))




data.frame(cbind(var_og,description_og)) %>%
  kable(caption = '',
        col.names = c("Variable Name","Description"))%>% kable_classic_2(full_width = F,                                                       html_font = "Ariel",lightable_options =c("striped","bordered")  )%>% 
  column_spec (1:2,
               border_left = T, 
               border_right = T)
data.frame(ring,ring_code) %>%
  kable(caption = '',
        col.names = c("Ring Type","Code"))%>% kable_classic(full_width = F,                                                       html_font = "Ariel",lightable_options ="striped"  )%>% 
  column_spec (1:2,
               border_left = T, 
               border_right = T)

data.frame(habitat,habitat_code) %>%
  kable(caption = '',
        col.names = c("Habitat","Code"))%>% kable_classic(full_width = F,                                                       html_font = "Ariel",lightable_options ="striped"  )%>% 
  column_spec (1:2,
               border_left = T, 
               border_right = T)

data.frame(shape,shape_code) %>%
  kable(caption = '',
        col.names = c("Cap Shape","Code"))%>% kable_classic(full_width = F,                                                       html_font = "Ariel",lightable_options ="striped"  )%>% 
  column_spec (1:2,
               border_left = T, 
               border_right = T)

data.frame(oy) %>%
  kable(caption = '',
        col.names = c("Color","Color Code","Ring Type","Ring Code","Habitat Type","Habitat Code","Cap Shape","Cap Code","Season","Season Code"))%>% kable_classic_2(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:10,
               border_left = T, 
               border_right = T)



#Example of the preprocessing process, shows raw and processed side by side
cbind(head(mush[c(4,23)],3),head( mush2[c(3,6,9:12)],3))%>%kable()%>% kable_classic(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:8,
               border_left = T, 
               border_right = T) %>%
  column_spec(c(3:4), bold = T, color = "black", background = "lightblue") %>%
  column_spec(c(5:8), bold = T, color = "black", background = "pink")
mush2 = mush1[c(3,11,15:86)]







mush3$cap.color_n=ifelse(mush3$cap.color_n=="1", paste("Yes: ",as.character(sum(mush2$cap.color_n))),paste("No: ",as.character(173-sum(mush2$cap.color_n))))

mush3$cap.color_b=ifelse(mush3$cap.color_b=="1", paste("Yes: ",as.character(sum(mush2$cap.color_b))),paste("No: ",as.character(173-sum(mush2$cap.color_b))))

mush3$cap.color_g=ifelse(mush3$cap.color_g=="1", paste("Yes: ",as.character(sum(mush2$cap.color_g))),paste("No: ",as.character(173-sum(mush2$cap.color_g))))

mush3$cap.color_r=ifelse(mush3$cap.color_r=="1", paste("Yes: ",as.character(sum(mush2$cap.color_r))),paste("No: ",as.character(173-sum(mush2$cap.color_r))))

mush3$cap.color_p=ifelse(mush3$cap.color_p=="1", paste("Yes: ",as.character(sum(mush2$cap.color_p))),paste("No: ",as.character(173-sum(mush2$cap.color_p))))

mush3$cap.color_u=ifelse(mush3$cap.color_u=="1", paste("Yes: ",as.character(sum(mush2$cap.color_u))),paste("No: ",as.character(173-sum(mush2$cap.color_u))))

mush3$cap.color_e=ifelse(mush3$cap.color_e=="1", paste("Yes: ",as.character(sum(mush2$cap.color_e))),paste("No: ",as.character(173-sum(mush2$cap.color_e))))

mush3$cap.color_w=ifelse(mush3$cap.color_w=="1", paste("Yes: ",as.character(sum(mush2$cap.color_w))),paste("No: ",as.character(173-sum(mush2$cap.color_w))))

mush3$cap.color_y=ifelse(mush3$cap.color_y=="1", paste("Yes: ",as.character(sum(mush2$cap.color_y))),paste("No: ",as.character(173-sum(mush2$cap.color_y))))

mush3$cap.color_l=ifelse(mush3$cap.color_l=="1", paste("Yes: ",as.character(sum(mush2$cap.color_l))),paste("No: ",as.character(173-sum(mush2$cap.color_l))))

mush3$cap.color_o=ifelse(mush3$cap.color_o=="1", paste("Yes: ",as.character(sum(mush2$cap.color_o))),paste("No: ",as.character(173-sum(mush2$cap.color_o))))

mush3$cap.color_k=ifelse(mush3$cap.color_k=="1", paste("Yes: ",as.character(sum(mush2$cap.color_k))),paste("No: ",as.character(173-sum(mush2$cap.color_k))))




mush3$stem.color_n=ifelse(mush3$stem.color_n=="1", paste("Yes: ",as.character(sum(mush2$stem.color_n))),paste("No: ",as.character(173-sum(mush2$stem.color_n))))

mush3$stem.color_b=ifelse(mush3$stem.color_b=="1", paste("Yes: ",as.character(sum(mush2$stem.color_b))),paste("No: ",as.character(173-sum(mush2$stem.color_b))))

mush3$stem.color_g=ifelse(mush3$stem.color_g=="1", paste("Yes: ",as.character(sum(mush2$stem.color_g))),paste("No: ",as.character(173-sum(mush2$stem.color_g))))

mush3$stem.color_r=ifelse(mush3$stem.color_r=="1", paste("Yes: ",as.character(sum(mush2$stem.color_r))),paste("No: ",as.character(173-sum(mush2$stem.color_r))))

mush3$stem.color_p=ifelse(mush3$stem.color_p=="1", paste("Yes: ",as.character(sum(mush2$stem.color_p))),paste("No: ",as.character(173-sum(mush2$stem.color_p))))

mush3$stem.color_u=ifelse(mush3$stem.color_u=="1", paste("Yes: ",as.character(sum(mush2$stem.color_u))),paste("No: ",as.character(173-sum(mush2$stem.color_u))))

mush3$stem.color_e=ifelse(mush3$stem.color_e=="1", paste("Yes: ",as.character(sum(mush2$stem.color_e))),paste("No: ",as.character(173-sum(mush2$stem.color_e))))

mush3$stem.color_w=ifelse(mush3$stem.color_w=="1", paste("Yes: ",as.character(sum(mush2$stem.color_w))),paste("No: ",as.character(173-sum(mush2$stem.color_w))))

mush3$stem.color_y=ifelse(mush3$stem.color_y=="1", paste("Yes: ",as.character(sum(mush2$stem.color_y))),paste("No: ",as.character(173-sum(mush2$stem.color_y))))

mush3$stem.color_l=ifelse(mush3$stem.color_l=="1", paste("Yes: ",as.character(sum(mush2$stem.color_l))),paste("No: ",as.character(173-sum(mush2$stem.color_l))))

mush3$stem.color_o=ifelse(mush3$stem.color_o=="1", paste("Yes: ",as.character(sum(mush2$stem.color_o))),paste("No: ",as.character(173-sum(mush2$stem.color_o))))

mush3$stem.color_k=ifelse(mush3$stem.color_k=="1", paste("Yes: ",as.character(sum(mush2$stem.color_k))),paste("No: ",as.character(173-sum(mush2$stem.color_k))))



mush3$season_a=ifelse(mush3$season_a=="1",   paste("Yes: ",as.character(sum(mush2$season_a))),paste("No: ",as.character(173-sum(mush2$season_a))))

mush3$season_u=ifelse(mush3$season_u=="1",  paste("Yes: ",as.character(sum(mush2$season_u))),paste("No: ",as.character(173-sum(mush2$season_u))))

mush3$season_s=ifelse(mush3$season_s=="1",  paste("Yes: ",as.character(sum(mush2$season_s))),paste("No: ",as.character(173-sum(mush2$season_s))))

mush3$season_w=ifelse(mush3$season_w=="1",  paste("Yes: ",as.character(sum(mush2$season_w))),paste("No: ",as.character(173-sum(mush2$season_w))))

n = 1

u=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(season_u))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Grows in Summer?")


a=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(season_a))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Grows in Fall?")

w=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(season_w))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Grows in Winter?")

sp=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(season_s))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Grows in Spring?")


grid.arrange(a, u,w,sp, ncol=2)




mush3$cap.shape_b=ifelse(mush3$cap.shape_b=="1", paste("Yes: ",as.character(sum(mush2$cap.shape_b))),paste("No: ",as.character(173-sum(mush2$cap.shape_b))))
mush3$cap.shape_c=ifelse(mush3$cap.shape_c=="1", paste("Yes: ",as.character(sum(mush2$cap.shape_c))),paste("No: ",as.character(173-sum(mush2$cap.shape_c))))

mush3$cap.shape_x=ifelse(mush3$cap.shape_x=="1", paste("Yes: ",as.character(sum(mush2$cap.shape_x))),paste("No: ",as.character(173-sum(mush2$cap.shape_x))))

mush3$cap.shape_f=ifelse(mush3$cap.shape_f=="1",  paste("Yes: ",as.character(sum(mush2$cap.shape_f))),paste("No: ",as.character(173-sum(mush2$cap.shape_f))))


mush3$ring.type_z=ifelse(mush3$ring.type_z=="1", paste("Yes: ",as.character(sum(mush2$ring.type_z))),paste("No: ",as.character(173-sum(mush2$ring.type_z))))

mush3$cap.shape_s=ifelse(mush3$cap.shape_s=="1",  paste("Yes: ",as.character(sum(mush2$cap.shape_s))),paste("No: ",as.character(173-sum(mush2$cap.shape_s))))


mush3$cap.shape_o=ifelse(mush3$cap.shape_o=="1",  paste("Yes: ",as.character(sum(mush2$cap.shape_o))),paste("No: ",as.character(173-sum(mush2$cap.shape_o))))

mush3$cap.shape_p=ifelse(mush3$cap.shape_p=="1", paste("Yes: ",as.character(sum(mush2$cap.shape_p))),paste("No: ",as.character(173-sum(mush2$cap.shape_p))))




b=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.shape_b))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Bell Shaped?")

c=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.shape_c))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Conical Shaped?")

xp=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.shape_x))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Convex Shaped?")

f=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.shape_f))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Flat Shaped?")


s=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.shape_s))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Sunken Shaped?")


o=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.shape_o))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Other Shaped?")


p=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.shape_p))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Spherical Shaped?")


grid.arrange(b,
             c,
             xp,
             f,
             s,
             o,
             p, ncol = 2)




stC_n=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_n))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Brown?")


stC_b=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_b))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Buff?")

stC_g=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_g))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Gray?")

stC_r=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_r))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Green?")

stC_p=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_p))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Pink?")

stC_u=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_u))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Purple?")

stC_e=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_e))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Red?")

stC_w=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_w))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color White?")

stC_y=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_y))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Yellow?")

stC_l=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_l))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Blue?")

stC_o=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_o))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Orange?")

stC_k=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(stem.color_k))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Stem Color Black?")


grid.arrange(stC_n,
             stC_b,
             stC_g,
             stC_r,
             stC_p,
             stC_u,
             stC_e,
             stC_w,
             stC_y,
             stC_l,
             stC_o,
             stC_k, ncol = 3)






cpC_n=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_n))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Brown?")


cpC_b=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_b))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Buff?")

cpC_g=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_g))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Gray?")

cpC_r=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_r))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Green?")

cpC_p=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_p))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Pink?")

cpC_u=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_u))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Purple?")

cpC_e=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_e))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Red?")

cpC_w=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_w))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color White?")

cpC_y=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_y))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Yellow?")

cpC_l=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_l))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Blue?")

cpC_o=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_o))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Orange?")

cpC_k=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(cap.color_k))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size=10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Cap Color Black?")


grid.arrange(cpC_n,
             cpC_b,
             cpC_g,
             cpC_r,
             cpC_p,
             cpC_u,
             cpC_e,
             cpC_w,
             cpC_y,
             cpC_l,
             cpC_o,
             cpC_k, ncol = 3)

ring_z=mush3%>%ggplot(aes( x=factor(classtype), fill=factor(classtype)))+
  geom_col(aes(x = 1, y = n), position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~factor(ring.type_z))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 10))+
  scale_fill_discrete(name = "Edible?", 
                      labels = c("No","Yes"))+
  labs(title="Ring Type Zone?")


grid.arrange(w,b,
             cpC_n,
             cpC_r,
             stC_w, ncol = 2)


(
  
  
  
  
  
  
  
  
  
  
  
  
mush3$ring.type_c=ifelse(mush3$ring.type_c=="1", paste("Yes: ",as.character(sum(mush3$ring.type_c)),paste("No: ",as.character(173-sum(mush3$ring.type_c)))))

mush3$ring.type_b=ifelse(mush3$ring.type_e=="1", paste("Yes: ",as.character(sum(mush2$ring.type_e))),paste("No: ",as.character(173-sum(mush2$ring.type_e))))

mush3$ring.type_g=ifelse(mush3$ring.type_r=="1", paste("Yes: ",as.character(sum(mush2$ring.type_r))),paste("No: ",as.character(173-sum(mush2$ring.type_r))))

mush3$ring.type_r=ifelse(mush3$ring.type_g=="1", paste("Yes: ",as.character(sum(mush2$ring.type_g))),paste("No: ",as.character(173-sum(mush2$ring.type_g))))

mush3$ring.type_p=ifelse(mush3$ring.type_l=="1", paste("Yes: ",as.character(sum(mush2$ring.type_l))),paste("No: ",as.character(173-sum(mush2$ring.type_l))))

mush3$ring.type_u=ifelse(mush3$ring.type_p=="1", paste("Yes: ",as.character(sum(mush2$ring.type_p))),paste("No: ",as.character(173-sum(mush2$ring.type_p))))

mush3$ring.type_e=ifelse(mush3$ring.type_s=="1", paste("Yes: ",as.character(sum(mush2$ring.type_s))),paste("No: ",as.character(173-sum(mush2$ring.type_s))))

mush3$ring.type_w=ifelse(mush3$ring.type_z=="1", paste("Yes: ",as.character(sum(mush2$ring.type_z))),paste("No: ",as.character(173-sum(mush2$ring.type_z))))

mush3$ring.type_y=ifelse(mush3$ring.type_y=="1", paste("Yes: ",as.character(sum(mush2$ring.type_y))),paste("No: ",as.character(173-sum(mush2$ring.type_y))))

mush3$ring.type_l=ifelse(mush3$ring.type_m=="1", paste("Yes: ",as.character(sum(mush2$ring.type_m))),paste("No: ",as.character(173-sum(mush2$ring.type_m))))

mush3$ring.type_o=ifelse(mush3$ring.type_f=="1", paste("Yes: ",as.character(sum(mush2$ring.type_f))),paste("No: ",as.character(173-sum(mush2$ring.type_f))))

mush3$ring.type_k=ifelse(mush3$ring.type_z=="1", paste("Yes: ",as.character(sum(mush2$ring.type_z))),paste("No: ",as.character(173-sum(mush2$ring.type_z))))





grid.arrange(b,
             c,
             xp,
             f,
             s,
             o,
             p, ncol = 2)

