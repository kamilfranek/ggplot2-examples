#libraries
library(ggplot2)
library(dplyr)
library(scales)
library(forcats)
library(stringr)
library(readxl)

#setting up variables
plot_title = "Income Statement for Apple Inc."
plot_subtitle = "Income statement in billions USD. Fiscal year ends in September."
source = "kamilfranek.com | SEC EDGAR"
data = read.csv("data.csv")

#loading data
data_tmp = data %>%
  filter(ticker == "aapl") %>%
  arrange(desc(type), date,element_id)%>%
  mutate(metric_wrap = str_wrap(label, width = 55))%>%
  mutate(metric_wrap = fct_rev(fct_inorder(metric_wrap))) %>% 
  mutate(grouping = fct_inorder(grouping)) %>%
  filter(grouping %in% c("2016","2017","2018","Change 2018"))  

# building list of items that we want to see bold in final chart
bold_items = data_tmp %>% 
  filter(category=="sub_core") %>%
  select(label) %>%
  mutate(label=as.character(label))%>%
  pull %>%
  unlist %>%
  unique()
bold_items_vect = as.expression(sapply(bold_items, function(x) bquote(bold(.(x))))) 

#generating plot
plot_waterfall = data_tmp %>% 
    ggplot(aes(fill=color_categ))+
    geom_hline(yintercept = 0, color="grey80", size=0.2)+
    geom_segment(aes(x=id2+0.40,xend = id2+0.60,y=ifelse(category=="sub_core",AmountEOPMagnified,AmountBOPMagnified),
                     yend=ifelse(category=="sub_core",AmountEOPMagnified,AmountBOPMagnified)), 
                 size=0.2,linetype = "solid",color="grey75")+
    geom_rect(aes(x=metric_wrap,xmin=id2-0.41,xmax=id2+0.41,ymin=AmountBOPMagnified,ymax=AmountEOPMagnified),
              alpha=1)+
    geom_text(data=(data_tmp %>% filter(category!="sub_core", abs(AmountAdj)>0.05)), 
              aes(x=metric_wrap, y=ifelse(AmountAdj>0, AmountEOPMagnified, AmountEOPMagnified),
                  label=ifelse(abs(AmountAdj)>100,format(AmountAdj,digits=0),
                               format(round(AmountAdj,1),nsmall=1)),
                  hjust=ifelse(AmountAdj>0,ifelse(abs(AmountAdj)>100,0,-0.1),1.1)
              ),
              size=3, vjust=0.5, color="grey70")+
    geom_text(data=(data_tmp %>% filter(category=="sub_core", abs(AmountAdj)>0.05)), 
              aes(x=metric_wrap, y=ifelse(AmountAdj>0, AmountEOPMagnified, AmountEOPMagnified),
                  label=ifelse(abs(AmountAdj)>100,format(AmountAdj,digits=0),
                               format(round(AmountAdj,1),nsmall=1)),
                  hjust=ifelse(AmountAdj>0,ifelse(abs(AmountAdj)>100,-0.15,-0.1),1.1)
              ),
              size=3, vjust=0.5, color="grey50", fontface="bold")+
    scale_fill_manual(values = c(negative = "red3", negative_change="red", positive="green4",
                                 positive_change="green3", sub_core="grey"))+
    scale_y_continuous(labels = function(y) format(y,big.mark= ",", digits = 0), 
                     expand = c(0.3, 0),
                     breaks = pretty_breaks(n=3))+
    scale_x_discrete(labels=bold_items_vect)+
    guides(fill=FALSE, alpha=FALSE) + 
    coord_flip()+
    facet_wrap(~grouping, nrow=1)+
    theme_minimal(base_size = 15)+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.x=element_blank())+
    labs(title=plot_title, 
         subtitle=plot_subtitle,
         y="",  x="", caption = source)

         
plot_waterfall

