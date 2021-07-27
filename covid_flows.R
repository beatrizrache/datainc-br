## SRAG Flows in São Paulo and Amazonas states, Brazil
require(dplyr)
require(igraph)
require(geobr)
require(htmlwidgets)
require(sf)
require(ggplot2)
require(plotly)
require(manipulateWidget)

# Load Hospitalization Data
flows<-read.table(url('https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2021/INFLUD21-05-07-2021.csv'),sep=';',header=T,encoding='UTF-8')
flows_filter<-flows%>%
  filter((SG_UF_INTE=='SP'&SG_UF=='SP')|(SG_UF_INTE=='AM'&SG_UF=='AM'))%>%
  mutate(date=as.Date(DT_INTERNA,format='%d/%m/%Y'),
         quarter=lubridate::quarter(date),
         age=NU_IDADE_N)

# Calculate hospitalization shares
flows_count<-flows_filter%>%
  filter(!is.na(CO_MUN_RES)&!is.na(CO_MU_INTE))%>%
  group_by(quarter,state=SG_UF,mun_res=CO_MUN_RES,mun_hosp=CO_MU_INTE)%>%
  summarize(count=n(),count_icu=sum(ifelse(UTI==1,1,0)))%>%
  mutate(share=count/sum(count),share_icu=count_icu/sum(count_icu,na.rm=T))
write.csv(flows_count,'Z:/Temp/BR/sp_am_flows_2021.csv',row.names=F)

# Load Shapefile
# install.packages('geobr')
# install.packages('sf')
shp<-geobr::read_municipality(year=2020,showProgress=F)%>%
  filter(abbrev_state%in%c('AM','SP'))%>%
  mutate(mun=floor(code_muni/10))%>%
  select(mun,name_muni,geom)
shp_centroid<-shp%>%
  group_by(mun)%>%
  mutate(centr=st_centroid(geom))

# Network for each state
get_nodes<-function(s) {
  flows_count%>%
    filter(state==s)%>%
    data.table::melt(measure.vars=c('mun_res','mun_hosp'))%>%
    distinct(value)%>%
    dplyr::rename(mun=value)}

get_edges<-function(s,q,w) {
  get_nodes(s)%>%rename(mun_res=mun)%>%
    merge(get_nodes(s)%>%rename(mun_hosp=mun),all=T)%>%
    left_join(flows_count%>%filter(quarter==q),by=c('mun_res','mun_hosp'))%>%
    mutate_at(.vars=c('share','share_icu'),.funs=function(x) {ifelse(is.na(x),0,x)})%>%
    rename(weight=w)}

get_net<-function(s,q,w) {
  graph_from_data_frame(get_edges(s,q,w),get_nodes(s),directed=TRUE) }

get_cluster<-function(s,q,w){
  get_nodes(s)%>%
    mutate(cluster_fg=cluster_fast_greedy(as.undirected(get_net(s,q,w)))$membership,
           label=reorder(paste0('Cluster: ',cluster_fg),cluster_fg))}

# Test
get_cluster(s='SP',q=1,w='share')%>%head

## Figure 1: SRAG Hospitalization Flows and Clusters in 2021
# Map
gph=ggplot(get_cluster(s='SP',q=1,w='share')%>%
             inner_join(shp,by=c('mun')),aes(text=name_muni))+
  geom_sf(aes(geometry=geom,fill=label),lwd=0.1,show.legend=F)+
  #geom_sf(data=shp_centroid%>%filter(name_muni=='São Paulo')%>%ungroup,aes(geometry=centr))+
  theme_void()+
  theme(axis.line=element_blank(),
        plot.title=element_text(hjust = 0.5))+
  scale_fill_manual(values=rep(RColorBrewer::brewer.pal(n=12,name='Paired'),times=10))
gph_plotly<-ggplotly(gph,tooltip=c('text','fill'))%>%
  layout(title=list(text = paste0('Figure 1: SARS Hospitalization Clusters in Q1 2021 in São Paulo state, Brazil',
                                  '<br>',
                                  '<sup>',
                                  'Note: Colors repeat themselves in the legend. Interact with graph by hovering over map with mouse and clicking on clusters on the legend.',
                                  '</sup>')),
         margin = list(t=50))
saveWidget(as_widget(gph_plotly),"Z:/Temp/BR/fig1_map_covid21_sp_total.html")

## Figure 2: ICU SRAG Hospitalization Flows and Clusters in 2021
# Map
gph_icu=ggplot(get_cluster(s='SP',q=1,w='share_icu')%>%
                 inner_join(shp,by=c('mun')),aes(text=name_muni))+
  geom_sf(aes(geometry=geom,fill=label),lwd=0.1,show.legend=F)+
  geom_sf(data=shp_centroid%>%filter(name_muni=='São Paulo')%>%ungroup,aes(geometry=centr))+
  theme_void()+
  theme(axis.line=element_blank(),
        plot.title=element_text(hjust = 0.5))+
  scale_fill_manual(values=rep(RColorBrewer::brewer.pal(n=12,name='Paired'),times=10))+
  labs(caption='')
gph_plotly_icu<-ggplotly(gph_icu,tooltip=c('text','fill'))%>%
  layout(title=list(text = paste0('Figure 2: ICU SARS Hospitalization Clusters in Q1 2021 in São Paulo state, Brazil',
                                  '<br>',
                                  '<sup>',
                                  'Note: Colors repeat themselves in the legend. Interact with graph by hovering over map with mouse and clicking on clusters on the legend.',
                                  '</sup>')),
         margin = list(t=50))
saveWidget(as_widget(gph_plotly_icu),"Z:/Temp/BR/fig2_map_covid21_sp_icu.html")

## Figure 3: ICU SRAG Hospitalization Flows and Clusters in AM 2021
# Map
gph_icu_am1=ggplot(get_cluster(s='AM',q=1,w='share_icu')%>%
                     inner_join(shp,by=c('mun')),aes(text=name_muni))+
  geom_sf(aes(geometry=geom,fill=label),lwd=0.1,show.legend=F)+
  geom_sf(data=shp_centroid%>%filter(name_muni=='Manaus')%>%ungroup,aes(geometry=centr))+
  theme_void()+
  theme(axis.line=element_blank(),
        plot.title=element_text(hjust = 0.5))+
  scale_fill_manual(values=rep(RColorBrewer::brewer.pal(n=12,name='Paired'),times=10))+
  labs(caption='')
gph_plotly_icu_am1<-ggplotly(gph_icu_am1,tooltip=c('text','fill'))%>%
  layout(title=list(text = paste0('Figure 3: ICU SARS Hospitalization Clusters in Q1 2021 in Amazonas state, Brazil',
                                  '<br>',
                                  '<sup>',
                                  'Note: Interact with graph by hovering over map with mouse and clicking on clusters on the legend.',
                                  '</sup>')),
         margin = list(t=50))
saveWidget(as_widget(gph_plotly_icu_am1),"Z:/Temp/BR/fig3_map_covid21_am_icu.html")

# Q2
gph_icu_am2=ggplot(get_cluster(s='AM',q=2,w='share_icu')%>%
                     inner_join(shp,by=c('mun')),aes(text=name_muni))+
  geom_sf(aes(geometry=geom,fill=label),lwd=0.1,show.legend=F)+
  geom_sf(data=shp_centroid%>%filter(name_muni=='Manaus')%>%ungroup,aes(geometry=centr))+
  theme_void()+
  theme(axis.line=element_blank(),
        plot.title=element_text(hjust = 0.5))+
  scale_fill_manual(values=rep(RColorBrewer::brewer.pal(n=12,name='Paired'),times=10))+
  labs(caption='')
gph_plotly_icu_am2<-ggplotly(gph_icu_am2,tooltip=c('text','fill'))%>%
  layout(title=list(text = paste0('Figure 4: ICU SARS Hospitalization Clusters in Q2 2021 in Amazonas state, Brazil',
                                  '<br>')),
         margin = list(t=50))
saveWidget(manipulateWidget::combineWidgets(gph_plotly_icu_am1,gph_plotly_icu_am2),
           "Z:/Temp/BR/fig3_map_covid21_am_icu_byquarter.html")