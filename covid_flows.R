## SRAG Flows in São Paulo, Brazil
require(dplyr)
require(igraph)
require(geobr)
require(htmlwidgets)
require(sf)
require(ggplot2)
require(plotly)

# Load Hospitalization Data
flows<-read.table(url('https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2021/INFLUD21-05-07-2021.csv'),sep=';',header=T,encoding='UTF-8')%>%
   filter(SG_UF_INTE=='SP'&SG_UF=='SP')%>%
   mutate(date=as.Date(DT_INTERNA,format='%D/%M/%Y'),
         age=NU_IDADE_N)


# Calculate hospitalization shares
flows_count<-flows%>%
  filter(!is.na(CO_MUN_RES)&!is.na(CO_MU_INTE))%>%
  group_by(mun_res=CO_MUN_RES,mun_hosp=CO_MU_INTE)%>%
  summarize(count=n(),count_icu=sum(ifelse(UTI==1,1,0)))%>%
  mutate(share=count/sum(count),share_icu=count_icu/sum(count_icu,na.rm=T))

# Load Shapefile
# install.packages('geobr')
# install.packages('sf')
shp<-geobr::read_municipality(year=2020,showProgress=F)%>%
  filter(abbrev_state=='SP')%>%
  mutate(mun=floor(code_muni/10))%>%
  select(mun,name_muni,geom)
shp_centroid<-shp%>%
  group_by(mun)%>%
  mutate(centr=st_centroid(geom))

# Network
nodes<-flows_count%>%
  data.table::melt(measure.vars=c('mun_res','mun_hosp'))%>%
  distinct(value)%>%
  dplyr::rename(mun=value)

edges<-nodes%>%rename(mun_res=mun)%>%
  merge(nodes%>%rename(mun_hosp=mun),all=T)%>%
  left_join(flows_count,by=c('mun_res','mun_hosp'))%>%
  mutate_at(.vars=c('share','share_icu'),.funs=function(x) {ifelse(is.na(x),0,x)})%>%
  rename(weight=share)

dir_net<-graph_from_data_frame(edges%>%rename(),nodes,directed=TRUE)
nodes_cluster<-nodes%>%
  mutate(cluster_fg=cluster_fast_greedy(as.undirected(dir_net))$membership,
         label=reorder(paste0('Cluster: ',cluster_fg),cluster_fg))

## Figure 1: SRAG Hospitalization Flows and Clusters in 2021
# Map
  gph=ggplot(nodes_cluster%>%
               inner_join(shp,by=c('mun')),aes(text=name_muni))+
    geom_sf(aes(geometry=geom,fill=label),lwd=0.1,show.legend=F)+
    geom_sf(data=shp_centroid%>%filter(name_muni=='São Paulo')%>%ungroup,aes(geometry=centr))+
    theme_void()+
    theme(axis.line=element_blank(),
          plot.title=element_text(hjust = 0.5))+
    scale_fill_manual(values=rep(RColorBrewer::brewer.pal(n=12,name='Paired'),times=10))
  gph_plotly<-ggplotly(gph,tooltip=c('text','fill'))%>%
        layout(title=list(text = paste0('Figure 1: SARS Hospitalization Clusters in 2021 in São Paulo, Brazil',
                                    '<br>',
                                    '<sup>',
                                    'Note: Colors repeat themselves in the legend. Interact with graph by hovering over map with mouse and clicking on clusters on the legend.',
                                    '</sup>')),
               margin = list(t=50))
  saveWidget(as_widget(gph_plotly),"Z:/Temp/BR/fig1_map_covid21_total.html")
  

## Figure 2: ICU SRAG Hospitalization Flows and Clusters in 2021
# Network
edges_icu<-nodes%>%rename(mun_res=mun)%>%
  merge(nodes%>%rename(mun_hosp=mun),all=T)%>%
  left_join(flows_count,by=c('mun_res','mun_hosp'))%>%
  mutate_at(.vars=c('share','share_icu'),.funs=function(x) {ifelse(is.na(x),0,x)})%>%
  rename(weight=share_icu)

dir_net_icu<-graph_from_data_frame(edges_icu%>%rename(),nodes,directed=TRUE)
nodes_cluster_icu<-nodes%>%
  mutate(cluster_fg=cluster_fast_greedy(as.undirected(dir_net_icu))$membership,
         label=reorder(paste0('Cluster: ',cluster_fg),cluster_fg))

# Map
  gph_icu=ggplot(nodes_cluster_icu%>%
               inner_join(shp,by=c('mun')),aes(text=name_muni))+
    geom_sf(aes(geometry=geom,fill=label),lwd=0.1,show.legend=F)+
    geom_sf(data=shp_centroid%>%filter(name_muni=='São Paulo')%>%ungroup,aes(geometry=centr))+
    theme_void()+
    theme(axis.line=element_blank(),
          plot.title=element_text(hjust = 0.5))+
    scale_fill_manual(values=rep(RColorBrewer::brewer.pal(n=12,name='Paired'),times=10))+
    labs(caption='')
  gph_plotly_icu<-ggplotly(gph_icu,tooltip=c('text','fill'))%>%
    layout(title=list(text = paste0('Figure 2: ICU SARS Hospitalization Clusters in 2021 in São Paulo, Brazil',
                                    '<br>',
                                    '<sup>',
                                    'Note: Colors repeat themselves in the legend. Interact with graph by hovering over map with mouse and clicking on clusters on the legend.',
                                    '</sup>')),
           margin=list(t=50))
  saveWidget(as_widget(gph_plotly_icu),"Z:/Temp/BR/fig2_map_covid21_icu.html")
