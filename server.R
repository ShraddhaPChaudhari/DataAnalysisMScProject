
server <- shinyServer(function(input, output,session) {
 
  
#wordcloud---------------------------------------------------------------------------------------------
    wc_data<-reactive({
        input$update
        isolate({
            withProgress({
                setProgress(message = "Processing Corpus")
                wc_file<-input$wc
                if(!is.null(wc_file)){
                    wc_text<-readLines(wc_file$datapath)
                }
                else{
                    wc_text<-"The most common methods of suicide are hanging, pesticide self-poisoning, and firearms. Key interventions that have shown success in reducing suicides are restricting access to means; educating the media on responsible reporting of suicide; implementing programmes among young people to build life skills that enable them to cope with life stresses; and early identification, management and follow-up of people at risk of suicide."
                }
                wc_corpus<-Corpus(VectorSource(wc_text))
                wc_corpus_clean<-tm_map(wc_corpus,tolower)
                wc_corpus_clean<-tm_map(wc_corpus_clean,removeNumbers)
                wc_corpus_clean<-tm_map(wc_corpus_clean,removeWords,stopwords())
                wc_corpus_clean<-tm_map(wc_corpus_clean,stripWhitespace)
                wc_corpus_clean<-tm_map(wc_corpus_clean,stemDocument)
            })
        })
    })
    worldcloud_rep<-repeatable(wordcloud)  
    output$wcplot= renderPlot({
        withProgress({
            setProgress(message = "Creating wordcloud....")
            wc_corpus<-wc_data()
            wc_color<-brewer.pal(8,"Dark2")
            if(input$color == "Accent"){
                wc_color=brewer.pal(8,"Accent")
            }
            else{
                wc_color=brewer.pal(8,"Dark2")
            }
            wordcloud(wc_corpus,min.freq = input$wordfreq,max.words = input$maxword,colors=wc_color,random.order = input$random,rot.per =.30)
            })
      
    },bg="transparent") 
    output$intro1 <- renderText({"Van Gogh, Hemingway, Avicii, and other great souls committed suicide.
    Many people mourn and remember them even today. 
    Close to 800000 people die by suicide every year.
    That is one person every 40 seconds.
    While the link between suicide and mental disorders is well established in high-income countries, many suicides happen impulsively in moments of crisis, such as when facing life stresses such as financial problems, relationship break-up or chronic illness. 
    Suicide does not just occur in high-income countries, but is a global phenomenon in all regions of the world.
    In fact, over 79% of global suicides occurred in low- and middle-income countries in 2016.
    Is the situation getting better or worse over years? Which group of people is at the highest risk? 
    In this project, I compare socio-economic info with suicide rates by year and country,state to see where the world/India is heading."})
    
    
    output$intro2 <- renderText({
    "Dataset used in this project combines information available at World Health Organization (http://www.who.int/mental_health/suicide-prevention/en/), World Bank (http://databank.worldbank.org/data/source/world-development-indicators#), and United Nations Development Program (http://hdr.undp.org/en/indicators/137506).
    India's suicide data is also available on National Crime Bureau.
    More information on suicide prevention can be found on WHO website (https://www.who.int/health-topics/suicide#tab=tab_1).
    National suicide prevention hotline: 1-800-273-8255. "
    })
    
    #India--------------------------------------------------------------------------------------  
    
    ind<-read.csv("C:/Users/shraddha/Documents/Msc_Project/SuicideAnalysis/india.csv",header = T,sep = ',')
    ind_frame<-data.frame(ind)
    colnames(ind) <-c("State","Year","Type_code","Type","Gender","Age_group","Total")
    
    #India suicides -----------------------------------------------------------------------
    
    state<-reactive({
      updateSelectInput(session,inputId = 'select_state')
      x <- ind_frame%>%filter(State == input$select_state) %>% select(Gender,Total)%>% group_by(Gender)%>% summarise(total_all=sum(Total))%>%mutate(rs=sum(total_all), percent=round((total_all/rs)*100))
    })
    #gender-------------------------------------------------------------------------------
    colr<-c("dodgerblue4","mediumpurple3","goldenrod3","orangered4",
            "lightsalmon4","mistyrose4","palevioletred3","slateblue4","slateblue","slategray4","tan")
    
    output$gender<-renderPlotly({
      ggplotly(ggplot(state(),aes(x=Gender,y=percent,fill=Gender))+geom_bar(stat="identity")+scale_fill_manual(values=c("dodgerblue4","goldenrod3"))+geom_text(aes(label=percent))+theme(plot.background = element_rect(fill = "transparent", colour = NA)))
      })
    
    #year--------------------------------------------------------------------------------------
    output$year<-renderPlotly({
      ind$Year <- as.Date(as.character(ind$Year),format="%Y")
      temp <- ind %>% group_by(Year) %>% summarise(total_case=sum(Total)) 
      
      ggplotly(ggplot(temp,aes(Year,total_case,col="State"))+geom_line(size=1)+geom_point(size=2)+
                 theme(legend.position="none",axis.text.x=element_text(angle=90))+
                 xlab('Year')+ylab('Total Cases of Suicide')+ggtitle('Trend of Suicide Cases over 2006-2012'))
    })
    #age----------------------------------------------------------------------------------------
    colr<-c("dodgerblue4","mediumpurple3","goldenrod3","orangered4",
            "lightsalmon4","mistyrose4","palevioletred3","slateblue4","slateblue","slategray4","tan")
    
    output$age<-renderPlot({
      ind %>% select(Gender,Age_group,Total)%>% filter(!Age_group=="0-100+")%>% group_by(Gender,Age_group)%>% summarise(atot=sum(Total))%>% ggplot(aes(x=Age_group,y=atot,fill=Gender))+geom_bar(stat="identity",position="dodge")+
        scale_fill_manual(values=c("dodgerblue4","goldenrod3"))+labs(y="Count")
      
      pdata<-ind %>% select(Age_group,Total)%>% filter(!Age_group=="0-100+")%>%group_by(Age_group)%>% summarise(atot=sum(Total))
      pie3D(pdata$atot,labels=pdata$Age_group,explode=0.1,col=colr,main="Suicide and Age Groups ")
      
      
    })
    output$g1<-renderValueBox({
      a<-ind%>%select(Age_group,Total)%>%filter(Age_group=="0-14")%>%summarise(atot=sum(Total))
      valueBox("0-14",a,color = "light-blue")
      
    })
    output$g2<-renderValueBox({
      a<-ind%>%select(Age_group,Total)%>%filter(Age_group=="15-29")%>%summarise(atot=sum(Total))
      valueBox("15-29",a,color = "light-blue")
      
    })
    output$g3<-renderValueBox({
      a<-ind%>%select(Age_group,Total)%>%filter(Age_group=="30-44")%>%summarise(atot=sum(Total))
      valueBox("30-44",a,color = "light-blue")
      
    })
    output$g4<-renderValueBox({
      a<-ind%>%select(Age_group,Total)%>%filter(Age_group=="45-59")%>%summarise(atot=sum(Total))
      valueBox("45-59",a,color = "light-blue")
      
    })
    output$g5<-renderValueBox({
      a<-ind%>%select(Age_group,Total)%>%filter(Age_group=="60+")%>%summarise(atot=sum(Total))
      valueBox("60+",a,color = "light-blue")
      
    })
    
    #education------------------------------------------------------------------------
    output$education<-renderPlotly({
      colr<-c("dodgerblue4","mediumpurple3","goldenrod3","orangered4",
              "lightsalmon4","mistyrose4","palevioletred3","slateblue4","slateblue","slategray4","tan")
      
      sc_type<-ind_frame %>% filter(Type_code =="Education_Status")%>% select(Gender,Total,Type)%>% group_by(Gender,Type)%>% summarise(ttotal=sum(Total))
      sc_type %>% ggplot(aes(x=str_sub(Type,1,15),y=ttotal,fill=Type))+geom_boxplot()+scale_fill_manual(values=colr)+theme(legend.position = "bottom",axis.text.x = element_text(angle=90))+labs(x="Education Level",y="count")
       
    })
    output$education1<-renderPlotly({
      sc_type<-ind_frame %>% filter(Type_code =="Education_Status")%>% select(Gender,Total,Type)%>% group_by(Gender,Type)%>% summarise(ttotal=sum(Total))
      sc_type %>% ggplot(aes(x=str_sub(Type,1,15),y=ttotal,fill=Gender))+geom_bar(stat="identity",position="fill")+scale_fill_manual(values=colr)+theme(legend.position = "bottom",axis.text.x = element_text(angle=90))+labs(x="Education Level",y="count")
      
    })
    
    
    #social--------------------------------------------------------------------------------
    
    output$social<-renderPlotly({
      colr<-c("dodgerblue4","mediumpurple3","goldenrod3","orangered4",
              "lightsalmon4","mistyrose4","palevioletred3","slateblue4","slateblue","slategray4","tan")
      
      ss_type<-ind_frame %>% filter(Type_code =="Social_Status")%>% select(Gender,Total,Type,Age_group)%>% group_by(Gender,Type,Age_group)%>% summarise(ttotal=sum(Total))
      ss_type%>%ggplot(aes(x=Type,y=ttotal,fill=Type))+geom_boxplot()+scale_fill_manual(values=colr)+theme(legend.position = "bottom",axis.text.x = element_text(angle=90))+labs(x="Social Status" ,y="count")
      
    })
    output$prof<-renderPlotly({
      
      pp_type<-ind_frame %>% filter(Type_code =="Professional_Profile")%>% select(Gender,Total,Type,Age_group)%>% group_by(Gender,Type,Age_group)%>% summarise(ttotal=sum(Total))
      pp_type %>% ggplot(aes(x=str_sub(Type,1,20),y=ttotal,fill=Gender))+geom_bar(stat="identity",position="fill")+scale_fill_manual(values=colr)+theme(legend.position = "bottom",axis.text.x = element_text(angle=90))+labs(x="Social Status",y="Percent")
    
    })
    #cause -------------------------------------------------------------------------------
    
    output$causes<-renderPlotly({
      colr<-c("dodgerblue4","mediumpurple3","goldenrod3","orangered4",
              "lightsalmon4","mistyrose4","palevioletred3","slateblue4","slateblue","slategray4","tan")
      
      ind_frame %>% filter(Type_code=="Causes" & Type %in% c("Failure in Examination","Family Problems","Other Prolonged Illness","Unemployment","Dowry Dispute","Poverty","Insanity/Mental Illness"))%>%select(Year,Total,Type)%>% group_by(Year,Type)%>%summarise(ytot=sum(Total))%>%  ggplot(aes(x=factor(Year),y=ytot,color=Type,group=Type))+geom_line(size=1)+scale_color_manual(values=colr)+
        theme(legend.position = "bottom",axis.text.x = element_text(angle=65,vjust=0.5))+labs(x="Year",y="Count")+geom_point(size=2)
      
    })
    output$cause1<-renderPlotly({
      
      data_cy <- ind_frame%>%filter(Type_code=="Causes")%>%group_by(Type)%>%summarize(Total=sum(Total))
      
      
      data_cy$Type <- factor(data_cy$Type,levels =data_cy$Type[order(data_cy$Total)])
      
      
      ggplot(data_cy, aes(x=Type,y=Total,col=as.factor(Type),fill=as.factor(Type)))+
        geom_bar(stat = "identity",width = .75)+
        coord_flip()+
        theme(axis.line = element_line(color = "blue",size=1.25))+
        theme(legend.position="none")+
        theme(panel.background=element_blank())+
        scale_x_discrete("Causes ") + 
        scale_y_continuous(breaks=seq(0,350000,50000),name = "Nos of Suicides")+
        ggtitle("Suicides causes in India")
    })
    #means-adopted--------------------------------------------------------------------------
    output$mean<-renderPlotly({
      colr<-c("dodgerblue4","mediumpurple3","goldenrod3","orangered4",
              "lightsalmon4","mistyrose4","palevioletred3","slateblue4","slateblue","slategray4","tan")
      
      ma_type<-ind_frame %>% filter(Type_code =="Means_adopted") %>%group_by(Type,Gender,Age_group)%>%summarize(mtot=sum(Total))
      ma_type%>%ggplot(aes(x=Type,y=mtot,fill=Gender))+geom_bar(stat="identity",position="dodge")+scale_fill_manual(values=colr)+theme(legend.position = "bottom",axis.text.x = element_text(angle=90))+labs(x="Means Adopted",y="Count")
      
      ma_type%>%ggplot(aes(x=Type,y=mtot,fill=Age_group))+geom_bar(stat="identity",
                                                                   position="dodge")+scale_fill_manual(values=colr)+theme(legend.position = "bottom",axis.text.x = element_text(angle=90))+labs(x="Means Adopted",y="Count")
      
    })
    
    
    
    #topbottom-------------------------------------------------------------------------------
    
    output$topbot1<-renderPlotly({
      colr<-c("dodgerblue4","mediumpurple3","goldenrod3","orangered4",
              "lightsalmon4","mistyrose4","palevioletred3","slateblue4","slateblue","slategray4","tan")
      
      sc1<-ind_frame%>%filter(!State %in% c("Total (All India)","Total (States)","Total (Uts)"))%>% select(State,Year,Total) %>% group_by(State)%>% summarise(tot=sum(Total)) %>% arrange(desc(tot))%>%head(10)%>%
        ggplot(aes(x=factor(State,level=State),y=tot,color=State))+geom_point(size=4)+geom_segment(aes(xend=State,y=0,yend=tot),size=2)+theme(legend.position="none",axis.text.x=element_text(angle=90))+scale_color_manual(values=colr)+geom_text(aes(label=tot),vjust=0.3)+labs(x="State - More Suicides")
    })
    output$topbot2<-renderPlotly({
      colr<-c("dodgerblue4","mediumpurple3","goldenrod3","orangered4",
              "lightsalmon4","mistyrose4","palevioletred3","slateblue4","slateblue","slategray4","tan")
      
      sc2<- ind_frame%>%filter(!State %in% c("Total (All India)","Total (States)","Total (Uts)"))%>% select(State,Year,Total) %>% group_by(State)%>% summarise(tot=sum(Total)) %>% arrange(desc(tot))%>%tail(10)%>%
        ggplot(aes(x=factor(State,level=State),y=tot,color=State))+geom_point(size=4)+geom_segment(aes(xend=State,y=0,yend=tot),size=2)+theme(legend.position="none",axis.text.x=element_text(angle=90))+scale_color_manual(values=colr)+geom_text(aes(label=tot),vjust=0.3)+labs(x="State - Less Suicides")
      
    })
   #state
    output$statewise<-renderHighchart({
      data_cs <- ind_frame%>%filter(Type_code=="Causes")%>%group_by(State)%>%summarize(Total=sum(Total))
     hchart(data_cs,type="treemap",hcaes(x = State, value = Total, color = Total)) %>%
        hc_title(text = "No of Suicides in india by states") %>%
        hc_add_theme(hc_theme_google()) %>%
        hc_credits(enabled = TRUE, text = "Data Source: National Crime Records Bureau (NCRB), Govt of India ", style = list(fontSize = "12px")) %>%
        hc_legend(enabled = FALSE)
    }) 
    
 # Global------------------------------------------------------------------------------------------
    data<-read.csv("C:/Users/shraddha/Documents/Msc_Project/SuicideAnalysis/master1.csv",header = T,sep=',')
    suicide<- c("all_country", "year", "sex", "age", "suicide_no", 
                "population", "suicide_per_100k", "country_year", "HDI_for_year", "gdp_for_year", 
                "gdp_per_capita", "generation")
    
    data$continent <- countrycode(sourcevar = data[, "all_country"],
                                  origin = "country.name",
                                  destination = "continent")
    data1<-data.frame(data)
    
    #Global Suicides------------------------------------------------------------------------------
    
    #countrywise suicides---------------------------------------------------------------------------------
    data<-reactive({
      updateSelectInput(session,inputId = 'select_country')
      x <- data1%>%filter(all_country == input$select_country) 
      
    })
    
    output$global_plot <- renderPlotly({
      p<-ggplot(data(),aes(x=year,y=suicide_no,col=input$select_country)) + 
        geom_bar(stat="identity", fill="steelblue")
      ggplotly(p)
      
      
    })
    output$plots<-renderPlotly({
      data1 %>% 
        group_by(all_country) %>%
        summarize(suicide_per_100k=sum(suicide_no)*100000/sum(population)) %>%
        top_n(25) %>%
        ggplot(aes(reorder(all_country, suicide_per_100k), suicide_per_100k)) +
        geom_bar(stat="identity", fill="deepskyblue2", color="navy") +
        coord_flip() +
        labs(x="Country", y="Suicide rate per 100k population") +
        ggtitle("Suicide Rates by Country")
      
    })
    
    #sex-wise suicides-----------------------------------------------------------------------------------------
    new_data<-reactive({
      updateSelectInput(session,inputId = 'select_country')
      x <- data1%>%filter(year == input$select_year)
    })
    output$suicide_rate<-renderPlotly({
      p<-ggplot(new_data(),aes(x=sex,y=suicide_no,col=input$select_year )) +
        geom_line(fill="deepskyblue2", color="navy") 
      
      ggplotly(p)
    })
    #age-wise suicides-----------------------------------------------------------------------------------------
    age<-reactive({
      updateSelectInput(session,inputId = 'select_age')
      x <- data1%>%filter(age == input$select_age) 
    })
    output$suicide_by_age<-renderPlotly({
      p<-ggplot(age(),aes(x=sex,y=suicide_no,col=input$select_age)) +
        geom_bar(fill="mediumpurple3",stat = "identity") +theme_bw()
      
      ggplotly(p)
    })
    
    #top countries + bottomcountries  ---------------------------------------------------------------------
    output$top5more <- renderPlotly({p<-data1 %>% group_by(year,all_country) %>% summarise(suicide_per_100k = sum(suicide_no)/sum(population)*100000) %>% filter(year== input$year2) %>% slice_max(order_by=suicide_per_100k,n=5) %>% ggplot(aes(x=all_country,y=suicide_per_100k,fill=all_country))+geom_col()+scale_x_discrete(guide = guide_axis(n.dodge = 2))+labs(x="",y="Suicides Per 100k",title = "Countries with the highest suicide rates")+theme(plot.title=element_text(hjust=0.5),legend.position = "none")
    ggplotly(p)})
    output$top5less <- renderPlotly({data1 %>% group_by(year,all_country) %>% summarise(suicide_per_100k = sum(suicide_no)/sum(population)*100000) %>% filter(year==input$year2) %>% slice_min(order_by=suicide_per_100k,n=5) %>% ggplot(aes(x=all_country,y=suicide_per_100k,fill=all_country))+geom_col()+labs(x="",y="Suicides Per 100k",title = "Countries with the lowest suicide rates")+scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme(plot.title=element_text(hjust =0.5 ),legend.position = "none")})
    
    #Men Vs Women ---------------------------------------------------------------------------------------------   
    output$menwomen<-renderPlot({
      pie.age <- data1 %>%
        select(age, suicide_no, population) %>%
        group_by(age) %>%
        summarise(suicide_capita = round((sum(suicide_no) / sum(population)) * 100000, 2))
      
      suicides.by.age.pie <- ggplot(pie.age, aes(x = '', y = suicide_capita, fill = age))
      
      suicides.by.age.pie + 
        geom_bar(stat = 'identity') + 
        coord_polar('y', start = 0) +
        geom_text(aes(label = suicide_capita), position = position_stack(vjust = 0.5), colour = 'Black')
       })
    
    #generation wise ------------------------------------------------------------------------------------------------ -  
    output$generation<-renderPlotly({
      ggplotly(data1 %>% group_by(year,generation) %>% summarise(suicide_per_100k=sum(suicide_no)/sum(population)*100000) %>% filter(year <=2015)%>% ggplot(aes(x=year,y=suicide_per_100k,colour=generation))+facet_grid(generation ~.,scale = "free_y")+geom_line()+geom_point()+theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = T)+labs(x="Suicides Per 100k",y="Suicides Per 100k",title="Evolution of the Suicides (per 100k) Numbers - Generations"))
    })
    #continent wise-----------------------------------------------------------------------------------------------------   
    output$con<-renderPlotly({
      data1 %>% 
        group_by(all_country, continent) %>%
        summarize(avg_suicide_rate=mean(suicide_per_100k)/100) %>% 
        ggplot(aes(continent, avg_suicide_rate)) +
        geom_boxplot( fill="deepskyblue2", color="navy") +
        # coord_flip() +
        labs(x="Continent", y="Suicide rate per 100k population") +
        ggtitle("Suicide rate by Continent")
      
    })
    output$con1<-renderPlotly({
      continent.sex.tibble <- data1 %>%
        select(continent, sex, suicide_no, population) %>%
        group_by(continent, sex) %>%
        summarise(suicide_capita = round((sum(suicide_no) / sum(population)) * 100000, 2))
      
      suicides.continent.by.sex <- ggplot(data = continent.sex.tibble,
                                          aes(x = continent, y = suicide_capita, group = sex, fill = sex))
      suicides.continent.by.sex + 
        geom_bar(stat = 'identity', position = position_dodge()) +
        labs(title = 'Suicides by continent and sex',
             subtitle = '1985 - 2015',
             x = 'Continent',
             y = 'Suicides per 100k population') +
        theme(axis.title = element_text(size = 10, colour = 'Black'),
              plot.title = element_text(size = 10, colour = 'Black', hjust = 0.5),
              
              plot.subtitle = element_text(size = 10, colour = 'Black', hjust = 0.5),
              
              axis.text = element_text(size = 10))
      
    })
    #animation
    output$animation<-renderPlotly({
      p <- ggplot(data1, aes(gdp_per_capita, suicide_per_100k, color = continent)) +
           geom_point(aes(size = population, frame = year, ids = all_country)) +
           scale_x_log10()
      ggplotly(p)
      
      
    })
    
    #Indicators---------------------------------------------------
    
    country_year <- data1 %>%
      group_by(country_year) %>%
      summarize(suicide_per_100k = (sum(suicide_no)/sum(population))*100000) %>%
      ungroup()
    
    eco <- data1 %>%
      dplyr::select(all_country, year, country_year:gdp_per_capita) %>%
      unique() %>%
      rename(country = all_country, gdp_per_capita = gdp_per_capita)
    
    country_eco <- inner_join(country_year, eco)
    country_eco$continent <- countrycode(country_eco$country, "country.name", "continent")
    
    ##GDP
    continent_eco_gdp <- country_eco %>%
      group_by(year, country) %>%
      na.omit(gdp_per_capita) %>%
      summarise(pop = mean(population), suicide_per_100k = mean(suicide_per_100k), gdp_per_capita = mean(gdp_per_capita)) %>%
      ungroup() 
    continent_eco_gdp$continent <- countrycode(continent_eco_gdp$country, "country.name", "continent")
    country_eco_gdp <- continent_eco_gdp %>%
      group_by(country) %>%
      summarise(suicide_per_100k = mean(suicide_per_100k), gdp_per_capita = mean(gdp_per_capita))
    country_eco_gdp$continent <- countrycode(country_eco_gdp$country, "country.name", "continent")
    country_eco_gdp
    output$gdp_plot <- renderPlot({
      ggplot(country_eco_gdp, aes(gdp_per_capita, suicide_per_100k)) +
        geom_point(aes(color = continent), show.legend = T) +
        geom_smooth(method = "lm", formula = y~x) +
        ggtitle("Suicides per 100k population") +
        ylab("GDP versus suicides per capita") +
        xlab("GDP per capita") + 
        annotate(geom="text", x=65000, y=32, label="y=17353.5+182.2x, r^2=0.006415",
                 color="red")
    })
    
    ##HDI
    continent_eco_hdi <- country_eco %>%
      group_by(year, country) %>%
      na.omit(HDI.for.year) %>%
      summarise(pop = mean(population), suicide_per_100k = mean(suicide_per_100k), HDI = mean(HDI_for_year)) %>%
      ungroup() 
    country_eco_hdi <- continent_eco_hdi %>%
      group_by(country) %>%
      summarise(suicide_per_100k = mean(suicide_per_100k), HDI= mean(HDI))
    country_eco_hdi$continent <- countrycode(country_eco_hdi$country, "country.name", "continent")
    
    output$hdi_plot <- renderPlot({
      ggplot(country_eco_hdi,aes(HDI, suicide_per_100k)) +
        geom_point(aes(color = continent)) +
        geom_smooth(method = "lm", formula = y~x) +
        ggtitle("HDI versus suicides per 100k population") +
        ylab("Suicides per capita") +
        xlab("Human Development Index") +
        annotate(geom="text", x=0.85, y=35, label="y=0.740259+0.002516x, r^2=0.06512",
                 color="red")
    })
    
    output$gdp_text <- renderText({
       "Since most countries locate in the bottom left corner, disregarding a few data points might produce a linear model with bigger r squared."
    })
    output$hdi_text <- renderText({
      "From simple observation and calculation, the increase in HDI can better explain the increase in suicides rates.
   However, it is interesting, since a higher HDI implies a better development of a country. 
  (HDI considers life expectancy, education, and economics)." 
    })
    
#time-series---------------------------------------------------------------------------
    tsdata<-ts(data1$suicide_per_100k,frequency = 12,start=2001,end = 2015)
    arima1<-auto.arima(tsdata)
    forecast1<-forecast(arima1,h=17)
   
    output$times<-renderPlot({
      plot(forecast1,col="darkblue")
    })
   # output$acc1<-renderPrint({
   #   accuracy(forecast1)
    #})
    tsdata1<-ts(ind_frame$Total,frequency = 12,start=2001,end = 2012)
    ses2<-ses(tsdata1)
    forecast2<-forecast(ses2)
    
    output$indf<-renderPlot({
      plot(forecast2,col="darkblue")
    })
    # output$acc2<-renderPrint({
     # accuracy(forecast2)
    #})    
#maps--------------------------------------------------------------------------------
    mapData<-read.csv("C:/Users/shraddha/Documents/Msc_Project/SuicideAnalysis/master1.csv",header = T,sep=',')
    head(mapData)
    mapData<-aggregate(mapData$suicide_per_100k ,by=list(all_country=mapData$all_country),FUN=sum)
    colnames(mapData)[colnames(mapData)=="x"]<-"Global_suicides"
    WorldSuicide<-joinCountryData2Map(mapData,nameJoinColumn = "all_country",joinCode="NAME")
    #set color
    color<-RColorBrewer::brewer.pal(10,'Spectral')
    output$mymap<-renderPlot({
      mapCountryData(WorldSuicide,
                     mapTitle = "Global Suicides",
                     catMethod='fixedwidth',
                     colourPalette=color,
                     missingCountryCol='white',
                     numCats=100)
    },bg="transparent")
    #india map--------------------------------------------------------------
    states_shape = readShapeSpatial("C:/Users/shraddha/Documents/IND_adm1.shp")
    rate<-read.csv("C:/Users/shraddha/Documents/Msc_Project/SuicideAnalysis/rate.csv")
    fortify_shape = fortify(states_shape, region = "ID_1")
    Merged_data = merge(fortify_shape, rate, by="id", all.x=TRUE)
    Map_plot = Merged_data[order(Merged_data$order), ]
    
    output$indiamap<-renderPlotly({
      ggplot() +geom_polygon(data = Map_plot,aes(x = long, y = lat, group = group, fill = rate),
                      color = "black", size = 0.5) +coord_map()+theme(plot.background = element_rect(fill = "transparent", colour = NA))
    })
    
  #regression
    output$re<-renderText({
      "The following graph shows predictions from the linear regression model and the random forests model compared to the true values of suicide rates in the test dataset for each year. The plot shows that annual predictions generated by random forests model are closer to the true value of suicide rates than those preducted by the linear regression model."
      
    })
    
    vars  <- c("continent", "population", "all_country", "sex", "year", "age", "gdp_per_capita")
    outcome <- "suicide_rate_log"
    (fmla <- as.formula(paste(outcome, "~", paste(vars, collapse = " + "))))
    
    # Variable transformation
    suicide <- data1 %>%
      mutate(suicide_rate_log=log(1+suicide_per_100k))
    
    
    ### Create Training and Testing sets
    #Training and testing datasets are created. Testing set is 20% of the entire dataset.
    
    # Split to training and testing datasets
    set.seed(1, sample.kind="Rounding")
    test_index <- createDataPartition(y = suicide$suicide_rate_log, times = 1, 
                                      p = 0.2, list = FALSE)
    train <- suicide[-test_index,]
    test <- suicide[test_index,]
    
    
    ### Train the Model
    #### Linear Regression
    
    # Linear regression
    lm1 <- train %>% 
      lm(fmla, data=.)
    
    #### Random Forests
    
    # Random forests
    set.seed(1, sample.kind="Rounding")
    rf1 <- ranger(fmla, # formula 
                  train, # data
                  num.trees = 500, 
                  respect.unordered.factors = "order",
                  seed = 1)
    
    
    # Generate predictions using the test data
    test$lm <- predict(lm1, newdata = test)
    test$rf <- predict(rf1, test)$predictions
    
    
    # Calculate RMSE
    case1 <- test %>% gather(key=model, value=log_pred, lm, rf) %>%
      mutate(pred=exp(log_pred),
             residuals=suicide_per_100k-pred) %>%
      group_by(model) %>%
      summarize(rmse=sqrt(mean(residuals^2)))
    
    
    
    a<-test %>% mutate(lm=exp(lm), rf=exp(rf)) %>%
      gather(key=valuetype, value=rate, suicide_per_100k, lm, rf) %>%
      mutate(suicides=rate*population/100000) %>%
      group_by(year, valuetype) %>%
      mutate(rate_year=sum(suicides)*100000/sum(population)) %>%
      ggplot(aes(year, rate_year, col=valuetype)) +
      geom_line() +
      geom_point(size = 2) +
      scale_x_continuous(breaks = seq(1985, 2016, 2)) +
      theme(axis.text.x = element_text(angle = 45))
   
     output$linearR1<-renderPlotly({
      a
      
    })
     
    #twitter
     consumerKey='YOIhqKq6h8LFHlnVkMOLoKfP0'
     consumerSecret='rAtocGOvlxGIQCZMlhuWU4C0AQqVeUux8P4lrd2R4qGWeG9GTF'
     accessToken='1300727522573787138-NrnvOOntkaNGnz4WkwfQHgcAoYmHlT'
     accessTokenSecret='WdO6DNDhcPUgrqecmGnB6Wn4LrwmiMeNrnH7iasmtMZna'
     setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
     #Adding '#' before the word for Twitter searching purpose
     hashtag <-reactive({
       req(input$word)
       paste("#",input$word)
     })
     
     #Collecting data from Twitter using the input word
     tweets <- reactive({
       search_tweets(hashtag(), input$n , include_rts = FALSE)
     })
     
     #Unlist hashtags vector
     hashtags_list <- reactive({tweets()$hashtags %>%
         unlist()  %>%   #Collapse the hashtag lists into one long list
         tolower() }) #Convert text to lowercase
     
     #Make a frequency table of hashtags
     hashtags_count <- reactive({table(hashtags_list())})
     
     #Transform to a data frame
     hashtags_df <- reactive({cbind.data.frame(tags=names(hashtags_count()),count=as.integer(hashtags_count()))})
     
     #Sort the table in decending order
     table_sort<-reactive({
       hashtags_df()[order(hashtags_df()$count,decreasing = T),]
     })
     
     # Create the wordcloud from the hashtag
     output$wc2 <- renderWordcloud2({
       hashtags_df() %>%  #create a dataframe from the hashtags table
         filter(hashtags_df()$tags != input$word) %>% #filter out the hashtag term itself from the wordcloud
         wordcloud2(.)
     }) #make the wordcloud
    
})
