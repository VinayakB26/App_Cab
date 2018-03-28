ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    "))
  ),
  
  mainPanel(
    
    
    navbarPage("Your Cabs!",theme = "bootstrap.css",inverse = TRUE,
                tabPanel("Objective",fluidRow(textOutput("obj"),style="text-align:center")),
                tabPanel("Data Set",dataTableOutput("cab1")),
                tabPanel("Data Prediction",sidebarPanel (radioButtons("id","Travel Type",
                                                                      c("1.Long Distance" = "one",
                                                                        "2.Point to Point" = "two",
                                                                        "3.Hourly" = "three"
                                                                      )),style ="background: transparent;
                                                         border: 0px;" 
                                                         )
                         ,sidebarPanel( selectInput("vmd","Vehicle Model",choices = cabTrain1$vehicle_model_id),style ="background: transparent; border: 0px;" ),
                         sidebarPanel( selectInput("pid","Package",choices = cabTrain1$package_id),helpText("*Note : Choose Null for Travel Type 1 and 2*"),style ="background: transparent; border: 0px;")
                         ,sidebarPanel( selectInput("ob", " Onilne Booking",choices=cabTrain1$online_booking),style ="background: transparent; border: 0px;"),
                         sidebarPanel(selectInput("fad","From Area",choices=cabTrain1$from_area_id),style ="background: transparent; border: 0px;"),
                         sidebarPanel( selectInput("tad","To Area",choices=cabTrain1[which(is.na(cabTrain1$to_area_id)==F),c("to_area_id")]),style ="background: transparent; border: 0px;"),
                         sidebarPanel( dateInput("fd",label = "From Date",format = "mm/dd/yyyy"),style ="background: transparent; border: 0px;"),
                         sidebarPanel(timeInput("ft","From Time",seconds = FALSE),style ="background: transparent; border: 0px;"),
                         sidebarPanel( dateInput("td",label = "To Date",format = "mm/dd/yyyy"),style ="background: transparent; border: 0px;"),
                         sidebarPanel(timeInput("tt","To Time",seconds = FALSE),style ="background: transparent; border: 0px;"),
                         sidebarPanel( selectInput("fcd","From City",choices = cabTrain1[which(cabTrain1$from_city_id!="NULL"),c("from_city_id")]),style ="background: transparent; border: 0px;"),
                         sidebarPanel( selectInput("tcd","To City",choices=cabTrain1[which(cabTrain1$to_city_id!="NULL"),c("to_city_id")]),style ="background: transparent; border: 0px;"),
                         fluidRow(column(4,offset =1,actionButton("button", "Submit",style="background-color:#000;color:#FFF"))),fluidRow(column(8,offset=6,hidden( div(id="text_div",
                                                                                       textOutput("pred")))),style="font-family: 'Segoe UI Light';font-size:30px; color:#ad1d28")
                         
                         
                )
    )
  )
)

server <- function(input, output) 
{
  
  output$obj<-renderPrint({
    cat("The goal is to create a predictive model for classifying new bookings as to whether they will eventually get cancelled due to car unavailability")
  })
  
  output$cab1<-renderDataTable({
    cabTrain1
  })
  observeEvent(input$button,{
    toggle("text_div")
    
    output$pred<-renderPrint(
      {
        
        
        
        fl_1<-0
        flt_1<-0
        tl_1<-0
        tlt_1<-0
        
        id <- switch(input$id,
                     one = cabTrain_1,
                     two = cabTrain_2,
                     three = cabTrain_3
        )
        
        vmd<-0
        pid<-as.factor("NULL")
        tad<-0
        
        if(!is.null(id$vehicle_model_id))
        {
          vmd<-as.numeric(input$vmd)
        }
        if(!is.null(id$package_id))
        {
          pid<-as.factor(input$pid)
        }
        
        fad<-as.numeric(input$fad)
        i<-match(fad,cabTrain1$from_area_id)
        fl_1<-cabTrain1$from_long[i]
        flt_1<-cabTrain1$from_lat[i]
        fl_1<-as.character(fl_1)
        fl_1<-as.numeric(fl_1)
        flt_1<-as.character(flt_1)
        flt_1<-as.numeric(flt_1)
        
        
        if(!is.null(id$to_area_id))
        {
          tad<-as.numeric(input$tad)
          j<-match(tad,cabTrain1$to_area_id)
          tl_1<-cabTrain1$to_long[j]
          tlt_1<-cabTrain1$to_lat[j]
          tl_1<-as.character(tl_1)
          tl_1<-as.numeric(tl_1)
          tlt_1<-as.character(tlt_1)
          tlt_1<-as.numeric(tlt_1)
          
        }
        
        tcd<-as.numeric(input$tcd)
        
        ob<-as.numeric(input$ob)
        
        
        fd<-as.factor(input$fd)
        #print(fd)
        bc<-Sys.time()
        td<-as.factor(input$td)
        #print(td)
        td<-strptime(td,format = "%Y-%m-%d")
        #print(td)
        bc<-strptime(bc,format = "%Y-%m-%d %H:%M:%S")
        fd<-strptime(fd,format = "%Y-%m-%d")
        #print(fd)
        fm<-month(fd)
        fda<-weekdays(fd)
        bm<-month(bc)
        bd<-weekdays(bc)
        tm<-month(td)
        tda<-weekdays(td)
        bt<-as.POSIXlt(bc,format="%m/%d/%Y %H:%M")
        bt<-format(bt, format = "%H:%M")
        #print(input$ft)
        ft<-format(input$ft,format="%H:%M")
        #print(bt)
        #print(class(ft))
        
        
        {fti<-0
          
          if (ft >="06:00" && ft <="11:59")
          {
            
            fti<-"Morning"
          }
          else
            if (ft =="12:00")
            {
              fti<-"Noon"
            }
          else
            if (ft >="12:01" && ft <="17:00")
            {
              fti<-"Afternoon"
            }
          else
            if (ft >="17:01" && ft <="20:00")
            {
              fti<-"Evening"
            }
          else
            if (ft >="20:01" && ft <="23:59")
            {
              fti<-"Night"
            }
          else
            if (ft =="00:00")
            {
              fti<-"MidNight"
            }
          else
            if (ft >="00:01" && ft <="5:59")
            {
              fti<-"AfterMidnight"
            }
        }
        {bti<-0
          
          if (bt >="06:00" && bt <="11:59")
          {
            
            bti<-"Morning"
          }
          else
            if (bt =="12:00")
            {
              bti<-"Noon"
            }
          else
            if (bt >="12:01" && bt <="17:00")
            {
              bti<-"Afternoon"
            }
          else
            if (bt >="17:01" && bt <="20:00")
            {
              bti<-"Evening"
            }
          else
            if (bt >="20:01" && bt <="23:59")
            {
              bti<-"Night"
            }
          else
            if (bt =="00:00")
            {
              bti<-"MidNight"
            }
          else
            if (bt >="00:01" && bt <="05:59")
            {
              bti<-"AfterMidnight"
            }
        }
        fti<-as.factor(fti)
        bti<-as.factor(bti)
        #print(year(fd))
        #print(bti)
        #print(fti)
        #updateSelectInput()
        flag<-0
        if(year(fd)<year(bc) || fm<bm || day(fd)<day(bc) )
        {
          flag<-1
          writeLines("Cab Booking Not Possible \n Error: Wrong Date")
        }
        else
        if(fm>tm)
        {
          flag<-1
          writeLines("Cab Booking Not Possible \n Error: Wrong Date")
        }
        else
          if(fm==tm)
          {
            if(day(td)<day(fd))
            {
              flag<-1
              writeLines("Cab Booking Not Possible\n Error: Wrong Date")
            }
          }
            
        
          if(flag==0)
          {
          #  cat("cab booking possible")
            
            if(vmd!=0)
            {
              test1<-data.frame(vmd,fad,tcd,ob,fm,fda,bm,bd,tm,tda,bti)
              
              
              colnames(test1)<-c("vehicle_model_id","from_area_id","to_city_id","online_booking","from_month",
                                 "from_day","booking_month","booking_day","to_month","to_day","booking_time_interval")
              
              
              #t1<-read.csv("t1.csv")
              t1<-rbind(t1,test1)
              rownames(t1)<-1:nrow(t1)
              n1<-nrow(t1)
              
              #levels(t1$from_day)<-c(levels(t1$from_day),"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
              #levels(t1$booking_day)<-c(levels(t1$booking_day),"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
              #levels(t1$to_day)<-c(levels(t1$to_day),"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
              #levels(t1$booking_time_interval)<-c(levels(t1$booking_time_interval),"AfterMidnight","Afternoon","Evening","MidNight","Night","Noon","Morning")
              
              #print(test1$booking_time_interval)
              
              
              cc<-predict(rf_cab1,newdata = t1[n1,])
              cc<-as.character(cc)
              cc<-as.numeric(cc)
              
                   
              if(cc==0)
              {
                cat("Cab Booking Confirmed")
              }else
              {
                cat("Cab  Cancelled")
              }
            }
            else
              if(tad!=0)
              {
                d<-distm(c(fl_1, flt_1), c(tl_1, tlt_1), fun = distVincentyEllipsoid)
                
                { dt<-0
                  
                  if(d>=0 && d <= 5000)
                  {
                    dt<-"very short"
                  }
                  else
                    if (d>5000 && d<=15000)
                    {
                      dt<-"short"
                    }
                  else
                    if(d>15000 && d <=25000 )
                    {
                      dt<-"medium"
                    }
                  else
                    if(d>25000 && d <=35000 )
                    {
                      dt<-"long"
                    }
                  else
                    if(d>35000)
                    {
                      dt<-"very long"
                    }
                }
                dt<-as.factor(dt)
                bd<-as.factor("Sunday")
                bti<-as.factor("Morning")
                test2<-data.frame(fad,tad,ob,fm,fda,bm,bd,bti,fti,dt,d)
                
                
                colnames(test2)<-c("from_area_id","to_area_id","online_booking","from_month","from_day","booking_month"
                                   ,"booking_day","booking_time_interval","from_time_interval","distance_type","distance")
                
               # t2<-read.csv("t2.csv")
                t2<-rbind(t2,test2)
                rownames(t2)<-1:nrow(t2)
                n2<-nrow(t2)
                
                #levels(t2$from_day)<-c(levels(t2$from_day),"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
                #levels(t2$booking_day)<-c(levels(t2$booking_day),"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
                #levels(t2$booking_time_interval)<-c(levels(t2$booking_time_interval),"AfterMidnight","Afternoon","Evening","MidNight","Night","Noon","Morning")
                #levels(t2$from_time_interval)<-c(levels(t2$from_time_interval),"AfterMidnight","Afternoon","Evening","MidNight","Night","Noon","Morning")
                #levels(t2$distance_type)<-c(levels(t2$distance_type),"long","medium","short","very long","very short")
                
                
                #print(str(t2))
                
                #print(t2$distance)
                
                cc2<-predict(rf_cab2,newdata = t2[n2,])
                
                cc2<-as.character(cc2)
                cc2<-as.numeric(cc2)
                
                if(cc2==0)
                {
                 cat("Cab Booking Confirmed")
                }else
                {
                 cat("Cab Cancelled")
                }
                #j<-j+1
              }
            else
              if(pid!="NULL")
              {
                
                #print(class(pid))
                bm<-5
                bd<-as.factor("Sunday")
                
                test3<-data.frame(pid,fad,ob,fm,fda,bm,bd,bti,fti)
                
                colnames(test3)<-c("package_id","from_area_id","online_booking","from_month","from_day","booking_month"
                                   ,"booking_day","booking_time_interval","from_time_interval")
                
                #t3<-read.csv("t3.csv")
                t3<-rbind(t3,test3)
                rownames(t3)<-1:nrow(t3)
                n3<-nrow(t3)
                #t3$package_id<-as.factor(t3$package_id)
                #levels(t3$from_day)<-c(levels(t3$from_day),"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
                #levels(t3$booking_day)<-c(levels(t3$booking_day),"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
                #levels(t3$booking_time_interval)<-c(levels(t3$booking_time_interval),"AfterMidnight","Afternoon","Evening","MidNight","Night","Noon","Morning")
                #levels(t3$from_time_interval)<-c(levels(t3$from_time_interval),"AfterMidnight","Afternoon","Evening","MidNight","Night","Noon","Morning")
                #levels(t3$package_id)<-c(levels(t3$package_id),"1","2","3","4","5","6","7","NULL")
                
                
                #print(str(t3$package_id))
                
                #print(t3[n3,])
                #print(class(t3$package_id))
                
                cc3<-predict(rf_cab3,newdata = t3[n3,])
                #print(cc3)
                cc3<-as.character(cc3)
                cc3<-as.numeric(cc3)
                #print(cc3)
                if(cc3==0)
                {
                cat("Cab Booking Confirmed")
                }
                else
                {
                 cat("Cab Cancelled")
                }
              }
          }
      }
    )
  })
}
shinyApp(ui = ui, server = server)