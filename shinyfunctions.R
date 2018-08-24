library(compiler)
# ##############################################################################
#Side by Side View
side.rgl.3d <- function(num.raw.col,x,y,z,ext.x,ext.y,ext.z,x.offset,y.offset,y.offset.2,x.vector,treatment.array,
                        treatment.array.1,treatment.array.2,
                        colorlut,zlim,treat.range,width.offset,session,side.heigth.offset,
                        raw.titles,label.marker,colorlut.s,zlim.s,colorlut.dp,zlim.dpt,colorlut.rh,zlim.rh,
                        colorlut.pt,zlim.ppt,colorlut.ws,zlim.ws,colorlut.vp,zlim.vpd,raw.yr.titles,l.choices,
                        zlim.ppt.sum,colorlut.pt.s) 
{
progress <- shiny::Progress$new(session, min=1,max=num.raw.col)
on.exit(progress$close())
progress$set(message = 'Loading 3D plots now...')
  
z.yr.title.offset.1 <- as.numeric(1)#seq(1,length(raw.yr.titles))
z.yr.title.offset.2 <- as.numeric(1)#seq(1,length(raw.yr.titles))
z.title.offset <- as.numeric(-100 * ext.z)
  
show("length of raw titles in side function")
show(length(raw.titles))

for(c in 1:length(l.choices))
{
for(d in 1:length(raw.titles))
{
#show("Plot #")
show(d)
#show(raw.titles[d])
progress$set(value = d)
##############################################################################
if(length(l.choices) > 0 & c == 1){
z <- ext.z * treatment.array[,,d]
x.vector[,,d] <- as.numeric((x - x.offset)) 
##############################################################################
if(raw.titles[d] == "AT"){col <- colorlut[ z - zlim[1] + 1]
}else if(raw.titles[d] == "Dew_Point"){col <- colorlut.dp[ z - zlim.dpt[1] + 1]
}else if(raw.titles[d] == "RH_Percent"){col <- colorlut.rh[ z - zlim.rh[1] + 1] 
}else if(raw.titles[d] == "VPD"){col <- colorlut.vp[ z - zlim.vpd[1] + 1]
}else if(raw.titles[d] == "ppt_mm"){
  z <- 2 * ext.z * treatment.array[,,d]
  col <- colorlut.pt[ z - zlim.ppt[1] + 1]
}else if(raw.titles[d] == "ppt_mm_sum"){col <- colorlut.pt.s[ z - zlim.ppt.sum[1] + 1]
}else if(raw.titles[d] == "Soil4in_C"){col <- colorlut[ z - zlim[1] + 1]
}else if(raw.titles[d] == "Soil8in_C"){col <- colorlut[ z - zlim[1] + 1]
}else if(raw.titles[d] == "Solar_WM2"){col <- colorlut.s[ z - zlim.s[1] + 1]
}else if(raw.titles[d] == "ws2m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]
}else if(raw.titles[d] == "ws10m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]}
##############################################################################
if(length(l.choices) > 0 & c == 1){
rgl.surface(x.vector[,,d], y, z , color=col, alpha=1, front="fill", back="fill")
y.offset.2 <- y.offset.2 + 1
x.offset <- x.offset + (ext.x * side.heigth.offset)
}}
##############################################################################
if(length(l.choices) > 1 & c == 2){
x.vector[,,d] <- as.numeric((x - x.offset))
z <- ext.z * treatment.array.1[,,d]
##############################################################################
if(raw.titles[d] == "AT"){col <- colorlut[ z - zlim[1] + 1]
}else if(raw.titles[d] == "Dew_Point"){col <- colorlut.dp[ z - zlim.dpt[1] + 1]
}else if(raw.titles[d] == "RH_Percent"){col <- colorlut.rh[ z - zlim.rh[1] + 1] 
}else if(raw.titles[d] == "VPD"){col <- colorlut.vp[ z - zlim.vpd[1] + 1]
}else if(raw.titles[d] == "ppt_mm"){
  z <- 2 * ext.z * treatment.array.1[,,d]
  col <- colorlut.pt[ z - zlim.ppt[1] + 1]
}else if(raw.titles[d] == "ppt_mm_sum"){col <- colorlut.pt.s[ z - zlim.ppt.sum[1] + 1]
}else if(raw.titles[d] == "Soil4in_C"){col <- colorlut[ z - zlim[1] + 1]
}else if(raw.titles[d] == "Soil8in_C"){col <- colorlut[ z - zlim[1] + 1]
}else if(raw.titles[d] == "Solar_WM2"){col <- colorlut.s[ z - zlim.s[1] + 1]
}else if(raw.titles[d] == "ws2m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]
}else if(raw.titles[d] == "ws10m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]}
##############################################################################
rgl.surface(x.vector[,,d], y, z , color=col, alpha=1, front="fill", back="fill")
y.offset.2 <- y.offset.2 + 1
x.offset <- x.offset + (ext.x * side.heigth.offset)
}
##############################################################################
if(length(l.choices) > 2 &  c == 3){
x.vector[,,d] <- as.numeric((x - x.offset))
z <- ext.z * treatment.array.2[,,d]
##############################################################################
if(raw.titles[d] == "AT"){col <- colorlut[ z - zlim[1] + 1]
}else if(raw.titles[d] == "Dew_Point"){col <- colorlut.dp[ z - zlim.dpt[1] + 1]
}else if(raw.titles[d] == "RH_Percent"){col <- colorlut.rh[ z - zlim.rh[1] + 1] 
}else if(raw.titles[d] == "VPD"){col <- colorlut.vp[ z - zlim.vpd[1] + 1]
}else if(raw.titles[d] == "ppt_mm"){
  z <- 2 * ext.z * treatment.array.2[,,d]
  col <- colorlut.pt[ z - zlim.ppt[1] + 1]
}else if(raw.titles[d] == "ppt_mm_sum"){col <- colorlut.pt.s[ z - zlim.ppt.sum[1] + 1]
}else if(raw.titles[d] == "Soil4in_C"){col <- colorlut[ z - zlim[1] + 1]
}else if(raw.titles[d] == "Soil8in_C"){col <- colorlut[ z - zlim[1] + 1]
}else if(raw.titles[d] == "Solar_WM2"){col <- colorlut.s[ z - zlim.s[1] + 1]
}else if(raw.titles[d] == "ws2m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]
}else if(raw.titles[d] == "ws10m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]}
##############################################################################
rgl.surface(x.vector[,,d], y, z, color=col, alpha=1, front="fill", back="fill")
y.offset.2 <- y.offset.2 + 1
x.offset <- x.offset + (ext.x * side.heigth.offset)
}
##############################################################################

# if(label.marker == 1)text3d(color = "black", x = (((96/2) * -1) - ((d-1)*side.heigth.offset))*ext.x,
#                                                   y = 0, z = z.title.offset, text = raw.titles[d])
# if((label.marker >= 1) & (is.na(raw.yr.titles[d]) == FALSE))
# {
#   text3d(color = "black", x = 200,y = 0,z = ((182*z.yr.title.offset.2) + ((z.yr.title.offset.1-1)*width.offset))*ext.y,
#                                           text = raw.yr.titles[d])
#   z.yr.title.offset.1 <- z.yr.title.offset.1 + 1
#   z.yr.title.offset.2 <- z.yr.title.offset.2 + 2
# }

# y.offset.2 <- y.offset.2 + 1
# x.offset <- x.offset + (ext.x * side.heigth.offset)
    
if(y.offset == y.offset.2) 
{
  y <- y + (ext.y * treat.range) + width.offset
  #z <- ext.z * treatment.array[,,d]
  x.offset <- as.numeric(0)
  y.offset.2 <- as.numeric(0)
  if(label.marker != 0)label.marker <- as.numeric(2)
}   
}#End of for loop of rgl scene
y.offset.2 <- as.numeric(0)
}#End of l.choices
}
##############################################################################
#Side by Side View
# side.rgl.3d <- function(num.raw.col,x,y,z,ext.x,ext.y,ext.z,x.offset,y.offset,y.offset.2,x.vector,treatment.array,
#                         treatment.array.1,treatment.array.2,
#                         colorlut,zlim,treat.range,width.offset,session,side.heigth.offset,
#                         raw.titles,label.marker,colorlut.s,zlim.s,colorlut.dp,zlim.dpt,colorlut.rh,zlim.rh,
#                         colorlut.pt,zlim.ppt,colorlut.ws,zlim.ws,colorlut.vp,zlim.vpd,raw.yr.titles,l.choices,
#                         zlim.ppt.sum,colorlut.pt.s) 
# {
#   progress <- shiny::Progress$new(session, min=1,max=num.raw.col)
#   on.exit(progress$close())
#   progress$set(message = 'Loading 3D plots now...')
#   
#   z.yr.title.offset.1 <- as.numeric(1)#seq(1,length(raw.yr.titles))
#   z.yr.title.offset.2 <- as.numeric(1)#seq(1,length(raw.yr.titles))
#   z.title.offset <- as.numeric(-100 * ext.z)
#   
# #   show("length of raw titles in side function")
# #   show(length(raw.titles))
#   
#   for(c in 1:length(l.choices))
#   {
#     for(d in 1:length(raw.titles))
#     {
#       show("Plot #")
#       show(d)
#       #show(raw.titles[d])
#       progress$set(value = d)
#       ##############################################################################
#       #if(length(l.choices) > 0 & c == 1){
# if(length(l.choices) > 1){
# z <- ext.z * treatment.array[,,d]
# x.vector[,,d] <- as.numeric((x - x.offset)) 
# ##############################################################################
#         if(raw.titles[d] == "AT"){col <- colorlut[ z - zlim[1] + 1]
#         }else if(raw.titles[d] == "Dew_Point"){col <- colorlut.dp[ z - zlim.dpt[1] + 1]
#         }else if(raw.titles[d] == "RH_Percent"){col <- colorlut.rh[ z - zlim.rh[1] + 1] 
#         }else if(raw.titles[d] == "VPD"){col <- colorlut.vp[ z - zlim.vpd[1] + 1]
#         }else if(raw.titles[d] == "ppt_mm"){
#           z <- 2 * ext.z * treatment.array[,,d]
#           col <- colorlut.pt[ z - zlim.ppt[1] + 1]
#         }else if(raw.titles[d] == "ppt_mm_sum"){col <- colorlut.pt.s[ z - zlim.ppt.sum[1] + 1]
#         }else if(raw.titles[d] == "Soil4in_C"){col <- colorlut[ z - zlim[1] + 1]
#         }else if(raw.titles[d] == "Soil8in_C"){col <- colorlut[ z - zlim[1] + 1]
#         }else if(raw.titles[d] == "Solar_WM2"){col <- colorlut.s[ z - zlim.s[1] + 1]
#         }else if(raw.titles[d] == "ws2m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]
#         }else if(raw.titles[d] == "ws10m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]}
#       }
# ##############################################################################
# if(length(l.choices) > 1){
# x.vector[,,d] <- as.numeric((x - x.offset))
# z.1 <- ext.z * treatment.array.2[,,d]
# ##############################################################################
#         if(raw.titles[d] == "AT"){col.1 <- colorlut[ z.1 - zlim[1] + 1]
#         }else if(raw.titles[d] == "Dew_Point"){col.1 <- colorlut.dp[ z.1 - zlim.dpt[1] + 1]
#         }else if(raw.titles[d] == "RH_Percent"){col.1 <- colorlut.rh[ z.1 - zlim.rh[1] + 1] 
#         }else if(raw.titles[d] == "VPD"){col.1 <- colorlut.vp[ z.1 - zlim.vpd[1] + 1]
#         }else if(raw.titles[d] == "ppt_mm"){
#           z.1 <- 2 * ext.z * treatment.array.2[,,d]
#           col.1 <- colorlut.pt[ z.1 - zlim.ppt[1] + 1]
#         }else if(raw.titles[d] == "ppt_mm_sum"){col.1 <- colorlut.pt.s[ z.1 - zlim.ppt.sum[1] + 1]
#         }else if(raw.titles[d] == "Soil4in_C"){col.1 <- colorlut[ z.1 - zlim[1] + 1]
#         }else if(raw.titles[d] == "Soil8in_C"){col.1 <- colorlut[ z.1 - zlim[1] + 1]
#         }else if(raw.titles[d] == "Solar_WM2"){col.1 <- colorlut.s[ z.1 - zlim.s[1] + 1]
#         }else if(raw.titles[d] == "ws2m_ms"){col.1 <- colorlut.ws[ z.1 - zlim.ws[1] + 1]
#         }else if(raw.titles[d] == "ws10m_ms"){col.1 <- colorlut.ws[ z.1 - zlim.ws[1] + 1]}
# }
# ##############################################################################
# if(length(l.choices) > 2){
# x.vector[,,d] <- as.numeric((x - x.offset))
# z.2 <- ext.z * treatment.array.2[,,d]
# ##############################################################################
# if(raw.titles[d] == "AT"){col.2 <- colorlut[ z.2 - zlim[1] + 1]
#           }else if(raw.titles[d] == "Dew_Point"){col.2 <- colorlut.dp[ z.2 - zlim.dpt[1] + 1]
#           }else if(raw.titles[d] == "RH_Percent"){col.2 <- colorlut.rh[ z.2 - zlim.rh[1] + 1] 
#           }else if(raw.titles[d] == "VPD"){col.2 <- colorlut.vp[ z.2 - zlim.vpd[1] + 1]
#           }else if(raw.titles[d] == "ppt_mm"){
#             z.2 <- 2 * ext.z * treatment.array.2[,,d]
#             col.2 <- colorlut.pt[ z.2 - zlim.ppt[1] + 1]
#           }else if(raw.titles[d] == "ppt_mm_sum"){col.2 <- colorlut.pt.s[ z.2 - zlim.ppt.sum[1] + 1]
#           }else if(raw.titles[d] == "Soil4in_C"){col.2 <- colorlut[ z.2 - zlim[1] + 1]
#           }else if(raw.titles[d] == "Soil8in_C"){col.2 <- colorlut[ z.2 - zlim[1] + 1]
#           }else if(raw.titles[d] == "Solar_WM2"){col.2 <- colorlut.s[ z.2 - zlim.s[1] + 1]
#           }else if(raw.titles[d] == "ws2m_ms"){col.2 <- colorlut.ws[ z.2 - zlim.ws[1] + 1]
#           }else if(raw.titles[d] == "ws10m_ms"){col.2 <- colorlut.ws[ z.2 - zlim.ws[1] + 1]}
# }
#           ##############################################################################
#         #if(length(l.choices) > 0 & c == 1){
# if(length(l.choices) > 0){
#   rgl.surface(x.vector[,,d], y, z, color=col, alpha=1, front="fill", back="fill")
#   y.offset.2 <- y.offset.2 + 1
#   x.offset <- x.offset + (ext.x * side.heigth.offset)
# }
# if(length(l.choices) > 1){
#   rgl.surface(x.vector[,,d], y, z.1, color=col.1, alpha=1, front="fill", back="fill")
#   y.offset.2 <- y.offset.2 + 1
#   x.offset <- x.offset + (ext.x * side.heigth.offset)
# }
# if(length(l.choices) > 2){
#   rgl.surface(x.vector[,,d], y, z.2, color=col.2, alpha=1, front="fill", back="fill")
#   y.offset.2 <- y.offset.2 + 1
#   x.offset <- x.offset + (ext.x * side.heigth.offset)
# }
# #           
# # y.offset.2 <- y.offset.2 + 1
# # x.offset <- x.offset + (ext.x * side.heigth.offset)
#        # }}
#       ##############################################################################
# #       if(length(l.choices) > 1 & c == 2){
# #         x.vector[,,d] <- as.numeric((x - x.offset))
# #         z <- ext.z * treatment.array.1[,,d]
# #         ##############################################################################
# #         if(raw.titles[d] == "AT"){col <- colorlut[ z - zlim[1] + 1]
# #         }else if(raw.titles[d] == "Dew_Point"){col <- colorlut.dp[ z - zlim.dpt[1] + 1]
# #         }else if(raw.titles[d] == "RH_Percent"){col <- colorlut.rh[ z - zlim.rh[1] + 1] 
# #         }else if(raw.titles[d] == "VPD"){col <- colorlut.vp[ z - zlim.vpd[1] + 1]
# #         }else if(raw.titles[d] == "ppt_mm"){
# #           z <- 2 * ext.z * treatment.array.1[,,d]
# #           col <- colorlut.pt[ z - zlim.ppt[1] + 1]
# #         }else if(raw.titles[d] == "ppt_mm_sum"){col <- colorlut.pt.s[ z - zlim.ppt.sum[1] + 1]
# #         }else if(raw.titles[d] == "Soil4in_C"){col <- colorlut[ z - zlim[1] + 1]
# #         }else if(raw.titles[d] == "Soil8in_C"){col <- colorlut[ z - zlim[1] + 1]
# #         }else if(raw.titles[d] == "Solar_WM2"){col <- colorlut.s[ z - zlim.s[1] + 1]
# #         }else if(raw.titles[d] == "ws2m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]
# #         }else if(raw.titles[d] == "ws10m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]}
# #         ##############################################################################
# #         rgl.surface(x.vector[,,d], y, z , color=col, alpha=1, front="fill", back="fill")
# #         y.offset.2 <- y.offset.2 + 1
# #         x.offset <- x.offset + (ext.x * side.heigth.offset)
# #       }
# # #       ##############################################################################
# # #       if(length(l.choices) > 2 &  c == 3){
# # #         x.vector[,,d] <- as.numeric((x - x.offset))
# # #         z <- ext.z * treatment.array.2[,,d]
# # #         ##############################################################################
# # #         if(raw.titles[d] == "AT"){col <- colorlut[ z - zlim[1] + 1]
# # #         }else if(raw.titles[d] == "Dew_Point"){col <- colorlut.dp[ z - zlim.dpt[1] + 1]
# # #         }else if(raw.titles[d] == "RH_Percent"){col <- colorlut.rh[ z - zlim.rh[1] + 1] 
# # #         }else if(raw.titles[d] == "VPD"){col <- colorlut.vp[ z - zlim.vpd[1] + 1]
# # #         }else if(raw.titles[d] == "ppt_mm"){
# # #           z <- 2 * ext.z * treatment.array.2[,,d]
# # #           col <- colorlut.pt[ z - zlim.ppt[1] + 1]
# # #         }else if(raw.titles[d] == "ppt_mm_sum"){col <- colorlut.pt.s[ z - zlim.ppt.sum[1] + 1]
# # #         }else if(raw.titles[d] == "Soil4in_C"){col <- colorlut[ z - zlim[1] + 1]
# # #         }else if(raw.titles[d] == "Soil8in_C"){col <- colorlut[ z - zlim[1] + 1]
# # #         }else if(raw.titles[d] == "Solar_WM2"){col <- colorlut.s[ z - zlim.s[1] + 1]
# # #         }else if(raw.titles[d] == "ws2m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]
# # #         }else if(raw.titles[d] == "ws10m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]}
# # #         ##############################################################################
# #         rgl.surface(x.vector[,,d], y, z, color=col, alpha=1, front="fill", back="fill")
# #         y.offset.2 <- y.offset.2 + 1
# #         x.offset <- x.offset + (ext.x * side.heigth.offset)
# #       }
#       ##############################################################################
#       
#       # if(label.marker == 1)text3d(color = "black", x = (((96/2) * -1) - ((d-1)*side.heigth.offset))*ext.x,
#       #                                                   y = 0, z = z.title.offset, text = raw.titles[d])
#       # if((label.marker >= 1) & (is.na(raw.yr.titles[d]) == FALSE))
#       # {
#       #   text3d(color = "black", x = 200,y = 0,z = ((182*z.yr.title.offset.2) + ((z.yr.title.offset.1-1)*width.offset))*ext.y,
#       #                                           text = raw.yr.titles[d])
#       #   z.yr.title.offset.1 <- z.yr.title.offset.1 + 1
#       #   z.yr.title.offset.2 <- z.yr.title.offset.2 + 2
#       # }
#       
#       # y.offset.2 <- y.offset.2 + 1
#       # x.offset <- x.offset + (ext.x * side.heigth.offset)
#       
#       if(y.offset == y.offset.2) 
#       {
#         y <- y + (ext.y * treat.range) + width.offset
#         #z <- ext.z * treatment.array[,,d]
#         x.offset <- as.numeric(0)
#         y.offset.2 <- as.numeric(0)
#         if(label.marker != 0)label.marker <- as.numeric(2)
#       }   
#     }#End of for loop of rgl scene
#   }#End of l.choices
# }
##############################################################################
#Stacked View
stack.rgl.3d <- function(num.raw.col,x,y,z,ext.x,ext.y,ext.z,y.offset,y.offset.2,z.vector,treatment.array,
                         colorlut,zlim,treat.range,z.offset,offset.stack,session,width.offset,
                         raw.titles,label.marker,colorlut.s,zlim.s,colorlut.dp,zlim.dpt,colorlut.rh,zlim.rh,
                         colorlut.pt,zlim.ppt,colorlut.ws,zlim.ws,colorlut.vp,zlim.vpd,raw.yr.titles) 
{
  
  
  z.yr.title.offset.1 <- as.numeric(1)#seq(1,length(raw.yr.titles))
  z.yr.title.offset.2 <- as.numeric(1)#seq(1,length(raw.yr.titles))
  
  progress <- shiny::Progress$new(session, min=1,max=num.raw.col)
  on.exit(progress$close())
  progress$set(message = 'Loading 3D plots now...')
  
  for(d in 1:length(raw.titles))#for(d in 1:num.raw.col)
  {
    #show("Plotting: Plot #")
    progress$set(value = d)
    z <- ext.z * treatment.array[,,d]
    z.vector[,,d] <- as.numeric(z + z.offset)
    
    #####Apply offset to each surface#######   
    if(raw.titles[d] == "AT"){col <- colorlut[ z - zlim[1] + 1]
    }else if(raw.titles[d] == "Dew_Point"){col <- colorlut.dp[ z - zlim.dpt[1] + 1]
    }else if(raw.titles[d] == "RH_Percent"){col <- colorlut.rh[ z - zlim.rh[1] + 1]
    }else if(raw.titles[d] == "VPD"){col <- colorlut.vp[ z - zlim.vpd[1] + 1]
    }else if(raw.titles[d] == "ppt_mm"){
      z <- 2 * ext.z * treatment.array[,,d]
      col <- colorlut.pt[ z - zlim.ppt[1] + 1]
    }else if(raw.titles[d] == "Soil4in_C"){col <- colorlut[ z - zlim[1] + 1]
    }else if(raw.titles[d] == "Soil8in_C"){col <- colorlut[ z - zlim[1] + 1]
    }else if(raw.titles[d] == "Solar_WM2"){col <- colorlut.s[ z - zlim.s[1] + 1]
    }else if(raw.titles[d] == "ws2m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]
    }else if(raw.titles[d] == "ws10m_ms"){col <- colorlut.ws[ z - zlim.ws[1] + 1]}
    
    rgl.surface(x, y, z.vector[,,d], color=col, alpha=1, front="fill", back="fill")
    
    if(label.marker == 1)text3d(color = "black", x = 50 * ext.x, y = (offset.stack * (d-1) * -1)* ext.y,
                                z = -200* ext.z, text = raw.titles[d])
    
    if((label.marker >= 1) & (is.na(raw.yr.titles[d]) == FALSE))
    {
      text3d(color = "black", x = (48 * ext.x), y = (200 * ext.y),
             z = ((182*z.yr.title.offset.2) + 
                    ((z.yr.title.offset.1-1)*width.offset))*ext.z,
             text = raw.yr.titles[d])
      z.yr.title.offset.1 <- z.yr.title.offset.1 + 1
      z.yr.title.offset.2 <- z.yr.title.offset.2 + 2
    }
    y.offset.2 <- y.offset.2 + 1
    z.offset <- z.offset - (ext.z * offset.stack)
    
    if(y.offset == y.offset.2) 
    {
      y <- y + (ext.y * treat.range) + width.offset#100
      z <- ext.z * treatment.array[,,d]
      z.offset <- as.numeric(0)
      y.offset.2 <- as.numeric(0)
      if(label.marker != 0)label.marker <- as.numeric(2)
    }     
  }#End of for loop of rgl scene 
}
##############################################################################
side.rgl.3d.plot <- cmpfun(side.rgl.3d)
stack.rgl.3d.plot <- cmpfun(stack.rgl.3d)
##############################################################################
create.treatment.array.cust <- function(doy.range,dap.seq,treatment.col1,num.raw.col,treat.range,
                                        num.treatments,rawdata,temp.marker,y.temp,midnight,
                                        mask.row,tod.start,tod.end,num.meas.days,raw.titles)
{
treatment.array <- array(dim = c(num.meas.days, treat.range, ncol(rawdata)))

if(temp.marker == 1)#DOY VIEW
{
for(t in 1:ncol(rawdata))
{
temp.start <- midnight[treatment.col1]
temp.end <- temp.start + (num.meas.days - 1)
for(j in 1:treat.range)
{
treatment.array[,j,t] <- cbind(rawdata[temp.start:temp.end, y.temp])
temp.start <- temp.end + 1
temp.end <- temp.start + (num.meas.days-1)
}
y.temp <- y.temp + 1
}
 }#End of DOY view
    
return(treatment.array)
}


