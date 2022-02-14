theme_Himalaya <- function( base_size = 11, base_family = "") {

  theme_bw() %+replace% 

    theme( 
      plot.title = element_text(color = "slateblue4", size = 16, face = "bold" ,
                                hjust = 0.5 , vjust = 2 , family = "sans") ,
           axis.text = element_text( size = 12 , family = "sans") ,
           axis.title = element_text( size = 16 , family = "sans") ,
           legend.title = element_text( size = 16 ) ,
           legend.text = element_text( size = 12 )
      )
}
