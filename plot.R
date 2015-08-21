
# used to create PDFs
library("Cairo");
 
DEFAULT_PDF_WIDTH <- 12; # 
DEFAULT_PDF_HEIGHT <- 6; # 


# plot default values

DEFAULT_PLOT_X_LAB <- get_translation("date");
DEFAULT_PLOT_Y_LAB <- get_translation("dist_km");

# histogram default values
HIST_CLASSES <- 100; # display 100 different classes in histograms

HIST_DEFAULT_MAX_XLIM <- 0.1; # display only up to 100 meters

HIST_DEFAULT_CLASS_WIDTH <- HIST_DEFAULT_MAX_XLIM / HIST_CLASSES;

HIST_DEFAULT_XLIMS <- c(0, HIST_DEFAULT_MAX_XLIM);


#hist_big_max is used so that all values are included in the breaks
hist_big_max <- 2; # 2 kms should be enough to include all distances!
HIST_DEFAULT_BREAKS <- seq ( 0, hist_big_max, HIST_DEFAULT_CLASS_WIDTH );


DEFAULT_HIST_Y_LAB <- get_translation("freq");
DEFAULT_HIST_X_LAB <- get_translation("dist_km");

  
  #########################################
  #
  # explanations for the graphs:
  # 5 functions are used: jpeg(), par(), plot(), axis() and dev.off()
  #
  
  # jpeg() function
  # ==============
  # opens an image file in the png format to draw in it
  # the "cairo" type looks like the best, so it is used here =)
  # the "bg" argument (here "white) specifies the color of the background of the image.
  # 
  
  # par() function
  # ===============
  # par(mfrow=c(x,y)) tells to create x*y graphs in x rows and y columns
  # par(mfrow=c(2,1)) creates 2 graphs in 2 rows and 1 column
  #
  
  # plot() function
  # =============
  # actually draws the graph
  # the 2 first arguments are the x and y values in vectors
  # type "l" creates lines, type "o" is the default and creates circles at every point
  # xlab, ylab specify the name of the axis (x and y labels)
  # main gives the main title of the graph
  # xaxt="n" tells not to create the x axis, as we want to customize it!
  #
  
  # axis() function
  # =============
  # creates an axis
  # the first argument (here "1") specifies which axis to draw (here bottom)
  # the values are the text values for the dates, instead of the timestamps
  # padj tells to move the values a little down.
  #
  
  # dev.off() function
  # =================
  # closes the png file, otherwise the graph would be replaced with the next plot() function call
  # This is absolutely needed!!!
  #
  

# given a time in hours (decimal), gives a nicely formatted string to display, in the form H:M:S
pretty_time <- function(t_in)
{
  h <- trunc(t_in);
  m <- trunc (60 * (t_in - h));
  s <- trunc (3600 * (t_in - m / 60 - h));
  return (paste(h,':',m,':',s,sep=""));
}



# adds a title to the currently written plot
add_title_y <- function(title, subtitle, dy, y = 0.03, fig=c(0.49,0.51,0.98,1))
{
  par(xpd=NA);
  par(fig=fig);
  par(font=2);
  text(x=c(0),y=c(y), labels=c(title), cex=1.5);
  par(font=1);
  text(x=c(0),y=c(dy), labels=c(subtitle), cex=1.25);
}
# adds a title to the currently written plot
add_title <- function(title, subtitle, dy=-0.08)
{
  add_title_y(title, subtitle, dy);
}

internal_pdfFolder__ <- "";
internal_pdfName__ <- "";
internal_inPDF__ <- FALSE;

checkInPDF <- function()
{
  if ( ! internal_inPDF__ )
    stop("You must first call startPDF!");
}
checkUseDataLabels <- function(useDateLabels)
{
  if (useDateLabels &&   
       !exists("axis_dates") )
          stop("axis dates are not defined!!");
  if (useDateLabels &&   
       !exists("axis_labels") )
          stop("axis labels are not defined!!");
}

setPDFFolder <- function ( folder )
{
  if (folder == "")
    stop ( "No folder provided, first call setPDFFolder()" );

  internal_pdfFolder__ <<- folder;
}

startPDF <- function
  (
    name,
    w=DEFAULT_PDF_WIDTH,
    h=DEFAULT_PDF_HEIGHT,
    mfrow=NULL # if multiple graphs on same page, format: c(nrows, ncols)
  )
{
    if (name == "")
      stop ( "No file name provided to startPDF()" );
    if ( internal_inPDF__ )
      stop ( "PDF already active, first call endPDF()");

    internal_pdfName__ <<- name;

    filepath <- paste(internal_pdfFolder__, internal_pdfName__, ".pdf", sep="");

    writeLines(paste("Starting PDF: ", filepath));
    CairoPDF ( filepath, width=w, height=h );
    internal_inPDF__ <<- TRUE;

    if ( ! is.null (mfrow))
      par (mfrow=mfrow);
}

endPDF <- function ()
{
    internal_inPDF__ <<- FALSE;
    internal_pdfName__ <<- "";
    dev.off();
    writeLines("Ending PDF");
}

makePlot <- function
  (
    x,
    y,
    name_transl_key,
    name_transl_args=c(),
    main_transl_key=name_transl_key,
    main_transl_args=name_transl_args,
    pdf_width=DEFAULT_PDF_WIDTH,
    xlab=DEFAULT_PLOT_X_LAB,
    ylab=DEFAULT_PLOT_Y_LAB,
    useDateLabels=TRUE,
    type="l",
    ylim=NULL,
    col="black",
    extraFn=NULL # we will call this function if not null
  )
{
  filename <- get_trans_filename(name_transl_key, name_transl_args);
  startPDF ( filename, pdf_width );

  justPlot( x=x, y=y, main_transl_key=main_transl_key,
            main_transl_args=main_transl_args, pdf_width=pdf_width,
            xlab=xlab, ylab=ylab, useDateLabels=useDateLabels,
            type=type, ylim=ylim, col=col, extraFn=extraFn );

  endPDF();

  echo <- paste("Graph file",filename,"created");
  writeLines(echo);
}

justPlot <- function
  (
    x,
    y,
    main_transl_key,
    main_transl_args=c(),
    main_direct=NULL,
    sub=NULL,
    pdf_width=DEFAULT_PDF_WIDTH,
    xlab=DEFAULT_PLOT_X_LAB,
    ylab=DEFAULT_PLOT_Y_LAB,
    useDateLabels=TRUE,
    type="l",
    ylim=NULL,
    col="black",
    extraFn=NULL, # we will call this function if not null
    xaxt="s",
    custom_datetime_labels=NULL,
    custom_datetime_labels_at=NULL
  )
{
  checkInPDF();

#special only in justPlot, for the pages with several dogs
  useCustomDateLabels <- FALSE;
  if ( is.null(custom_datetime_labels) & is.null(custom_datetime_labels_at))
    checkUseDataLabels(useDateLabels)
  else
  {
    if ( is.null(custom_datetime_labels) )
      stop("cannot have null date-time labels if date-time at is specified");
    if ( is.null(custom_datetime_labels_at) )
      stop("cannot have null date-time at if date-time labels are specified");
    useDateLabels = TRUE; # make sure we have it on =)
    useCustomDateLabels <- TRUE;
  }
  
  xaxt <- if (useDateLabels) "n" else xaxt; # suppress axis if we use our own labels

  main <- if ( ! is.null(main_direct) ) main_direct
          else if (main_transl_key=="") NULL
          else get_translation(main_transl_key, main_transl_args);
  plot ( x, y, main=main, sub=sub, xlab=xlab, ylab=ylab, xaxt=xaxt, type=type, ylim=ylim, col=col );

  if (useDateLabels)
  {
    at_ = if (useCustomDateLabels) custom_datetime_labels_at else axis_labels;
    ls_ = if (useCustomDateLabels) custom_datetime_labels else axis_dates;
    axis(1, at=at_, labels=ls_, padj=0.5);
  }
  
  if (! is.null (extraFn) )
    extraFn();

  echo <- paste("Graph ",main,"created");
}

makeHist <- function
  (
    x ,
    name_transl_key,
    name_transl_args=c(),
    main_transl_key=name_transl_key,
    main_transl_args=name_transl_args,
    xlab=DEFAULT_HIST_X_LAB,
    ylab=DEFAULT_HIST_Y_LAB,
    showMedian=TRUE,
    useDefaultBreaks=TRUE, # false means we compute the breaks from the median
    extraFn=NULL # we will call this function if not null
  )
{
  filename <- get_trans_filename(name_transl_key, name_transl_args);
  startPDF(name=filename);

  justHist ( x, main_transl_key=main_transl_key, main_transl_args=main_transl_args, xlab=xlab, ylab=ylab, showMedian=showMedian, useDefaultBreaks=useDefaultBreaks, extraFn );

  endPDF();

  echo <- paste("Graph file",filename,"created");
  writeLines(echo);
}

justHist <- function
  (
    x ,
    main_transl_key,
    main_transl_args=c(),
    main_direct=NULL,
    sub=NULL,
    xlab=DEFAULT_HIST_X_LAB,
    ylab=DEFAULT_HIST_Y_LAB,
    showMedian=TRUE,
    useDefaultBreaks=TRUE, # false means we compute the breaks from the median
    extraFn=NULL, # we will call this function if not null
    ylim=NULL
  )
{
  if (showMedian)
    med <- median (x);

  if (showMedian)
  {
     tr <- get_translation("red_median", c(signif(med, digits=4)))
     if ( is.null(sub) ) sub <- tr
     else sub <- paste(sub, tr, sep="\n");
  }

  if (useDefaultBreaks)
  {
    xlim <- HIST_DEFAULT_MAX_XLIM;
    breaks <- HIST_DEFAULT_BREAKS;
  }
  else
  {
    xlim <- 8*med;
    breaks <- seq(0, xlim, xlim / HIST_CLASSES);
    x <- Filter( function(i)i<=xlim, x);
  }

  main <- if ( ! is.null(main_direct) ) main_direct
          else if (main_transl_key=="") NULL
          else get_translation(main_transl_key, main_transl_args);
  hist (x, breaks=breaks, xlab=xlab, ylab=ylab, main=main, sub=sub);

  if (showMedian)
    abline ( v=med, col="red", lwd=2 );

  if (! is.null (extraFn) )
    extraFn();

  echo <- paste("Graph ",main,"created");
}



