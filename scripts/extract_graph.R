suppressPackageStartupMessages(require(made4))
suppressPackageStartupMessages(require(impute))
suppressPackageStartupMessages(require(repr))
suppressPackageStartupMessages(require(optparse))
 
check_variable <- function(long_name, optional=FALSE) {
    if (is.null(opt[[long_name]])) {
        if (optional) {
            cat ("No value provided for ")
            cat (long_name)
            cat ("\n")
        } else {        
            print_help(option_parser)
            stop("parameter not provided")
        }
    } else {
        if (opt$verbose) {
            cat (long_name)
            cat (" = ")
            cat (opt[[long_name]])
            cat ("\n")
        }
    }
}

check_column <- function(data, column_name) {
    if (!is.null(column_name)){
        if(column_name %in% colnames(data)){
            if (opt$verbose) {
                cat (column_name)
                cat (" found in the data!\n")
            } 
        } else {
            error_message = paste0(c("No Colomn",column_name, "found in the data!"), collapse = " ")
            stop(error_message)
        }
    }
}

extract_values <- function(data, value, column_names, row_names){
    if (is.null(row_names)){
        return (extract_values_simple(data, value, column_names))
    }
    tryCatch({
        extract_values_with_row_names(data, value, column_names, row_names) 
    }, error = function(err) {
        print(paste("Error extracting values with row names:  ",err))
        print("Extracting values without row names")
        return (extract_values_simple(data, value, column_names))
    }) # END tryCatch
}

extract_values_with_row_names <- function(data, value, column_names, row_names){
    data_list <- split(data , f = data[row_names] )
    mysummary("data_list", data_list)

    if (all(unlist(lapply(data_list, function(x){length(unique(x[[opt$column_names]])) == length(x[[column_names]])})))){
        two_list = lapply(data_list, function(x){subset(x, select=c(value, column_names))})
        for (i in 1:length(two_list)) {
            colnames(two_list[[i]])[colnames(two_list[[i]]) == value] <- names(two_list)[[i]]
        }
        mysummary("two_list", two_list)

        merged_list = Reduce(function(...) merge(..., all=T), two_list)
        rownames(merged_list) <- merged_list[,column_names]
        merged_list[column_names] <- NULL
        mysummary("merged_list", merged_list)
        return (as.matrix(merged_list))
    } else {
        stop("Duplicates vaues where found for values with row and column name")
    }   
}

extract_values_simple <- function(data, value, column_names){
    data_list <- split(data , f = data[column_names] )
    merged_list = matrix(unlist(lapply(data_list, function(y)y[,value])), ncol=length(data_list), byrow=TRUE)
    colnames(merged_list) <- names(data_list)
    return (merged_list)    
}

mysize <- function(data) {
    dim_data = dim(data)
    if (!is.null(dim_data)){
        return (toString(dim_data))
    }
    if (length(data) == 1){
        return ("1")
    }
    the_size = mysize(data[[1]])
    #print (the_size)
    for (i in 2:length(data)) {
        new_size = mysize(data[[i]])
        #print (new_size)
        if (the_size !=  new_size){
        return(paste (paste(length(data),",", sep=""), "various", sep = " "))
        }
    }
    if (the_size == "1"){
        return (length(data))
    } else {
        return(paste (paste(length(data),",", sep=""), the_size, sep = " "))
    }
}

mysummary <- function(long_name, data) {
    if (opt$verbose) {
        cat (long_name)
        cat (": class = ")
        cat (class(data))
        cat (": type = ")
        cat (typeof(data))
        cat (", size = ")
        cat (mysize(data))
        cat ("\n")
    }
}

mymessages <- function(mess_array){
    if (opt$verbose) {
        cat(paste(mess_array, sep = " ", collapse = NULL))
        cat ("\n")
    }
}

option_list = list(
  make_option("--csv_file", action="store", type='character',
              help="csf file to read data from"),
  make_option("--value", action="store", type='character', default="Value",
              help="Name of column which holds the values to extract and plot to the graph"),
  make_option("--column_names", action="store", type='character', default="CellName",
              help="Name of column which holds the data to be used as column names in the extracted data and X axis of the graph"),
  make_option("--row_names", action="store", type='character', default="GeneName",
              help="Name of column which holds the names to be used as rows in the extracted data"),
  make_option("--filter", action="store", type='character', default=NULL,
              help="One or more filter to apply to the data"),
  make_option("--na_value", action="store", type='character', default=NULL,
              help="Value that should be considered NA"),
  make_option("--data_output_file", action="store", type='character', 
              help="File to write reduced data to (csv fromat). if not provided no data will be putput."),
  make_option("--output_na_value", action="store", type='character', default=NULL,
              help="Value that should be used for any NA in the output file. If not provided the empty string is used."),
  make_option("--graph_file", action="store", type='character', 
              help="File to write graph to. If not provided no graph is output"),
  make_option("--graph_height", action="store", type='double', default="10",
              help="Height in inches of graph"),
  make_option("--graph_width", action="store", type='double', default="10",
              help="Width in inches of graph"),
  make_option("--graph_title", action="store", type='character',
              help="Title to give the graph. If no title is provided one will be created automatically."),
  make_option("--graph_red_line_value", action="store", type='double', 
              help="Value at which to draw a horizonatal red line. If not provided no line is draw"),

  make_option(c("--verbose"), action="store_true", default=FALSE,
              help="Should the program print extra stuff out? [default %default]")
)

option_parser = OptionParser(option_list=option_list)
opt = parse_args(option_parser)

check_variable("csv_file")
check_variable("value")
check_variable("row_names")
check_variable("column_names", optional=TRUE)
check_variable("filter", optional=TRUE)
check_variable("na_value", optional=TRUE)
check_variable("data_output_file", optional=TRUE)
if (!is.null(opt$data_output_file)){
    if (is.null(opt$output_na_value)){
        opt$output_na_value = ""
    }
    check_variable("output_na_value")
}
check_variable("graph_file", optional=TRUE)
if (!is.null(opt$graph_file)){
    check_variable("graph_height")
    check_variable("graph_width")
    check_variable("graph_red_line_value", optional=TRUE)
    if (is.null(opt$graph_title)) {
        opt$graph_title <- paste(opt$value,"by",opt$row_name)
    }
    check_variable("graph_title")
}

# Reading data from CSV file
data <- read.csv(opt$csv_file,check.names=FALSE,row.names=1)
mysummary("data read", data)

check_column(data, opt$value)
check_column(data, opt$row_names)
check_column(data, opt$column_names)

if (!is.null(opt$filter)){
    for (a_filter in strsplit(opt$filter, ",")[[1]]){
        print (a_filter)
        my_filter = parse(text=a_filter)
        old_length = nrow(data)
        data = subset(data, eval(my_filter))
        new_length = nrow(data)
        if (new_length == old_length) {
            mymessages(c("applying filter",a_filter,"had no effect"))
        } else if (new_length == 0){
            error_message = paste(a_filter,"has removed all the data", sep = " ")
            stop(error_message)
        } else {
            mymessages(c("applying filter",a_filter,"has reduced the rows from", old_length,"to",new_length))
        }
    }
}

mysummary("data after filter", data)

merged_list = extract_values(data, opt$value, opt$column_names, opt$row_names)

mysummary("merged_list", merged_list)

clean_list <- merged_list
if (! is.null(opt$na_value)){
    clean_list[clean_list == opt$na_value] <- NA
    mymessages(c("replacing",opt$na_value,"with NA"))
}
mysummary("clean_list", clean_list)

if (is.null(opt$data_output_file)){
    mymessages(c("No data output as data_output_file parameter not provided"))
} else {
    write.csv(clean_list, file = opt$data_output_file, row.names=TRUE, na=opt$output_na_value)
    mymessages(c("Data output to", opt$data_output_file,"in csv format"))
}

if (is.null(opt$graph_file)){
    mymessages(c("No graph plotted as graph_file parameter not provided"))
} else {
    postscript(opt$graph_file, horizontal=F, width=opt$graph_width, height=opt$graph_height, paper="special", onefile=FALSE)
    old.par <- par(mfrow=c(2, 1))
    par(cex.axis=0.55)
    boxplot(clean_list,ylab="Ct Value", names=colnames(clean_list), las=2,font.axis=0.5)
    title(opt$graph_title)
    if (is.null(opt$graph_red_line_value)){
        mymessages(c("No red line as graph_red_line_value parameter not provided"))  
    } else {
        abline(h=opt$graph_red_line_value, col="red", lwd=3)
    }
    done = dev.off()
    mymessages(c("Plotted graph to",opt$graph_file))
}


