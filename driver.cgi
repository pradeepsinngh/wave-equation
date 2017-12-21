#!/usr/bin/env perl
#
# File name: driver.cgi
# Date: 12/18/2017
# Author: Pradeep Singh
# Description: The program "driver.cgi" run twave for various time values, generating a sequence of
#              snapshots in gnuplot format and then runs gnuplot on each of these snapshots to generate
#              an png format image for corresponding snapshots. Finally, it runs the convert command
#              to make a gif of all images.


#To make html text compatiable
print "Content-type: text/html; charset=iso-8859-1", "\n\n";

#parse the string and extrct useful parameters
%query = &cgiparse();

#If not enough inputs, terminate is 1
$terminate = 0;

#Read key and their values
$funchoice = $query{"command"};	#user's function choice

$funchoice = "gaussian";
$L = 6;
$alpha = 1;
$beta = 2;
$sigma = 5;
$delta = 1;

#If blank form is submitted
if(!$funchoice){
  $funchoice = "No function selected!";
  $funcomment = "Please select either sine, gaussian, or cubic functions.\n";
  $funparam = "Discarded parameters (if provided any)!";
  $terminate = 1;	#Prompt user for function
}
#Check parameters
else{

  #Parameters for Sine function
  if($funchoice eq "sine"){
    print("Inside sine extraction! <br><br>");
#    $L = $query{"sineL"};	#Extract string length
    $funparam = "L = $L";	#Store it for results
    $cmdargs = "$L";	#To be passed to twave.F90
    $extend = 0;	#Part of compiling twave
    $xrange = "0.0:$L";	#xlimit for the sine graph
    $yrange = "-1.0:1.0";	#ylimit for the sine graph    
  
    #Check parametrs
    if($L <= 0 || $L > 10){
      $funcomment = "The value of L (string length) should be in range of 0 and 10.<br>";	#Reason for the exit
      $terminate = 1;
    }
  }

  #Parameters for Gaussian function
  elsif($funchoice eq "gaussian"){
    print("Inside gaussian extraction! <br><br>");
    $extend = 1;	#Part of compiling twave

#    $L = $query{"gaussianL"};	#Extract string length
    $funparam = "L = $L, ";	#Store it for results
    $cmdargs = "$L ";	#To be passed to twave.F90

    #Check parameters
    if($L <= 0 || $L > 10){
      $funcomment = "The value of L (string length) should be in range of 0 and 10.<br>";	#Reason for the exit
      $terminate = 1;
    }

#    $alpha = $query{"paramAlpha"};	#Extract alpha parameter
    $funparam .= "Alpha = $alpha, ";	#Store it for results
    $cmdargs .= "$alpha ";	#To be passed to twave.F90

#    $beta = $query{"paramBeta"};	#Extract beta parameter
    $funparam .= "Beta = $beta, ";	#Store it for results
    $cmdargs .= "$beta ";	#To be passed to twave.F90
    if($beta <= 0 || $beta > $L){
      #reason for the exit
      $funcomment .= "The value of beta should be in range of 0 and L (string length), where L is less than 10.<br>";
      $terminate = 1;
    }

#    $sigma = $query{"paramSigma"};	#Extract sigma parameter
    $funparam .= "and Sigma = $sigma";	#Store it for results
    $cmdargs .= "$sigma";	#To be passed to twave.F90
    if($sigma <= 0){
      $funcomment .= "The value of sigma should be greater than 0.<br>";	#Reason for the exit
      $termiante = 1;
    }
    $xrange = "0.0:$L";	#xlimit for the gaussian graph
    $yrange = "-2*$alpha:2*$alpha";	#ylimit for the gaussian graph
  }

  #parameters for Cubic function
  elsif($funchoice eq "cubic"){
    print("Inside cubic exxtraction!<br>");
    $extend = 1;	#Part of compiling twave

#    $L = $query{"cubicL"};	#Extract string length
    $funparam .= "L = $L, ";	#Store it for results
    $cmdargs = "$L ";	#To be passed to twave.F90
    if($L <= 0 || $L > 10){
      $funcomment = "The value of L (string length) should be in range of 0 and L.<br>";	#Reason for the exit
      $terminate = 1;
    }

#    $delta = $query{"paramDelta"};	#Extract delta parameter
    $funparam .= "and Delta = $delta";	#Store it for results
    $cmdargs .= "$delta";	#To be passed to twave.F90
    if($delta <= 0 || $delta > $L){
      #Reason for the exit
      $funcomment .= "The value of delta should be in range of 0 and L (string length), where L is less than 10.<br>";
      $terminate = 1;
    }
    $xrange = "0.0:$L";	#xlimit for the cubic graph
    $yrange = "-60:50";	#ylimit for the cubic graph
  }

  #We should not encounter this condition unless someone has introduced new functions in index.html!
  else{
    print("Something is wrong with the parameters!");
    $terminate = 1;
  }	
}

#If parameters are not correct, tell user to correct them and exit
if($terminate == 1){
  #do substitution and show results
  substitute($funchoice, $funparam, "Suggestions! <br>" . $funcomment, "");
  exit(0);	#Exit the program
}

#If here, all parameters are correct and extracted
#Remove all previous png files stored and wave.gif file.
#Assuming that if .gif file exists then .png files also exist
#at their supposed location.
if (-e "results/png_images/wave.gif"){
	system("rm ./results/wave.gif");
	system("rm ./results/png_images/*.png");
	#print("File removed! \n");
}

#Setting parameters
$epochs = 150;	#Total 150 png images will be created
$dir_path = "results/png_images/";	#Dir where all png files are stored
$c = 1;	#Default value
$tval = 0.0;	#Initial value
$incr = (2*$L/$c)/$epochs;	#Increment size

#Extract gnu.batch.template. It assumes that gnu.batch.template exists inside waveqn folder
$filecontent = `cat gnu.batch.template`;	#Extarcting file content
#do substitution
(my $filecontent = $filecontent) =~ s/XRANGE/$xrange/g;
(my $filecontent = $filecontent) =~ s/YRANGE/$yrange/g;
(my $filecontent = $filecontent) =~ s/TITLE/$funchoice with $funparam/g;
(my $filecontent = $filecontent) =~ s/FUNCHOICE/$funchoice/g;
#print("$filecontent");

open(OUTFILE, '>', "./results/gnu.batch") or die("Problem faced while creating gnu.batch file.");
  print(OUTFILE "$filecontent");	#Writing lines in gnu.batch
close(OUTFILE);

#Run twave, create data.t, convert it to wave.png, rename and store it to $dir_path
for ($i=0; $i<$epochs; $i++) {
  $pngname = $dir_path . "wave" . "$i" . ".png";
  #Complie twave and funval files
  system("gfortran -DFUNCHOICE=$funchoice -DEXTEND=$extend -o ./results/twave twave.F90 funval.F90");
  system("./results/twave $tval $c $cmdargs");	#Pass suitable paramaters
  #print("cmdargs $cmdargs");

  system("gnuplot ./results/gnu.batch");	#calling gnu.batch file to create plot
  #system("gnuplot gnu.batch.template");	#calling gnu.batch.template file to create plot
  
  system("mv wave.png $pngname");	#move created file to $dir_path
  $filelist .= "$pngname ";	#Needed to create wave.png file
  $tval += $incr;	#Increment value for next iteration
}

#Create .gif file
$gifloc = "./results/wave.gif";
$gifname = "Output of $funchoice function";
$outputgif = "<center><img src=$gifloc alt=$gifname>";
$loopparam = 5; #How many times to iterate the gif animation
system("convert -delay 15 -loop $loopparam $filelist $gifloc");

#Print results after substituting strings
substitute($funchoice, $funparam, "", $outputgif);
exit(0);	#program ends here


# subroutine cgiparse() parses the contents of the cgi input into
# an associative array where the value of each name-value pair is stored
# using the name as the key.
#
# typical use:
#                %query = &cgiparse();
#                if (defined $query{'Name'} ) ....
#
# a string may be passed as a parameter for testing or if the cgi input
# has already been collected

sub cgiparse {

  local ($data) = @_;

  # fetch the data for this request

  if ( defined $data ) {
    # we have data passed
  }
  elsif ( $ENV{'REQUEST_METHOD'} eq 'POST' ) {
    # read it from standard input
    local ($len) = $ENV{'CONTENT_LENGTH'};
    if ( read (STDIN, $data, $len) != $len ) {
      die ("Error reading 'POST' data\n");
    }
  }
  else {
    # fetch from environment variable
    $data = $ENV{'QUERY_STRING'};
  }

  local (%qs) = ();        #resulting hash

  # the data is encoded as name1=val1&name2=val2 -- etc
  # first split on name/value pairs
  foreach $qs ( split ('&', $data) ) {
    # then split name and value
    local ($name, $val) = split ('=', $qs);
    # url decode and put in resultant hash array
    $name = &url_decode ($name);
    if (defined $qs{$name} ) {
      # multiple values -- append using \0 separator
      $qs{$name} .= "\0" . &url_decode ($val);
    }
    else {
      # store it
      $qs{$name} = &url_decode ($val);
    }
  }
  # and return it
  %qs;
}	#cgiparse() ends here


# subroutine url_decode() decodes url data

sub url_decode {

  local ($s) = @_;

  # translate + to space, and %xx to the character code

  $s =~ tr/+/ /;
  $s =~ s/%([0-9A-F][0-9A-F])/pack("C",oct("0x$1"))/ge;
  $s;
}	#url_decode() ends here


# substitute strings in result file
sub substitute {

  my ($funchoice, $funparam, $funcomment, $outputgif) = @_;	#Input parameters

  #Write the findings in the results.html
  open(INFILE, "./results.html.template") or die("Make sure results.html.template file exists inside waveqn folder.");

  while (<INFILE>){
    if(/FUNCHOICE/){s/FUNCHOICE/$funchoice/g;}
    if(/FUNPARAM/){s/FUNPARAM/$funparam/g;}
    if(/FUNCOMMENT/){s/FUNCOMMENT/$funcomment/g;}
    if(/OUTPUTGIF/){s/OUTPUTGIF/$outputgif/g;}
    print;
  }
  close(INFILE);
}	#substitute() ends here
