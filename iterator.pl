#! /usr/bin/perl

# Driving portion of the Phage-to-Shark program.
# Sets input parameters and creates a .gif of the output
# Options for single runs, multiple runs

# Author: Jon Parsons
# Date: 1-11-19

use Time::Piece;

my $timeVar = localtime -> strftime('%d%m%y');

print "Single run, range, or statistical run (10 runs)? (s/r/t) \n";
my $runnum = <STDIN>;
chomp $runnum;

$grid = 100;
$numtime = 500;
$percentcover = 0.5;
$threshold = 1.2;
$sharkmass = 150;
$dayavg = 5;
$rate = 0.5;
$corBacNew = 1.0;
$corBacGrow = 1.0;
$adsorpFac = 0.02;
$bacDeath = 0.2;
$bacBurst = 50;
$phagedie = 0.5;
$fisheatmult = 1.0;
$fgrowfact = 1.0;
$disFlagin = 'N';
$disLevel = 3;

my @iterArray = ($grid, $numtime, $percentcover, $threshold, $sharkmass, $dayavg, $rate, $corBacNew, $corBacGrow, $adsorpFac, $bacDeath, $bacBurst, $phagedie, $fisheatmult, $fgrowfact, $disFlagin, $disLevel);

my @nameArray = ("Gridpoints", "Number of Timesteps", "Coral Percentage", "New Coral Threshold", "Shark Biomass", "Avg Number of Days Between Shark Events", 
				 "Bacterial Growth Rate", "Bacteria-New Coral Intensity", "Bacteria-Coral Growth Intensity", "Adsorption Factor Coefficient", "Rate of Bacterial Death", "Phage Burst Count", "Phage Decay Rate",
				 "Fish-Algae Intensity", "Fish Growth Rate", "Disaster Flag", "Disaster Intensity");

my @fileArray = ("avgcortime.dat", "bacttime.dat","Corgrowth.dat","cortottime.dat","fishtottime.dat","perctime.dat","phagelysratio.dat","vmr.dat");

if ($runnum eq 's') {

runit(\@iterArray);

system("mkdir Outputs");

print "Create a movie of the reef? (y/n) \n";
my $movie = <STDIN>;
chomp $movie;

if ($movie eq 'y' ) {
	coralmovie()
}

my @avgcor = ("avgcortime.dat", "Average Coral", "avgcoral.png", 1);
my @bacttime = ("bacttime.dat", "Bacteria Population", "bactpop.png", 1);
my @corgrow = ("Corgrowth.dat", "Coral Growth", "corgrow.png", 3);
my @cortot = ("cortottime.dat","Total Coral","cortot.png", 1);
my @fishtot = ("fishtottime.dat","Fish Population","fishtot.png", 1);
my @micropop = ("microbepops.dat","Microbe Populations","micropops.png", 2);
my @microspecs = ("microbespecs.dat","Microbe Species Count","microspecs.png", 2);
my @perctime = ("perctime.dat","Percentage of Coral Coverage","perctime.png", 1);
my @phlyrat = ("phagelysratio.dat","Phage - Lysogen Ratio","phlyrat.png",1);
my @vmrat = ("vmr.dat", "VMR", "VMR.png",1);

singlerun(\@avgcor);
singlerun(\@bacttime);
singlerun(\@corgrow);
singlerun(\@cortot);
singlerun(\@fishtot);
singlerun(\@micropop);
singlerun(\@microspecs);
singlerun(\@perctime);
singlerun(\@phlyrat);
singlerun(\@vmrat);

$timeVar .= "single";

system("mkdir /home/jon/Desktop/Phage2Shark/Runs");
system("cp -aR /home/jon/Desktop/Phage2Shark/Outputs /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("cp -aR /home/jon/Desktop/Phage2Shark/General /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("mv /home/jon/Desktop/Phage2Shark/inputs.dat /home/jon/Desktop/Phage2Shark/Runs/$timeVar/");
system("rm -r /home/jon/Desktop/Phage2Shark/General");
system("rm -r /home/jon/Desktop/Phage2Shark/Coral");
system("rm -r /home/jon/Desktop/Phage2Shark/Outputs");

##########################################################################################################################
# Begin Multiple run Portion
##########################################################################################################################
} elsif ($runnum eq 'r') {

print "Which variable is to be iterated over? Input the number of your choice.";
print "Choices: Initial Coral Coverage (1); New coral threshold (2); Piscivore Mass (3) \n";
print "Average days between Shark Events (4); Bacterial Growth Rate (5); Influence of Bacteria on New Coral (6);\n";
print "Influence of Bacteria on Coral Growth (7); Phage Adsorption coefficient (8); Rate of bacterial death (9)\n";
print "Burst Count of Phage (10); Rate of Phage decay (11); Fish Influence Factor (12); Rate of Fish Growth (13)\n";

my $deltavar = <STDIN>;
chomp $deltavar;
$deltavar = $deltavar + 1;

print "Enter the initial value\n";
my $iniVar = <STDIN>;
chomp $iniVar;

print "Enter the final value\n";
my $finVar = <STDIN>;
chomp $finVar;

my $numruns = 5;

my $varIter = ($finVar - $iniVar)/$numruns;

@iterArray[$deltavar] = $iniVar;

system("mkdir Outputs");


for (my $i = 1; $i <= $numruns; $i++) {
	
	runit(\@iterArray);
	
	@iterArray[$deltavar] = @iterArray[$deltavar] + $varIter;

	my $dirgen = '/home/jon/Desktop/Phage2Shark/General';
	chdir($dirgen);

	system("mv avgcortime.dat avgcortime$i.dat");
	system("mv bacttime.dat bacttime$i.dat");
	system("mv Corgrowth.dat Corgrowth$i.dat");
	system("mv cortottime.dat cortottime$i.dat");
	system("mv fishtottime.dat fishtottime$i.dat");
	system("mv microbepops.dat microbepops$i.dat");
	system("mv microbespecs.dat microbespecs$i.dat");
	system("mv perctime.dat perctime$i.dat");
	system("mv phagelysratio.dat phagelysratio$i.dat");
	system("mv vmr.dat vmr$i.dat");
	
	my $dir = '/home/jon/Desktop/Phage2Shark';
	chdir($dir);

}

	my @avgcor = ("avgcortime", "Average Coral", "avgcoral.png", 1, @nameArray[$deltavar], $iniVar, $varIter);
	my @bacttime = ("bacttime", "Bacteria Population", "bactpop.png", 1, @nameArray[$deltavar], $iniVar, $varIter);
	my @corgrow = ("Corgrowth", "Coral Growth", "corgrow.png", 3, @nameArray[$deltavar], $iniVar, $varIter);
	my @cortot = ("cortottime","Total Coral","cortot.png", 1, @nameArray[$deltavar], $iniVar, $varIter);
	my @fishtot = ("fishtottime","Fish Population","fishtot.png", 1, @nameArray[$deltavar], $iniVar, $varIter);	
	my @micropop = ("microbepops","Microbe Populations","micropops.png", 2, @nameArray[$deltavar], $iniVar, $varIter);
	my @microspecs = ("microbespecs","Microbe Species Count","microspecs.png", 2, @nameArray[$deltavar], $iniVar, $varIter);
	my @perctime = ("perctime","Percentage of Coral Coverage","perctime.png", 1, @nameArray[$deltavar], $iniVar, $varIter);
	my @phlyrat = ("phagelysratio","Phage - Lysogen Ratio","phlyrat.png", 1, @nameArray[$deltavar], $iniVar, $varIter);
	my @vmrat = ("vmr", "VMR", "VMR.png",1, @nameArray[$deltavar], $iniVar, $varIter);


	rangerun(\@avgcor);
	rangerun(\@bacttime);
	rangerun(\@corgrow);
	rangerun(\@cortot);
	rangerun(\@fishtot);
	rangerun(\@micropop);
	rangerun(\@microspecs);
	rangerun(\@perctime);
	rangerun(\@phlyrat);
	rangerun(\@vmrat);

my $dir = '/home/jon/Desktop/Phage2Shark';
chdir($dir);

$timeVar .= "range";

system("mkdir /home/jon/Desktop/Phage2Shark/Runs");
system("cp -aR /home/jon/Desktop/Phage2Shark/Outputs /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("cp -aR /home/jon/Desktop/Phage2Shark/General /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("mv /home/jon/Desktop/Phage2Shark/inputs.dat /home/jon/Desktop/Phage2Shark/Runs/$timeVar/");
system("rm -r /home/jon/Desktop/Phage2Shark/General");
system("rm -r /home/jon/Desktop/Phage2Shark/Coral");
system("rm -r /home/jon/Desktop/Phage2Shark/Outputs");

###################################################################################################################################
} else {  # Statistics run
###################################################################################################################################

print "Which variable is to be iterated over? Input the number of your choice.";
print "Choices: Initial Coral Coverage (1); New coral threshold (2); Piscivore Mass (3) \n";
print "Average days between Shark Events (4); Bacterial Growth Rate (5); Influence of Bacteria on New Coral (6);\n";
print "Influence of Bacteria on Coral Growth (7); Phage Adsorption coefficient (8); Rate of bacterial death (9)\n";
print "Burst Count of Phage (10); Rate of Phage decay (11); Fish Influence Factor (12); Rate of Fish Growth (13)\n";

my $deltavar = <STDIN>;
chomp $deltavar;
$deltavar = $deltavar;

print "Enter the initial value\n";
my $iniVar = <STDIN>;
chomp $iniVar;

print "Enter the final value\n";
my $finVar = <STDIN>;
chomp $finVar;

my $numruns = 10;

my $varIter = ($finVar - $iniVar)/$numruns;

@iterArray[$deltavar] = $iniVar;

system("mkdir Outputs");

	my $vmrAvgF = '/home/jon/Desktop/Phage2Shark/Outputs/avgvmr.dat';
	my $phlyAvgF = '/home/jon/Desktop/Phage2Shark/Outputs/avgphly.dat';
	my $fishAvgF = '/home/jon/Desktop/Phage2Shark/Outputs/avgfishtot.dat';
	my $totAvgF = '/home/jon/Desktop/Phage2Shark/Outputs/avgtotcor.dat';
	my $bactAvgF = '/home/jon/Desktop/Phage2Shark/Outputs/avgbact.dat';
	my $avgAvgF = '/home/jon/Desktop/Phage2Shark/Outputs/avgcoravg.dat';
	my $percAvgF = '/home/jon/Desktop/Phage2Shark/Outputs/avgperc.dat';


open(my $pa, '>', $percAvgF);
open(my $aa, '>', $avgAvgF);
open(my $ba, '>', $bactAvgF);
open(my $ta, '>', $totAvgF);
open(my $fa, '>', $fishAvgF);
open(my $ha, '>', $phlyAvgF);
open(my $va, '>', $vmrAvgF);

# Iterates variables
for (my $j = 1; $j <= $numruns; $j++){

	my $percAvg = 0.0;
	my $avgAvg = 0.0;
	my $bactAvg = 0.0;
	my $totAvg = 0.0;
	my $fishAvg = 0.0;
	my $phlyAvg = 0.0;
	my $vmrAvg = 0.0;

# Multiple runs for averages
	for (my $i = 1; $i <= $numruns; $i++) {

		my $dirgen = '/home/jon/Desktop/Phage2Shark';
		chdir($dirgen);

		print "Beginning Run $j;$i\n";
		system("rm -r /home/jon/Desktop/Phage2Shark/General");


		runit(\@iterArray);
	
		my $dirgen = '/home/jon/Desktop/Phage2Shark/General';
		chdir($dirgen);
		
	for (my $k = 0; $k <= 6; $k++){					
			open (SF, "/home/jon/Desktop/Phage2Shark/General/$fileArray[$k]"); 		
			
			my @tvals;
		
			<SF>;
			while(<SF>){
				my ($disc, $keep) = split;
				push @tvals, $keep;
			}
			
			if ($k eq 0) {		
						$avgAvg = $avgAvg + ($tvals[-1] - $tvals[0]);
				} elsif ($k eq 1) {
						$bactAvg = $bactAvg + ($tvals[-1] - $tvals[0]);
				} elsif ($k eq 2) {
						$totAvg = $totAvg + ($tvals[-1] - $tvals[0]);
				} elsif ($k eq 3) {
						$fishAvg = $fishAvg + ($tvals[-1] - $tvals[0]);
				} elsif ($k eq 4) {
						$percAvg = $percAvg + ($tvals[-1] - $tvals[0]);
				} elsif ($k eq 5) {
						$phlyAvg = $phlyAvg + ($tvals[-1] - $tvals[0]);	
				} elsif ($k eq 6) {
						$vmrAvg = $vmrAvg + ($tvals[-1] - $tvals[0]);
				} 

			undef @tvals;
			
			close(SF);
			
		}		
		
	}
	
	
	
	$avgAvg = $avgAvg/$numruns;
	$bactAvg = $bactAvg/$numruns;
	$totAvg = $totAvg/$numruns;
	$fishAvg = $fishAvg/$numruns;
	$percAvg = $percAvg/$numruns;
	$phlyAvg = $phlyAvg/$numruns;
	$vmrAvg = $vmrAvg/$numruns;
		
	my $dirgen = '/home/jon/Desktop/Phage2Shark/Outputs';
	chdir($dirgen);
		
	print $pa qq{$j "@nameArray[$deltavar+1]=@iterArray[$deltavar]" $percAvg \n};
	print $aa qq{$j "@nameArray[$deltavar+1]=@iterArray[$deltavar]" $avgAvg \n};
	print $ba qq{$j "@nameArray[$deltavar+1]=@iterArray[$deltavar]" $bactAvg \n};
	print $ta qq{$j "@nameArray[$deltavar+1]=@iterArray[$deltavar]" $totAvg \n};
	print $fa qq{$j "@nameArray[$deltavar+1]=@iterArray[$deltavar]" $fishAvg \n};
	print $ha qq{$j "@nameArray[$deltavar+1]=@iterArray[$deltavar]" $phlyAvg \n};
	print $va qq{$j "@nameArray[$deltavar+1]=@iterArray[$deltavar]" $vmrAvg \n};
	
	@iterArray[$deltavar] = @iterArray[$deltavar] + $varIter;
	
}

close $pa;
close $aa;
close $ba;
close $ta;
close $fa;
close $ha;
close $va;

	@iterArray[$deltavar] = @iterArray[$deltavar] + $varIter;
	
	my $dir = '/home/jon/Desktop/Phage2Shark';
	chdir($dir);

	$timeVar .= "stats";
	my $vmrAvgF = 'avgvmr.dat';
	my $phlyAvgF = 'avgphly.dat';
	my $fishAvgF = 'avgfishtot.dat';
	my $totAvgF = 'avgtotcor.dat';
	my $bactAvgF = 'avgbact.dat';
	my $avgAvgF = 'avgcoravg.dat';
	my $percAvgF = 'avgperc.dat';
	
	my @avgcor = ($avgAvgF, "Average Coral", "avgcoral.png", 1, @nameArray[$deltavar]);
	my @bacttime = ($bactAvgF, "Bacteria Population", "bactpop.png", 1, @nameArray[$deltavar]);
	my @cortot = ($totAvgF,"Total Coral","cortot.png", 1, @nameArray[$deltavar]);
	my @fishtot = ($fishAvgF,"Fish Population","fishtot.png", 1, @nameArray[$deltavar]);	
	my @perctime = ($percAvgF,"Percentage of Coral Coverage","perctime.png", 1, @nameArray[$deltavar]);
	my @phlyrat = ($phlyAvgF,"Phage - Lysogen Ratio","phlyrat.png", 1, @nameArray[$deltavar]);
	my @vmrat = ($vmrAvgF, "VMR", "VMR.png",1, @nameArray[$deltavar]);

	statrun(\@avgcor);
	statrun(\@bacttime);
	statrun(\@cortot);
	statrun(\@fishtot);
	statrun(\@perctime);
	statrun(\@phlyrat);
	statrun(\@vmrat);

system("mkdir /home/jon/Desktop/Phage2Shark/Runs");
system("cp -aR /home/jon/Desktop/Phage2Shark/Outputs /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("cp -aR /home/jon/Desktop/Phage2Shark/General /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("mv /home/jon/Desktop/Phage2Shark/inputs.dat /home/jon/Desktop/Phage2Shark/Runs/$timeVar/");
system("rm -r /home/jon/Desktop/Phage2Shark/General");
system("rm -r /home/jon/Desktop/Phage2Shark/Coral");
system("rm -r /home/jon/Desktop/Phage2Shark/Outputs");


}
############################################################################################################
# END OF MAIN
############################################################################################################
############################################################################################################
# Run the Model Subroutine here

sub runit {

my ($inargs) = @_;
my @arrin = @{$inargs};


my $fortinput = "inputs.dat";
open(my $fh, '>', $fortinput);
print $fh "@arrin[0]\n";
print $fh "@arrin[1]\n";
print $fh "@arrin[2]\n";
print $fh "@arrin[3]\n";
print $fh "@arrin[4]\n";
print $fh "@arrin[5]\n";
print $fh "@arrin[6]\n";
print $fh "@arrin[7]\n";
print $fh "@arrin[8]\n";
print $fh "@arrin[9]\n";
print $fh "@arrin[10]\n";
print $fh "@arrin[11]\n";
print $fh "@arrin[12]\n";
print $fh "@arrin[13]\n";
print $fh "@arrin[14]\n";
print $fh "@arrin[15]\n";
if (@arrin[15] ne 'N') {
	print $fh "@arrin[16]\n";
}

close $fh;

system("./Phage2Shark.x < inputs.dat");

}

##########################################################################################################
# Movie making subroutine

sub coralmovie {
my $dir = '/home/jon/Desktop/Phage2Shark/Coral';
system("cp /home/jon/Desktop/Phage2Shark/gnubatchfiles/gnuplotcont.batch.temp $dir");

opendir my $dh, $dir;

chdir($dir);
my @files = glob("*.dat");
$file_num = $#files;
print "Number of files: $file_num\n";

for ($i=0; $i<=$file_num; $i++){

# Open gnuplot batch template file for string replacement with error check
	$FHT = open (TEMP, "./gnuplotcont.batch.temp");

# Error check for file open
	if (!$FHT){
		print "Gnu batch template file missing from directory, unable to complete process. \n";
		exit;
	}

# Open file for final gnuplot batch 
	$FHF = open ( BATCH, ">./gnu.batch");
	
# Error check for file open
	if (!$FHF){
		print "Unable to open gnu.batch file. Exiting. \n";
		exit;
	}
# Write chenges to file.
	while (<TEMP>) {
		s/XDX/$grid/;
		s/YDY/$grid/;
		s/XTX/$i/;
		s/XFILEX/$files[$i]/ge;
		print BATCH;
	}	
	
	$fname = "cortime".$i.".png";
	system("gnuplot gnu.batch");
	system("mv coralt.png $fname");
	$filelist .= " $fname";

# Close the files
close(TEMP);
close(BATCH);

system("rm gnu.batch");

}

# Creates the .gif file from the images generated by the above loop
	system("convert -delay 4 -loop 0 $filelist coral.gif");
	system("ffmpeg -f gif -i coral.gif coral.mp4");
	system("mv coral.mp4 /home/jon/Desktop/Phage2Shark/Outputs");
# Clean the intermediate files
	system("rm cortime*.png");
}
#####################################################################################################
#General files here

sub singlerun {

my ($inargs) = @_;
my @arrin = @{$inargs};

my $curfile = @arrin[0];
my $curtitle = @arrin[1];
my $curname = @arrin[2];
my $multflag = @arrin[3] ;

my $dirgen = '/home/jon/Desktop/Phage2Shark/General';
my $batchpath = '/home/jon/Desktop/Phage2Shark/gnubatchfiles/genfiles.batch.temp';
my $batchout = "/home/jon/Desktop/Phage2Shark/General/gnucur.batch";

chdir($dirgen);

$FHT = open (TEMP, $batchpath);

# Error check for file open
	if (!$FHT){
		print "Gnu batch template (Average coral) file missing from directory, unable to complete process. \n";
		exit;
	}

# Open file for final gnuplot batch 
$FHF = open ( BATCH, '>'.$batchout );
	
# Error check for file open
	if (!$FHF){
		print "Unable to open gnu.batch file (Average coral). Exiting. \n";
		exit;
	}
	
if ($multflag eq 1) {
# Write chenges to file.
	while (<TEMP>) {
		s/XDX/$numtime/;
		s/XTITLEX/$curtitle/ge;
		s/XNAMEX/$curname/ge;
		s/XLINE1X/plot "$curfile" with lines/;
		s/XLINE2X/ /;
		print BATCH;
	}	

} elsif ($multflag eq 2) {

	while (<TEMP>) {
		s/XDX/$numtime/;
		s/XTITLEX/$curtitle/ge;
		s/XNAMEX/$curname/ge;
		s/XLINE1X/set key autotitle columnhead/;
		s/XLINE2X/plot for [col=2:4] "$curfile" using 1:col with lines/;
		print BATCH;
	}

} else {

	while (<TEMP>) {
		s/XDX/$numtime/;
		s/XTITLEX/$curtitle/ge;
		s/XNAMEX/$curname/ge;
		s/XLINE1X/set key autotitle columnhead/;
		s/XLINE2X/plot "$curfile" using 1:2 with lines, "$curfile" using 1:3 pt 7 ps 0.2/;
		print BATCH;
	}

}
	
system("gnuplot gnucur.batch");
system("mv $curname /home/jon/Desktop/Phage2Shark/Outputs");

# Close the files
close(TEMP);
close(BATCH);

system("rm gnucur.batch");	

}

###########################################################################################################################
# Multiple run files here

sub rangerun {

my ($inargs) = @_;
my @arrin = @{$inargs};

my $curfile = @arrin[0];
my $curtitle = @arrin[1];
my $curname = @arrin[2];
my $multflag = @arrin[3];
my $varIter = @arrin[4];
my $varVal1 = @arrin[5];
my $delVar = @arrin[6];

my $varVal2 = $varVal1 + $delVar;
my $varVal3 = $varVal1 + 2*$delVar;
my $varVal4 = $varVal1 + 3*$delVar;
my $varVal5 = $varVal1 + 4*$delVar;

my $curfile1 = $curfile."1.dat";
my $curfile2 = $curfile."2.dat";
my $curfile3 = $curfile."3.dat";
my $curfile4 = $curfile."4.dat";
my $curfile5 = $curfile."5.dat";


my $dirgen = '/home/jon/Desktop/Phage2Shark/General';
my $batchpath = '/home/jon/Desktop/Phage2Shark/gnubatchfiles/genfilesmult.batch.temp';
my $batchpathmic = '/home/jon/Desktop/Phage2Shark/gnubatchfiles/genfilesmultmicrobe.batch.temp';
my $batchout = "/home/jon/Desktop/Phage2Shark/General/gnucur.batch";
my $batchout2 = "/home/jon/Desktop/Phage2Shark/General/gnucurmic.batch";

chdir($dirgen);
	
if ($multflag eq 1) {

$FHT = open (TEMP, $batchpath);

# Error check for file open
	if (!$FHT){
		print "Gnu batch template file missing from directory, unable to complete process. \n";
		exit;
	}

# Open file for final gnuplot batch 
$FHF = open ( BATCH, '>'.$batchout );
	
# Error check for file open
	if (!$FHF){
		print "Unable to open gnu.batch file. Exiting. \n";
		exit;
	}
	
# Write chenges to file.
	while (<TEMP>) {
		s/XDX/$numtime/;
		s/XTITLEX/$curtitle/ge;
		s/XNAMEX/$curname/ge;
		s/XLINE1X/plot "$curfile1" using 1:2 title "$varIter = $varVal1" with lines,  \\/;
		s/XLABEL1X/    "$curfile2" using 1:2 title "$varIter = $varVal2" with lines,  \\/;
		s/XLINE2X/     "$curfile3" using 1:2 title "$varIter = $varVal3" with lines,  \\/;
		s/XLABEL2X/    "$curfile4" using 1:2 title "$varIter = $varVal4" with lines,  \\/;
		s/XLINE3X/     "$curfile5" using 1:2 title "$varIter = $varVal5" with lines		/;
		s/XLABEL3X/ /;
		s/XLINE4X/ /;
		s/XLABEL4X/ /;
		s/XLINE5X/ /;
		s/XLABEL5X/ /;
		s/XLINE6X/ /;
		print BATCH;
	}	

# Close the files
close(TEMP);
close(BATCH);

} elsif ($multflag eq 2) {

$FHT = open (TEMP, $batchpath);
$FHF = open ( BATCH, '>'.$batchout );


	while (<TEMP>) {
		s/XDX/$numtime/;
		s/XTITLEX/$curtitle/ge;
		s/XNAMEX/$curname/ge;
		s/XLINE1X/set key outside /;
		s/XLABEL1X/plot 1\/0 with points pt -1 t "$varIter = $varVal1", \\/;
		s/XLINE2X/      for [col=2:4] "$curfile1" using 1:col with lines lt col, \\/;
		s/XLABEL2X/ 		1\/0 with points pt -1 t "$varIter = $varVal2", \\/;
		s/XLINE3X/	    for [col=2:4] "$curfile2" using 1:col with lines lt col+3, \\/;
		s/XLABEL3X/ 		1\/0 with points pt -1 t "$varIter = $varVal3", \\/;
		s/XLINE4X/	    for [col=2:4] "$curfile3" using 1:col with lines lt col+6, \\/;
		s/XLABEL4X/ 		1\/0 with points pt -1 t "$varIter = $varVal4", \\/;
		s/XLINE5X/      for [col=2:4] "$curfile4" using 1:col with lines lt col+9, \\/;
		s/XLABEL5X/ 		1\/0 with points pt -1 t "$varIter = $varVal5", \\/;
		s/XLINE6X/	    for [col=2:4] "$curfile5" using 1:col with lines lt col+12/;
		print BATCH;
	}

close(TEMP);
close(BATCH);


} elsif ($multflag eq 3) {

$FHT = open (TEMP, $batchpath);
$FHF = open ( BATCH, '>'.$batchout );

	while (<TEMP>) {
		s/XDX/$numtime/;
		s/XTITLEX/$curtitle/ge;
		s/XNAMEX/$curname/ge;
		s/XLINE1X/set key outside /;
		s/XLABEL1X/plot 1\/0 with points pt -1 t "$varIter = $varVal1", \\/;
		s/XLINE2X/     "$curfile1" using 1:2 with lines lc rgbcolor "green", "$curfile1" using 1:3 pt 7 ps 0.6 lc rgbcolor "green", \\/;
		s/XLABEL2X/ 		1\/0 with points pt -1 t "$varIter = $varVal2", \\/;
		s/XLINE3X/     "$curfile2" using 1:2 with lines lc rgbcolor "red", "$curfile2" using 1:3 pt 7 ps 0.6 lc rgbcolor "red", \\/;
		s/XLABEL3X/ 		1\/0 with points pt -1 t "$varIter = $varVal3", \\/;
		s/XLINE4X/     "$curfile3" using 1:2 with lines lc rgbcolor "blue", "$curfile3" using 1:3 pt 7 ps 0.6 lc rgbcolor "blue", \\/;
		s/XLABEL4X/ 		1\/0 with points pt -1 t "$varIter = $varVal4", \\/;
		s/XLINE5X/     "$curfile4" using 1:2 with lines lc rgbcolor "black", "$curfile4" using 1:3 pt 7 ps 0.6 lc rgbcolor "black", \\/;
		s/XLABEL5X/ 		1\/0 with points pt -1 t "$varIter = $varVal5", \\/;
		s/XLINE6X/     "$curfile5" using 1:2 with lines lc rgbcolor "orange", "$curfile5" using 1:3 pt 7 ps 0.6 lc rgbcolor "orange" /;
		print BATCH;
	}
	
close(TEMP);
close(BATCH);

}
	
system("gnuplot gnucur.batch");
system("mv $curname /home/jon/Desktop/Phage2Shark/Outputs");

system("rm gnucur.batch");	

}
#####################################################################################################################################################
# Stat Run
#####################################################################################################################################################
sub statrun {

my ($inargs) = @_;
my @arrin = @{$inargs};

my $curfile = @arrin[0];
my $curtitle = @arrin[1];
my $curname = @arrin[2];
my $multflag = @arrin[3];
my $varIter = @arrin[4];

my $dirgen = '/home/jon/Desktop/Phage2Shark/General';
my $dirout = '/home/jon/Desktop/Phage2Shark/Outputs';
my $batchpath = '/home/jon/Desktop/Phage2Shark/gnubatchfiles/genfilesmult.batch.temp';
my $batchpathmic = '/home/jon/Desktop/Phage2Shark/gnubatchfiles/statfiles.batch.temp';
my $batchout = "/home/jon/Desktop/Phage2Shark/Outputs/gnucur.batch";
my $batchout2 = "/home/jon/Desktop/Phage2Shark/General/gnucurmic.batch";

chdir($dirgen);
	
$FHT = open (TEMP, $batchpathmic);

# Error check for file open
	if (!$FHT){
		print "Gnu batch template file missing from directory, unable to complete process. \n";
		exit;
	}

# Open file for final gnuplot batch 
$FHF = open ( BATCH, '>' .$batchout );
	
# Error check for file open
	if (!$FHF){
		print "Unable to open gnu.batch file. Exiting. \n";
		exit;
	}
	
# Write chenges to file.
	while (<TEMP>) {
		s/XDX/$numtime/;
		s/XTITLEX/$curtitle/ge;
		s/XNAMEX/$curname/ge;
		s/XLINE1X/plot "$curfile" using 1:3:xtic(2) with boxes /;
		s/XLABEL1X/ /;
		print BATCH;
	}

close(TEMP);
close(BATCH);

chdir($dirout);

system("gnuplot gnucur.batch");
#system("mv $curname /home/jon/Desktop/Phage2Shark/Outputs");

system("rm gnucur.batch");	

}
	
	
	
