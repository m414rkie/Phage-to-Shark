#! /usr/bin/perl

# Driving portion of the Phage-to-Shark program.
# Sets input parameters and creates a .gif of the output
# Options for single runs, multiple runs, and time-averaged runs.

# Author: Jon Parsons
# Date: 1-11-19

## Issue with VMR graphing

use Time::Piece;

my $timeVar = localtime -> strftime('%d%m%y');

print "Single run, range, or statistical run (10 runs)? (s/r/t) \n";
my $runnum = <>;
chomp $runnum;

$grid = 100;
$numtime = 2000;
$percentcover = 0.5;
$threshold = 1.0;
$sharkmass = 20;
$dayavg = 6;
$rate = 1.0;
$corBacNew = 1.0;
$corBacGrow = 1.0;
$adsorpFac = 1.0;
$bacDeath = 0.5;
$bacBurst = 50;
$phagedie = 0.5;
$fisheatmult = 260.0;
$fgrowfact = 0.003;
$diffco = 0.01;
$deconst = 0.007;
$disFlagin = 'N';
$disLevel = 5;

$adj_flag = 'N';

$threshold_2nd = 1.0;
$sharkmass_2nd = 40;
$dayavg_2nd = 6;
$rate_2nd = 0.55;
$corBacNew_2nd = 1.0;
$corBacGrow_2nd = 1.0;
$adsorpFac_2nd = 1.0;
$bacDeath_2nd = 0.5;
$bacBurst_2nd = 75;
$phagedie_2nd = 0.5;
$fisheatmult_2nd = 5000.0;
$fgrowfact_2nd = 0.003;
$diffco_2nd = 0.015;
$deconst_2nd = 0.02;

$shift_2nd = 17;

# Population injection/removal for fish and microbes. microbes over entire reef?
# Fish effect switch to dF/F instead of Fk.

my @iterArray = ($grid, $numtime, $percentcover, $threshold, $sharkmass, $dayavg,
								$rate, $corBacNew, $corBacGrow, $adsorpFac, $bacDeath, $bacBurst,
								$phagedie, $fisheatmult, $fgrowfact, $diffco, $deconst, $disFlagin,
								$disLevel, $adj_flag, $threshold_2nd, $sharkmass_2nd, $dayavg_2nd,
								$rate_2nd, $corBacNew_2nd, $corBacGrow_2nd, $adsorpFac_2nd, $bacDeath_2nd,
								$bacBurst_2nd, $phagedie_2nd, $fisheatmult_2nd, $fgrowfact_2nd,
								$diffco_2nd, $deconst_2nd);



my @nameArray = ("Gridpoints", "Number_of_Timesteps", "Coral_Percentage", "New_Coral_Threshold",
 								 "Shark_Biomass", "Avg_Days_Between_Shark_Events",
				         "Bacterial_Growth_Rate", "Bacteria_New_Coral_Intensity",
				  		 	 "Bacteria_Coral_Growth_Intensity", "Adsorption_Factor_Coefficient",
								 "Rate_of_Bacterial_Death", "Phage_Burst_Count", "Phage_Decay_Rate",
								 "Fish_Algae_Intensity", "Fish_Growth_Rate","Diffusion_Coefficient",
								 "Algae_Pressure", "Disaster_Flag", "Disaster_Intensity");

if ($runnum eq 's') {

runit(\@iterArray);

system("mkdir Outputs");

print "Create a movie of the reef? (y/n) \n";
my $movie = <STDIN>;
chomp $movie;

if ($movie eq 'y' ) {
	coralmovie()
}

singlerun();

$timeVar .= "_single";

if ($adj_flag eq 'D') {
	$timeVar .= "_adj";
}

my $q = 0;

system("mkdir /home/jon/Desktop/Phage2Shark/Runs");
system("cp --backup=numbered -aR /home/jon/Desktop/Phage2Shark/Outputs /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("cp --backup=numbered -aR /home/jon/Desktop/Phage2Shark/General /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("mv --backup=t /home/jon/Desktop/Phage2Shark/inputs.dat /home/jon/Desktop/Phage2Shark/Runs/$timeVar/");
system("rm -r /home/jon/Desktop/Phage2Shark/General");
system("rm -r /home/jon/Desktop/Phage2Shark/Coral");
system("rm -r /home/jon/Desktop/Phage2Shark/Outputs");

##########################################################################################################################
# Begin Multiple run Portion
##########################################################################################################################
} elsif ($runnum eq 'r') {

print "Iterate over first or second half variables? (1/2) \n";

my $which_half = int(<STDIN>);
chomp $which_half;

print "Which variable is to be iterated over? Input the number of your choice.";

print "Choices: \n
      Initial Coral Coverage              - 1  | New coral threshold                - 2 \n
      Piscivore Mass                      - 3  | Average days between Shark Events  - 4 \n
      Bacterial Growth Rate               - 5  | Influence of Bacteria on New Coral - 6 \n
      Bacterial Influence on Coral Growth - 7  | Phage Adsorption coefficient       - 8 \n
      Rate of bacterial death             - 9  | Burst Count of Phage               - 10 \n
      Rate of Phage decay                 - 11 | Fish Influence Factor              - 12 \n
      Rate of Fish Growth                 - 13 | Bacterial Diffusion Coefficient    - 14 \n
			Algae Pressure on Coral             - 15 |                                         \n";

my $choice_in = <STDIN>;
chomp $choice_in;
$choice_in = $choice_in + 1;

if ($which_half eq 1) {
 	$deltavar = $choice_in;
} elsif ($which_half eq 2) {
 	$deltavar = $choice_in + $shift_2nd;
}

my $deltavar2 = $deltavar - $shift_2nd;

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


for (my $i = 0; $i <= $numruns; $i++) {

	runit(\@iterArray);

	@iterArray[$deltavar] = @iterArray[$deltavar] + $varIter;

	my $val = @iterArray[$deltavar];

	if ($which_half eq 1) {
		@var_its[$i] = "@nameArray[$deltavar] = $val";
	} elsif ($which_half eq 2) {
		@var_its[$i] = "@nameArray[$deltavar2] = $val";
	}

	my $dirgen = '/home/jon/Desktop/Phage2Shark/General';
	chdir($dirgen);

my @data_files_abr = ("coral_percentage","coral_total","coral_average",
											"coral_delta","bact_pop","phage_pop","lys_pop",
											"bact_spec","phage_spec","lys_spec","fish_pop",
											"fish_delta","algae_perc","phage_lys_rat",
											"shark_evt","vmr","time","bact_pop_cor",
											"phage_pop_cor","lys_pop_cor",
											"bact_spec_cor","phage_spec_cor",
											"lys_spec_cor","phage_lys_rat_cor",
											"vmr_cor","area_cor","bact_pop_alg","phage_pop_alg",
											"lys_pop_alg","bact_spec_alg","phage_spec_alg",
											"lys_spec_alg","phage_lys_rat_alg","vmr_alg",
											"area_alg","bact_pop_bar","phage_pop_bar",
											"lys_pop_bar","bact_spec_bar","phage_spec_bar",
											"lys_spec_bar","phage_lys_rat_bar","vmr_bar",
											"area_bar");

foreach $file (@data_files_abr)	{
	my $j = $i + 1;
	system("mv $file.dat $file$j.dat");
}

	my $dir = '/home/jon/Desktop/Phage2Shark';
	chdir($dir);

}

	rangerun(\@var_its);

my $dir = '/home/jon/Desktop/Phage2Shark';
chdir($dir);

if ($adj_flag eq 'D') {
	$timeVar .= "_adj";
}

if ($which_half eq 1) {
	$timeVar .= "1_range_@nameArray[$deltavar]";
} elsif ($which_half eq 2) {
	my $deltavar2 = $deltavar - $shift_2nd;
	$timeVar .= "2_range_@nameArray[$deltavar]";
}

system("mkdir /home/jon/Desktop/Phage2Shark/Runs");
system("cp --backup=numbered -aR /home/jon/Desktop/Phage2Shark/Outputs /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("cp --backup=numbered -aR /home/jon/Desktop/Phage2Shark/General /home/jon/Desktop/Phage2Shark/Runs/$timeVar");
system("mv --backup=t /home/jon/Desktop/Phage2Shark/inputs.dat /home/jon/Desktop/Phage2Shark/Runs/$timeVar/");
system("rm -r /home/jon/Desktop/Phage2Shark/General");
system("rm -r /home/jon/Desktop/Phage2Shark/Coral");
system("rm -r /home/jon/Desktop/Phage2Shark/Outputs");

###################################################################################################################################
} else {  # Statistics run
###################################################################################################################################

print "Which variable is to be iterated over? Input the number of your choice. \n";
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
system("mv --backup=t /home/jon/Desktop/Phage2Shark/inputs.dat /home/jon/Desktop/Phage2Shark/Runs/$timeVar/");
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
print $fh "@arrin[0]\n"; # Grid
print $fh "@arrin[1]\n"; # Numtime
print $fh "@arrin[2]\n"; # Percent
print $fh "@arrin[3]\n"; # Threshold
print $fh "@arrin[4]\n"; # Sharkmass
print $fh "@arrin[5]\n"; # Dayavg
print $fh "@arrin[6]\n"; # Rate
print $fh "@arrin[7]\n"; # Corbacnew
print $fh "@arrin[8]\n"; # corbacgrow
print $fh "@arrin[9]\n"; # adsorp
print $fh "@arrin[10]\n"; # bacDeath
print $fh "@arrin[11]\n"; # bacburst
print $fh "@arrin[12]\n"; # phagedie
print $fh "@arrin[13]\n"; # fisheatmult
print $fh "@arrin[14]\n"; # fgrowfact
print $fh "@arrin[15]\n"; # diffco
print $fh "@arrin[16]\n"; # deconst
print $fh "@arrin[17]\n"; # Disflagin
if (@arrin[17] ne 'N') {
	print $fh "@arrin[18]\n"; # dislevel
}
print $fh "@arrin[19]\n"; # adjustment flag
if (@arrin[19] eq 'D') {
	print $fh "@arrin[20]\n"; # Threshold
	print $fh "@arrin[21]\n"; # Sharkmass
	print $fh "@arrin[22]\n"; # Dayavg
	print $fh "@arrin[23]\n"; # Rate
	print $fh "@arrin[24]\n"; # Corbacnew
	print $fh "@arrin[25]\n"; # corbacgrow
	print $fh "@arrin[26]\n"; # adsorp
	print $fh "@arrin[27]\n"; # bacDeath
	print $fh "@arrin[28]\n"; # bacburst
	print $fh "@arrin[29]\n"; # phagedie
	print $fh "@arrin[30]\n"; # fisheatmult
	print $fh "@arrin[31]\n"; # fgrowfact
	print $fh "@arrin[32]\n"; # diffco
	print $fh "@arrin[33]\n"; # deconst
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

my @data_files = ("coral_percentage.dat","coral_total.dat","coral_average.dat",
									"coral_delta.dat","bact_pop.dat","phage_pop.dat","lys_pop.dat",
									"bact_spec.dat","phage_spec.dat","lys_spec.dat","fish_pop.dat",
									"fish_delta.dat","algae_perc.dat","phage_lys_rat.dat",
									"shark_evt.dat","vmr.dat","time.dat");

my @dom_cor_files = ("bact_pop_cor.dat","phage_pop_cor.dat","lys_pop_cor.dat","bact_spec_cor.dat",
										"phage_spec_cor.dat","lys_spec_cor.dat","phage_lys_rat_cor.dat",
										"vmr_cor.dat","area_cor.dat","time.dat");

my @dom_alg_files = ("bact_pop_alg.dat","phage_pop_alg.dat","lys_pop_alg.dat","bact_spec_alg.dat",
 										"phage_spec_alg.dat","lys_spec_alg.dat","phage_lys_rat_alg.dat",
										"vmr_alg.dat","area_alg.dat","time.dat");

my @dom_bar_files = ("bact_pop_bar.dat","phage_pop_bar.dat","lys_pop_bar.dat","bact_spec_bar.dat",
										"phage_spec_bar.dat","lys_spec_bar.dat","phage_lys_rat_bar.dat",
										"vmr_bar.dat","area_bar.dat","time.dat");

my @title_names = ("Coral Percentage","Coral Total","Coral Average",
									"Coral Delta","Bacteria Population","Phage Population","Lysogen Population",
									"Bacteria Species","Phage Species","Lysogen Species","Fish Population",
									"Fish Delta","Algae Percentage","Phage Lysogen Ratio",
									"Shark Events","VMR","Time");

my @dom_cor_names = ("Bact Pop Coral","Phage Pop Coral","Lys Pop Coral","Bact. Spec. Coral",
										"Phage Spec. Coral","Lys Spec. Coral","Phage Lys Ratio Coral",
										"VMR Coral","Coral Area Percentage","time");

my @dom_alg_names = ("Bact Pop Algae","Phage Pop Algae","Lys Pop Algae","Bact. Spec. Algae",
										"Phage Spec. Algae","Lys Spec. Algae","Phage Lys Ratio Algae",
										"VMR Algae","Algae Area Percentage","time");

my @dom_bar_names = ("Bact Pop Barr","Phage Pop Barr","Lys Pop Barr","Bact. Spec. Barr",
										"Phage Spec. Barr","Lys Spec. Barr","Phage Lys Ratio Barr",
										"VMR Barr","Barr Area Percentage","time");

my @units = (" ", "(BU)","(BU Per 25 cm sq)","(dBU Per 25 cm sq)","(1/ml)","(1/ml)",
							"(1/ml)","(1/ml)","(1/ml)","(1/ml)","(FM)","(dFM)"," ", " "," "," ",
							"(Days)");

my $dirgen = '/home/jon/Desktop/Phage2Shark/General';
my $gnufile = "gnu.batch";
chdir($dirgen);

while (1) {

print "Choose 2 sets of data to plot against each other. Enter nothing to exit. \n";
print "Coral Percentage    -  1 | Coral Total          - 2 \n";
print "Coral Average       -  3 | Coral Delta          - 4 \n";
print "Bacteria Population -  5 | Phage Population     - 6 \n";
print "Lysogen Population  -  7 | Bacteria Species     - 8 \n";
print "Phage Species       -  9 | Lysogen Species      - 10\n";
print "Fish Population     - 11 | Fish Delta           - 12 \n";
print "Algae Percentage    - 13 | Phage-Lysogen Ratio  - 14 \n";
print "Shark Events        - 15 | VMR                  - 16 \n";
print "Time                - 17 | --------- \n";
print "One domain per graph please. \n";
print "Coral Domain      |      Algae Domain      |      Barrier Domain \n \n";
print "                  |                        |                        \n";
print "Bacteria pop - 21 |      Bacteria pop - 31 |      Bacteria pop - 41 \n";
print "Phage pop    - 22 |      Phage pop    - 32 |      Phage pop    - 42 \n";
print "Lys pop      - 23 |      Lys pop      - 33 |      Lys pop      - 43 \n";
print "Bact spec    - 24 |      Bact spec    - 34 |      Bact spec    - 44 \n";
print "Phage spec   - 25 |      Phage spec   - 35 |      Phage spec   - 45 \n";
print "Lys spec     - 26 |      Lys spec     - 36 |      Lys spec     - 46 \n";
print "Pha/lys rat  - 27 |      Pha/lys rat  - 37 |      Phag/lys rat - 47 \n";
print "vmr          - 28 |      vmr          - 38 |      vmr          - 48 \n";
print "Perc. cover  - 29 |      Perc. cover  - 39 |      Perc. cover  - 49 \n";
print "Time         - 30 |      Time         - 40 |      Time         - 50 \n";

print "\n First set of data (x axis): \n";
my $choice_one = <>;
chomp $choice_one;
return if ($choice_one eq "");
$choice_one = $choice_one - 1;

print "\n Second set of data (y axis): \n";
my $choice_two = <>;
chomp $choice_two;
$choice_two = $choice_two - 1;

unless (open FILE, '>'.$gnufile){
	die "\n Failed to create batch file for plotting $curfile. \n";
}

# Global
if ($choice_one lt 20){
	my $title = "@title_names[$choice_one] - @title_names[$choice_two]";
	my $output = "@title_names[$choice_one]_@title_names[$choice_two].png";
	my $file_one = "@data_files[$choice_one]";
	my $file_two = "@data_files[$choice_two]";

	print FILE "set terminal png \n
			set autoscale \n
			set title \"$title\"  \n
			set output \"$output\"  \n
			set xlabel \"@title_names[$choice_one] @units[$choice_one]\" \n
			set ylabel \"@title_names[$choice_two] @units[$choice_two]\" \n
			plot \"< paste $file_one $file_two\" using 1:2 title \"$title\" with lines";
# Coral
} elsif ($choice_one gt 19 && $choice_one le 29) {
	$choice_one = $choice_one - 20;
	$choice_two = $choice_two - 20;

	my $title = "@dom_cor_names[$choice_one] - @dom_cor_names[$choice_two]";
	my $output = "@dom_cor_names[$choice_one]_@dom_cor_names[$choice_two].png";
	my $file_one = "@dom_cor_files[$choice_one]";
	my $file_two = "@dom_cor_files[$choice_two]";

	print FILE "set terminal png \n
			set autoscale \n
			set title \"$title\"  \n
			set output \"$output\"  \n
			set xlabel \"@dom_cor_names[$choice_one]\" \n
			set ylabel \"@dom_cor_names[$choice_two]\" \n
			plot \"< paste $file_one $file_two\" using 1:2 title \"$title\" with lines";
# Algae
} elsif ($choice_one gt 29 && $choice_one le 39) {
	$choice_one = $choice_one - 30;
	$choice_two = $choice_two - 30;

	my $title = "@dom_alg_names[$choice_one] - @dom_alg_names[$choice_two]";
	my $output = "@dom_alg_names[$choice_one]_@dom_alg_names[$choice_two].png";
	my $file_one = "@dom_alg_files[$choice_one]";
	my $file_two = "@dom_alg_files[$choice_two]";

	print FILE "set terminal png \n
			set autoscale \n
			set title \"$title\"  \n
			set output \"$output\"  \n
			set xlabel \"@dom_alg_names[$choice_one]\" \n
			set ylabel \"@dom_alg_names[$choice_two]\" \n
			plot \"< paste $file_one $file_two\" using 1:2 title \"$title\" with lines";
# Barrier
} elsif ($choice_one gt 39 && $choice_one lt 49) {
	$choice_one = $choice_one - 40;
	$choice_two = $choice_two - 40;

	my $title = "@dom_bar_names[$choice_one] - @dom_bar_names[$choice_two]";
	my $output = "@dom_bar_names[$choice_one]_@dom_bar_names[$choice_two].png";
	my $file_one = "@dom_bar_files[$choice_one]";
	my $file_two = "@dom_bar_files[$choice_two]";

	print FILE "set terminal png \n
			set autoscale \n
			set title \"$title\"  \n
			set output \"$output\"  \n
			set xlabel \"@dom_bar_names[$choice_one]\" \n
			set ylabel \"@dom_bar_names[$choice_two]\" \n
			plot \"< paste $file_one $file_two\" using 1:2 title \"$title\" with lines";

}

# Close the file
close(FILE);

system("gnuplot $gnufile");
system("mv *.png /home/jon/Desktop/Phage2Shark/Outputs");

system("rm $gnufile");

}

}

###########################################################################################################################
# Multiple run files here

sub rangerun {
	my ($inargs) = @_;
	my @arrin = @{$inargs};

	my $title1 = @arrin[0];
	my $title2 = @arrin[1];
	my $title3 = @arrin[2];
	my $title4 = @arrin[3];
	my $title5 = @arrin[4];

	my @data_files = ("coral_percentage","coral_total","coral_average",
										"coral_delta","bact_pop","phage_pop","lys_pop",
										"bact_spec","phage_spec","lys_spec","fish_pop",
										"fish_delta","algae_perc","phage_lys_rat",
										"shark_evt","vmr","time");

	my @dom_cor_files = ("bact_pop_cor","phage_pop_cor","lys_pop_cor","bact_spec_cor",
											"phage_spec_cor","lys_spec_cor","phage_lys_rat_cor",
											"vmr_cor","area_cor","time");

	my @dom_alg_files = ("bact_pop_alg","phage_pop_alg","lys_pop_alg","bact_spec_alg",
	 										"phage_spec_alg","lys_spec_alg","phage_lys_rat_alg",
											"vmr_alg","area_alg","time");

	my @dom_bar_files = ("bact_pop_bar","phage_pop_bar","lys_pop_bar","bact_spec_bar",
											"phage_spec_bar","lys_spec_bar","phage_lys_rat_bar",
											"vmr_bar","area_bar","time");

my @title_names = ("Coral_Percentage","Coral_Total","Coral_Average",
									"Coral_Delta","Bacteria_Population","Phage_Population","Lysogen_Population",
									"Bacteria_Species","Phage_Species","Lysogen_Species","Fish_Population",
									"Fish_Delta","Algae_Percentage","Phage_Lysogen_Ratio",
									"Shark_Events","VMR","Time");

my @dom_cor_names = ("Bact Pop Coral","Phage Pop Coral","Lys Pop Coral","Bact. Spec. Coral",
										"Phage Spec. Coral","Lys Spec. Coral","Phage Lys Ratio Coral",
										"VMR Coral","Coral Area Percentage","time");

my @dom_alg_names = ("Bact Pop Algae","Phage Pop Algae","Lys Pop Algae","Bact. Spec. Algae",
										"Phage Spec. Algae","Lys Spec. Algae","Phage Lys Ratio Algae",
										"VMR Algae","Algae Area Percentage","time");

my @dom_bar_names = ("Bact Pop Barr","Phage Pop Barr","Lys Pop Barr","Bact. Spec. Barr",
										"Phage Spec. Barr","Lys Spec. Barr","Phage Lys Ratio Barr",
										"VMR Barr","Barr Area Percentage","time");

	my $dirgen = '/home/jon/Desktop/Phage2Shark/General';
	my $gnufile = "gnu.batch";
	chdir($dirgen);

	while (1) {
		print "Choose 2 sets of data to plot against each other. Enter nothing to exit. \n";
		print "Coral Percentage    -  1 | Coral Total          - 2 \n";
		print "Coral Average       -  3 | Coral Delta          - 4 \n";
		print "Bacteria Population -  5 | Phage Population     - 6 \n";
		print "Lysogen Population  -  7 | Bacteria Species     - 8 \n";
		print "Phage Species       -  9 | Lysogen Species      - 10\n";
		print "Fish Population     - 11 | Fish Delta           - 12 \n";
		print "Algae Percentage    - 13 | Phage-Lysogen Ratio  - 14 \n";
		print "Shark Events        - 15 | VMR                  - 16 \n";
		print "Time                - 17 | --------- \n";
		print "One domain per graph please. \n";
		print "Coral Domain      |      Algae Domain      |      Barrier Domain \n \n";
		print "                  |                        |                        \n";
		print "Bacteria pop - 21 |      Bacteria pop - 31 |      Bacteria pop - 41 \n";
		print "Phage pop    - 22 |      Phage pop    - 32 |      Phage pop    - 42 \n";
		print "Lys pop      - 23 |      Lys pop      - 33 |      Lys pop      - 43 \n";
		print "Bact spec    - 24 |      Bact spec    - 34 |      Bact spec    - 44 \n";
		print "Phage spec   - 25 |      Phage spec   - 35 |      Phage spec   - 45 \n";
		print "Lys spec     - 26 |      Lys spec     - 36 |      Lys spec     - 46 \n";
		print "Pha/lys rat  - 27 |      Pha/lys rat  - 37 |      Phag/lys rat - 47 \n";
		print "vmr          - 28 |      vmr          - 38 |      vmr          - 48 \n";
		print "Perc. cover  - 29 |      Perc. cover  - 39 |      Perc. cover  - 49 \n";
		print "Time         - 30 |      Time         - 40 |      Time         - 50 \n";

	print "\n First set of data (x-axis): \n";
	my $choice_one = <>;
	chomp $choice_one;
	return if ($choice_one eq "");
	$choice_one = $choice_one - 1;

	print "\n Second set of data (y-axis): \n";
	my $choice_two = <>;
	chomp $choice_two;
	$choice_two = $choice_two - 1;

	unless (open FILE, '>'.$gnufile){
		die "\n Failed to create batch file for plotting $curfile. \n";
	}


	# Global
	if ($choice_one lt 20){
		my $title = "@title_names[$choice_one] \& @title_names[$choice_two]";
		my $output = "@title_names[$choice_one]_@title_names[$choice_two].png";

		my $file_one = "@data_files[$choice_one]1.dat";
		my $file_two = "@data_files[$choice_two]1.dat";
		my $file_thr = "@data_files[$choice_one]2.dat";
		my $file_fou = "@data_files[$choice_two]2.dat";
		my $file_fiv = "@data_files[$choice_one]3.dat";
		my $file_six = "@data_files[$choice_two]3.dat";
		my $file_sev = "@data_files[$choice_one]4.dat";
		my $file_eig = "@data_files[$choice_two]4.dat";
		my $file_nin = "@data_files[$choice_one]5.dat";
		my $file_ten = "@data_files[$choice_two]5.dat";

		print FILE "set terminal png \n
				set autoscale \n
				set title \"$title\"  \n
				set output \"$output\"  \n
				set xlabel \"@title_names[$choice_one]\" \n
				set ylabel \"@title_names[$choice_two]\" \n
				plot \"< paste $file_one $file_two\" using 1:2 title \"$title1\" with lines, \\
				 \"< paste $file_thr $file_fou\" using 1:2 title \"$title2\" with lines, \\
	 			 \"< paste $file_fiv $file_six\" using 1:2 title \"$title3\" with lines, \\
				 \"< paste $file_sev $file_eig\" using 1:2 title \"$title4\" with lines, \\
				 \"< paste $file_nin $file_ten\" using 1:2 title \"$title5\" with lines";

	# Coral
	} elsif ($choice_one gt 19 && $choice_one le 29) {
		$choice_one = $choice_one - 20;
		$choice_two = $choice_two - 20;

		my $title = "@dom_cor_names[$choice_one] - @dom_cor_names[$choice_two]";
		my $output = "@dom_cor_names[$choice_one]_@dom_cor_names[$choice_two].png";


		my $file_one = "@dom_cor_files[$choice_one]1.dat";
		my $file_two = "@dom_cor_files[$choice_two]1.dat";
		my $file_thr = "@dom_cor_files[$choice_one]2.dat";
		my $file_fou = "@dom_cor_files[$choice_two]2.dat";
		my $file_fiv = "@dom_cor_files[$choice_one]3.dat";
		my $file_six = "@dom_cor_files[$choice_two]3.dat";
		my $file_sev = "@dom_cor_files[$choice_one]4.dat";
		my $file_eig = "@dom_cor_files[$choice_two]4.dat";
		my $file_nin = "@dom_cor_files[$choice_one]5.dat";
		my $file_ten = "@dom_cor_files[$choice_two]5.dat";

		print FILE "set terminal png \n
				set autoscale \n
				set title \"$title\"  \n
				set output \"$output\"  \n
				set xlabel \"@dom_cor_files[$choice_one]\" \n
				set ylabel \"@dom_cor_files[$choice_two]\" \n
				plot \"< paste $file_one $file_two\" using 1:2 title \"$title1\" with lines, \\
				 \"< paste $file_thr $file_fou\" using 1:2 title \"$title2\" with lines, \\
	 			 \"< paste $file_fiv $file_six\" using 1:2 title \"$title3\" with lines, \\
				 \"< paste $file_sev $file_eig\" using 1:2 title \"$title4\" with lines, \\
				 \"< paste $file_nin $file_ten\" using 1:2 title \"$title5\" with lines";

	# Algae
	} elsif ($choice_one gt 29 && $choice_one le 39) {
		$choice_one = $choice_one - 30;
		$choice_two = $choice_two - 30;

		my $title = "@dom_alg_names[$choice_one] - @dom_alg_names[$choice_two]";
		my $output = "@dom_alg_names[$choice_one]_@dom_alg_names[$choice_two].png";


		my $file_one = "@dom_alg_files[$choice_one]1.dat";
		my $file_two = "@dom_alg_files[$choice_two]1.dat";
		my $file_thr = "@dom_alg_files[$choice_one]2.dat";
		my $file_fou = "@dom_alg_files[$choice_two]2.dat";
		my $file_fiv = "@dom_alg_files[$choice_one]3.dat";
		my $file_six = "@dom_alg_files[$choice_two]3.dat";
		my $file_sev = "@dom_alg_files[$choice_one]4.dat";
		my $file_eig = "@dom_alg_files[$choice_two]4.dat";
		my $file_nin = "@dom_alg_files[$choice_one]5.dat";
		my $file_ten = "@dom_alg_files[$choice_two]5.dat";

		print FILE "set terminal png \n
				set autoscale \n
				set title \"$title\"  \n
				set output \"$output\"  \n
				set xlabel \"@dom_alg_files[$choice_one]\" \n
				set ylabel \"@dom_alg_files[$choice_two]\" \n
				plot \"< paste $file_one $file_two\" using 1:2 title \"$title1\" with lines, \\
				 \"< paste $file_thr $file_fou\" using 1:2 title \"$title2\" with lines, \\
	 			 \"< paste $file_fiv $file_six\" using 1:2 title \"$title3\" with lines, \\
				 \"< paste $file_sev $file_eig\" using 1:2 title \"$title4\" with lines, \\
				 \"< paste $file_nin $file_ten\" using 1:2 title \"$title5\" with lines";

	# Barrier
	} elsif ($choice_one gt 39 && $choice_one lt 49) {
		$choice_one = $choice_one - 40;
		$choice_two = $choice_two - 40;

		my $title = "@dom_bar_names[$choice_one] - @dom_bar_names[$choice_two]";
		my $output = "@dom_bar_names[$choice_one]_@dom_bar_names[$choice_two].png";


		my $file_one = "@dom_bar_files[$choice_one]1.dat";
		my $file_two = "@dom_bar_files[$choice_two]1.dat";
		my $file_thr = "@dom_bar_files[$choice_one]2.dat";
		my $file_fou = "@dom_bar_files[$choice_two]2.dat";
		my $file_fiv = "@dom_bar_files[$choice_one]3.dat";
		my $file_six = "@dom_bar_files[$choice_two]3.dat";
		my $file_sev = "@dom_bar_files[$choice_one]4.dat";
		my $file_eig = "@dom_bar_files[$choice_two]4.dat";
		my $file_nin = "@dom_bar_files[$choice_one]5.dat";
		my $file_ten = "@dom_bar_files[$choice_two]5.dat";

		print FILE "set terminal png \n
				set autoscale \n
				set title \"$title\"  \n
				set output \"$output\"  \n
				set xlabel \"@dom_bar_files[$choice_one]\" \n
				set ylabel \"@dom_bar_files[$choice_two]\" \n
				plot \"< paste $file_one $file_two\" using 1:2 title \"$title1\" with lines, \\
				 \"< paste $file_thr $file_fou\" using 1:2 title \"$title2\" with lines, \\
	 			 \"< paste $file_fiv $file_six\" using 1:2 title \"$title3\" with lines, \\
				 \"< paste $file_sev $file_eig\" using 1:2 title \"$title4\" with lines, \\
				 \"< paste $file_nin $file_ten\" using 1:2 title \"$title5\" with lines";

	}

 close(FILE);

	system("gnuplot $gnufile");
	system("mv \"$output\" /home/jon/Desktop/Phage2Shark/Outputs");

	system("rm $gnufile");

	}


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
my $batchout = "/home/jon/Desktop/Phage2Shark/Outputs/gnucur.batch";

chdir($dirgen);

print "Graphing $curfile";

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

system("rm gnucur.batch");

}

#####################################################################################################################################################
# Domain Plots
#####################################################################################################################################################

sub domrun {

my ($inargs) = @_;
my @arrin = @{$inargs};

my $curfile1 = @arrin[0];
my $curfile2 = @arrin[1];
my $curfile3 = @arrin[2];
my $curtitle = @arrin[3];
my $curname = @arrin[4];
my $tflag = @arrin[5];

my $dirgen = '/home/jon/Desktop/Phage2Shark/General';

my $gnufile = "gnu.batch";

chdir($dirgen);

unless (open FILE, '>'.$gnufile){
	die "\n Failed to create batch file for plotting $curfile. \n";
}

print "Plotting $curname \n";

unless (open FILE, '>'.$gnufile){
	die "\n Failed to create batch file for plotting $curname. \n";
}

if ($tflag eq 1) {
# Write chenges to file.

print FILE "set terminal png \n
		set datafile separator whitespace \n
		set autoscale \n
		set title \"$curtitle\" \n
		set output \"$curname\" \n
		set key autotitle columnhead \n
		plot \"$curfile1\" with lines, \"$curfile2\" with lines, \"$curfile3\" with lines";

} elsif	($tflag eq 2){

print FILE "set terminal png \n
		set datafile separator whitespace \n
		set autoscale \n
		set title \"$curtitle\"  \n
		set output \"$curname\"  \n
		set key autotitle columnhead \n
		plot \"$curfile1\" using 1:2 with lines, \"$curfile1\" using 1:3 with lines, \"$curfile1\" using 1:4 with lines, \"$curfile2\" using 1:2 with lines, \"$curfile2\" using 1:3 with lines, \"$curfile2\" using 1:4 with lines, \"$curfile3\" using 1:2 with lines,	\"$curfile3\" using 1:3 with lines, \"$curfile3\" using 1:4 with lines";

}

system("gnuplot $gnufile");
system("mv $curname /home/jon/Desktop/Phage2Shark/Outputs");

# Close the files
close(FILE);

system("rm $gnufile");


}
