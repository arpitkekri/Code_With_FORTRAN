# Load this script from gnuplot using command --> load 'plot.gnu' 
# It will create both graphs in saparate windows
set term qt 0
p 'output1.dat' u 1:2 w l title "E(v)"
set term qt 1
p 'output1.dat' u 1:3 w l title "delta E(v)"