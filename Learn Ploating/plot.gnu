set xzeroaxis
set style data lines
set samples 500
plot 'test.dat' u 1:2 smooth acsplines title 'acsplines', \
     '' u 1:2 smooth csplines title 'csplines', \
     '' u 1:2 smooth mcsplines lw 2 title 'mcsplines',\
     '' u 1:2 w p pt 7 title 'data points'