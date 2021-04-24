set xrange[-2:3.1]
do for [t=0:50] {
    set term qt t
    set term png
    set output "pic".t.".png"
    set title "time = ".t*10
    x = t*100
    p "psi".x.".txt" u 1:2 w l title "DFT psi", "potential.txt" u 1:2 w l title 'potential'
}