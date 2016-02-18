set title "Re-Analizing vs. restore"
set xlabel "Benchmark number"
set ylabel "Time in ms"
set key left  nobox  
set term post portrait  color  "Times-Roman"  14  
plot  'restore_vs_analize1.dat' title "dump_and_restore" with boxerrorbars , 'restore_vs_analize2.dat' title "module&analize" with impulses