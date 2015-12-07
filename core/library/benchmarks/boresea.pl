/*
1.1. Program to test calls  (boresea).

This is the one you always dreamed to test! Like all benchmarks,
it uses a loop calling the actual benchmark program. The benchmark
program consists of a sequence of 200 predicates having no arguments,
no choice points, NOTHING. 200 is chosen to have sufficient accuracy
in measuring the execution time.

The results show the effect of pure calls, and the Klips performance
can be called the peak performance of the prolog system. Note
that the peak performance has very little significance to classify
the overall performance of a Prolog system.

---------------- cut here - beginning of program listing ----------
*/

:- module(boresea,[boresea/1], []).

:- use_module(benchmark_utilities).

/* This program is called with the query "?-boresea(X)."         */
/* X is the number of loop iterations executed. It should be big */
/* enough to give significant results.                           */
/* suggested value for X: 100 for interpreted code*/
/*                       1000 for compiled code   */
/* average values for C-prolog interpreter:       */
/*       X=1000, Tloop=27.1 T.comp=1.0 Tnet=26.1 Klips=7.7 */

boresea(N):- common_loop(3, N, lips1, 200, boresea).

lips1 :- lips2.
lips2 :- lips3.
lips3 :- lips4.
lips4 :- lips5.
lips5 :- lips6.
lips6 :- lips7.
lips7 :- lips8.
lips8 :- lips9.
lips9 :- lips10.
lips10 :- lips11.
lips11 :- lips12.
lips12 :- lips13.
lips13 :- lips14.
lips14 :- lips15.
lips15 :- lips16.
lips16 :- lips17.
lips17 :- lips18.
lips18 :- lips19.
lips19 :- lips20.
lips20 :- lips21.
lips21 :- lips22.
lips22 :- lips23.
lips23 :- lips24.
lips24 :- lips25.
lips25 :- lips26.
lips26 :- lips27.
lips27 :- lips28.
lips28 :- lips29.
lips29 :- lips30.
lips30 :- lips31.
lips31 :- lips32.
lips32 :- lips33.
lips33 :- lips34.
lips34 :- lips35.
lips35 :- lips36.
lips36 :- lips37.
lips37 :- lips38.
lips38 :- lips39.
lips39 :- lips40.
lips40 :- lips41.
lips41 :- lips42.
lips42 :- lips43.
lips43 :- lips44.
lips44 :- lips45.
lips45 :- lips46.
lips46 :- lips47.
lips47 :- lips48.
lips48 :- lips49.
lips49 :- lips50.
lips50 :- lips51.
lips51 :- lips52.
lips52 :- lips53.
lips53 :- lips54.
lips54 :- lips55.
lips55 :- lips56.
lips56 :- lips57.
lips57 :- lips58.
lips58 :- lips59.
lips59 :- lips60.
lips60 :- lips61.
lips61 :- lips62.
lips62 :- lips63.
lips63 :- lips64.
lips64 :- lips65.
lips65 :- lips66.
lips66 :- lips67.
lips67 :- lips68.
lips68 :- lips69.
lips69 :- lips70.
lips70 :- lips71.
lips71 :- lips72.
lips72 :- lips73.
lips73 :- lips74.
lips74 :- lips75.
lips75 :- lips76.
lips76 :- lips77.
lips77 :- lips78.
lips78 :- lips79.
lips79 :- lips80.
lips80 :- lips81.
lips81 :- lips82.
lips82 :- lips83.
lips83 :- lips84.
lips84 :- lips85.
lips85 :- lips86.
lips86 :- lips87.
lips87 :- lips88.
lips88 :- lips89.
lips89 :- lips90.
lips90 :- lips91.
lips91 :- lips92.
lips92 :- lips93.
lips93 :- lips94.
lips94 :- lips95.
lips95 :- lips96.
lips96 :- lips97.
lips97 :- lips98.
lips98 :- lips99.
lips99 :- lips100.
lips100:- lips101.
lips101 :- lips102.
lips102 :- lips103.
lips103 :- lips104.
lips104 :- lips105.
lips105 :- lips106.
lips106 :- lips107.
lips107 :- lips108.
lips108 :- lips109.
lips109 :- lips110.
lips110 :- lips111.
lips111 :- lips112.
lips112 :- lips113.
lips113 :- lips114.
lips114 :- lips115.
lips115 :- lips116.
lips116 :- lips117.
lips117 :- lips118.
lips118 :- lips119.
lips119 :- lips120.
lips120 :- lips121.
lips121 :- lips122.
lips122 :- lips123.
lips123 :- lips124.
lips124 :- lips125.
lips125 :- lips126.
lips126 :- lips127.
lips127 :- lips128.
lips128 :- lips129.
lips129 :- lips130.
lips130 :- lips131.
lips131 :- lips132.
lips132 :- lips133.
lips133 :- lips134.
lips134 :- lips135.
lips135 :- lips136.
lips136 :- lips137.
lips137 :- lips138.
lips138 :- lips139.
lips139 :- lips140.
lips140 :- lips141.
lips141 :- lips142.
lips142 :- lips143.
lips143 :- lips144.
lips144 :- lips145.
lips145 :- lips146.
lips146 :- lips147.
lips147 :- lips148.
lips148 :- lips149.
lips149 :- lips150.
lips150 :- lips151.
lips151 :- lips152.
lips152 :- lips153.
lips153 :- lips154.
lips154 :- lips155.
lips155 :- lips156.
lips156 :- lips157.
lips157 :- lips158.
lips158 :- lips159.
lips159 :- lips160.
lips160 :- lips161.
lips161 :- lips162.
lips162 :- lips163.
lips163 :- lips164.
lips164 :- lips165.
lips165 :- lips166.
lips166 :- lips167.
lips167 :- lips168.
lips168 :- lips169.
lips169 :- lips170.
lips170 :- lips171.
lips171 :- lips172.
lips172 :- lips173.
lips173 :- lips174.
lips174 :- lips175.
lips175 :- lips176.
lips176 :- lips177.
lips177 :- lips178.
lips178 :- lips179.
lips179 :- lips180.
lips180 :- lips181.
lips181 :- lips182.
lips182 :- lips183.
lips183 :- lips184.
lips184 :- lips185.
lips185 :- lips186.
lips186 :- lips187.
lips187 :- lips188.
lips188 :- lips189.
lips189 :- lips190.
lips190 :- lips191.
lips191 :- lips192.
lips192 :- lips193.
lips193 :- lips194.
lips194 :- lips195.
lips195 :- lips196.
lips196 :- lips197.
lips197 :- lips198.
lips198 :- lips199.
lips199 :- lips200.
lips200.

/*--------------------cut here - end of program listing------------*/
