
d3intercept(new,A,picture) :-
	term_string(A,Aa), sub_string(Aa,1,_,0,Ab),
	writef("symbols.%w = pg.append(\"svg\");\n", [Ab]),
	!.


at_less(T,Out) :-
	term_string(T,S),
	(   sub_string(S,0,1,_,"@")
	 -> sub_string(S,1,_,0,Out )
	 ;  Out = S).

%% when running with use_module(library(pce)),
%%     pce automatically aliases size,size() to size(size()).
%% when running without,
%%     we have to manually alias to canonical form.
%  size of SVG
d3intercept(send,A,size,size(W,H)) :- d3intercept(send,A,size(size(W,H))).
d3intercept(send,A,size(size(W,H))) :-
	at_less(A,Ab),
	writef("symbols.%w.attr(\"width\",%w);\n", [Ab,W]),
	writef("symbols.%w.attr(\"height\",%w);\n", [Ab,H]),
	!.

%  location of SVG
d3intercept(send,A,open,point(X,Y)) :- d3intercept(send,A,open(point(X,Y))).
d3intercept(send,A,open(point(X,Y))) :-
	at_less(A,Ab),
	writef("// open() on the X framebuffer is an absolute coordinate relative to the displayport's 0,0, but that doesn't mean anything to the SVG renderer right now. maybe in the future it will, if we want to pervert the HTML canvas displayport into a framebuffer addressable from 0,0. \n"),
	writef("// symbols.%w.x = %w;\n", [Ab, X]),
	writef("// symbols.%w.y = %w;\n", [Ab, Y]),
	!.

%  rectangle
d3intercept(send,A,display,new(B,box(W,H)),point(X,Y)) :- d3intercept(send,A,display(new(B,box(W,H)),point(X,Y))).
d3intercept(send,A,display(new(B,box(W,H)),point(X,Y))) :-
	at_less(A,Ab), at_less(B,Bb),
	writef("symbols.%w = symbols.%w.append(\"rect\")\n", [Bb, Ab]),
	writef("    .attr(\"x\",%w)\n", [X]),
	writef("    .attr(\"y\",%w)\n", [Y]),
	writef("    .attr(\"width\",%w)\n", [W]),
	writef("    .attr(\"height\",%w);\n", [H]),
	!.

%  circle
d3intercept(send,A,display,new(B,circle(R)),point(X,Y)) :- d3intercept(send,A,display(new(B,circle(R)),point(X,Y))).
d3intercept(send,A,display(new(B,circle(R)),point(X,Y))) :-
	at_less(A,Ab), at_less(B,Bb),
	writef("symbols.%w = symbols.%w.append(\"circle\")\n", [Bb, Ab]),
	writef("    .attr(\"cx\",%w)\n", [X]),
	writef("    .attr(\"cy\",%w)\n", [Y]),
	writef("    .attr(\"r\",%w);\n", [R]),
	!.

%  fill
d3intercept(send,A,fill_pattern,colour(C)) :-
	at_less(A,Ab),
	term_string(A,Aa), sub_string(Aa,1,_,0,Ab),
	writef("symbols.%w\n", [Ab]),
	writef("    .style(\"fill\",\"%w\");\n", [C]),
	!.

%  text
d3intercept(send,A,display,new(B,text(T)),point(X,Y)) :- d3intercept(send,A,display(new(B,text(T)),point(X,Y))).
d3intercept(send,A,display(new(B,text(T)),point(X,Y))) :-
	at_less(A,Ab), at_less(B,Bb),
	writef("symbols.%w = symbols.%w.append(\"text\")\n", [Bb, Ab]),
	writef("    .attr(\"x\",%w)\n", [X]),
	writef("    .attr(\"y\",%w)\n", [Y]),
	writef("    .text(\"%w\");\n", [T]),
	!.

