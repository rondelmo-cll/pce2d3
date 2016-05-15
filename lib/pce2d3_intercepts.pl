
:- discontiguous d3intercept/2, d3intercept/3, d3intercept/4, d3intercept/5, d3intercept/6, d3intercept/7, d3intercept/8, d3intercept/9.

% ----------------------------------------------------------------------
% new
% ----------------------------------------------------------------------

% syntax: new(Reference, TermDescription)
% TermDescription = Functor(InitArg,...)
% where Functor is the class
%       InitArg =
%         | atom -> name attribute
%         | int  -> ???
%         | Class(InitArg) -> instance of class Class
%         | new(Class) -> instance of class Class
%         | new(Reference, TermDescription) -> recursive
%         | prolog(Term) -> unmodified prolog term

% so we could have something like
% new(R1,        meal(   new(    R2,  drink(  Coke))))
% new reference  termdescription
%                functor initarg                     
%                        new     ref  termdescription
%                                     functor initarg
%                                             atom

d3intercept(new,A,TermDescription) :-
	at_less(A,Ab),
	termdescription(Ab,TermDescription),
	!.

termdescription(This, A) :-
	atom(A), A = Functor, Args=[],
	writef("// WARNING: constructor for %w has functor %w with no arguments %w; this shouldn't happen.\n",[This, Functor, Args]).

termdescription(This,A) :-
	compound(A), A =.. [Functor | InitArgs],
	writef("// %w = new %w ( %w )\n",[This, Functor, InitArgs]),
	construct(This, Functor, InitArgs).

% ----------------------------------------------------------------------
% new > constructors
% ----------------------------------------------------------------------

% each class constructs its instances in a different way.      PICTURE --> svg
construct(This, picture, InitArgs) :-
	writef("symbols.%w = pg.append(\"svg\"); // initargs=%w\nvar svg = symbols.%w;\n", [This, InitArgs, This]),
	initargs(This, InitArgs).
% each class constructs its instances in a different way.      BOX --> rect
construct(This, box, InitArgs) :-
	[Width,Height] = InitArgs,
	writef("symbols.%w = svg.append(\"rect\") // initargs=%w\n", [This, InitArgs]),
	writef("    .attr(\"width\", %w)\n",[Width]),
	writef("    .attr(\"height\",%w)\n",[Height]),
	writef("    ;\n",[]).
% each class constructs its instances in a different way.      CIRCLE --> circle
construct(This, circle, InitArgs) :-
	[Radius] = InitArgs,
	writef("symbols.%w = svg.append(\"circle\") // initargs=%w\n", [This, InitArgs]),
	writef("    .attr(\"r\", %w)\n",[Radius]),
	writef("    ;\n",[]).

% failure case -- there's a class we don't know about
construct(This, Otherwise, InitArgs) :-
	writef("// ERROR: don't know how to construct class %w (%w)", [Otherwise,InitArgs]).

% ----------------------------------------------------------------------
% new > constructors > initargs
% ----------------------------------------------------------------------

initargs(This, []).
initargs(This, [H|T]) :-
	initarg(This, H),
	initargs(This, T).

initarg(This, H) :- atom(H),
					writef("symbols.%w.attr(\"name\",\"%w\");\n", [This, H]).
initarg(This, H) :- number(H),
					writef("// constructor(%w) UNHANDLED number argument %w\n", [This, H]).
initarg(This, H) :- compound(H),
					H =.. [ HH | HT ],
					writef("// constructor(%w) compound initarg could be one of four things... %w %w/%w\n", [This, H, HH, HT]),
					compoundInitArg(This, HH, HT).
compoundInitArg(This, new, [Class]) :-
	writef("// ERROR: UNHANDLED constructor(%w) involves new call to atomic class (%w)\n", [This, Class]).
compoundInitArg(This, new, RT) :-
	RT = [Reference, TermDescription | _],
	writef("// constructor(%w) compound initarg is a recursive new call (%w, %w)\n", [This, Reference, TermDescription]),
	d3intercept(new,Reference,TermDescription).
compoundInitArg(This, prolog, Term) :-
	writef("// ERROR: UNHANDLED constructor(%w) prolog term (%w)\n", [This, Term]).
compoundInitArg(This, Class, InitArgs) :-
	writef("// WARNING: constructor(%w) might be defining a class %w(%w). does this often occur?\n", [This, Class, InitArgs]),
	construct(This, Class, InitArgs).





% ----------------------------------------------------------------------
% send
% ----------------------------------------------------------------------

d3intercept(send,A,Selector,X) :-
	SelectorX =.. [Selector | X],
	d3intercept(send,A,SelectorX).
d3intercept(send,A,SelectorX) :-
	SelectorX =.. [Selector | Args],
	at_less(A,This),
	d3send(This, Selector, Args),
	!.

% ----------------------------------------------------------------------
% send display
% ----------------------------------------------------------------------

d3intercept(send,A,display,X) :- d3intercept(send,A,display(X)).

:- discontiguous d3send/3.

% we conflate the ideas of sending some new thing to a frame display, and constructing that new thing.
% todo: separate these ideas back out by:
%    new should create an svg element in the symbol table
%    send should append it to the pg group or current svg variable
d3send(This, display, []). % null case

% ----------------------------------------------------------------------
% send display new() point()
% ----------------------------------------------------------------------

% we distinguish this case because we need to know a little bit more about the new object to associate its point.
d3send(This, display, [NewArg, PointArg]) :-
	isNewArg(NewArg), isPointArg(PointArg,PointArgs),
	writef("// d3send: %w.send(display) commoncase %w %w\n",[This,NewArg,PointArg]),
	d3sendarg(This, display, NewArg),
	newRef(NewArg, Symbol, Class),
	setPoint(Symbol, Class, PointArgs),
	!.

% ----------------------------------------------------------------------
% send display new() point() helper functions
% ----------------------------------------------------------------------

isNewArg(X)        :- X =.. [new   | _]. % new(_)
isPointArg(X,Args) :- X =.. [point | Args]. % point(_)

newRef(NewArg, Symbol, Class) :-
%	writef("// will return symbol and class for newArg %w\n",[NewArg]),
	NewArg =.. [New, Ref, TermDescription | _],
%	writef("// extracted TermDescription = %w\n",[TermDescription]),
	TermDescription =.. [Class | InitArgs],
	at_less(Ref,Symbol).
%	writef(" // Symbol = %w, Class = %w\n",[Symbol, Class]).

setPoint(This, box, [X,Y]) :-
	writef("symbols.%w.attr(\"x\",%w);\n", [This,X]),
	writef("symbols.%w.attr(\"y\",%w);\n", [This,Y]).
setPoint(This, circle, [X,Y]) :-
	writef("symbols.%w.attr(\"cx\",%w);\n", [This,X]),
	writef("symbols.%w.attr(\"cy\",%w);\n", [This,Y]).
setPoint(This, Class, PointArgs) :-
	writef("// WARNING: don't know how to locate symbols.%w (class=%w) at %w\n",[This,Class,PointArgs]).

% ----------------------------------------------------------------------
% send display something-else
% ----------------------------------------------------------------------

% handle degenerate display cases where we don't have a new and a point together
d3send(This, display, [Arg | Args]) :-
	writef("// d3send: %w.send(display) general case %w\n",[This,Arg,Args]),
	d3sendarg(This, display, Arg),
	d3send(This, display, Args),
	!.

d3sendarg(This, display, ArgTerm) :-
	ArgTerm =.. [Functor | Args],
	d3sendarg_functor(This, Functor, Args).

d3sendarg_functor(This, new, X) :- compoundInitArg(This, new, X).

d3sendarg_functor(This, point, X) :-
	writef("// WARNING: isolated %w.send_point (%w), not sure who the object is\n", [This, X]).

% ----------------------------------------------------------------------
% send size
% ----------------------------------------------------------------------

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


% ----------------------------------------------------------------------
% send something-else
% ----------------------------------------------------------------------

d3send(This, Selector, Args) :-
	writef("// %w.send(%w) WARNING: unknown selector (%w)\n",[This,Selector,Args]).


% ----------------------------------------------------------------------
% below this line we need to convert to the above general style
% ----------------------------------------------------------------------




% 
% %  fill
% d3intercept(send,A,fill_pattern,colour(C)) :-
% 	at_less(A,Ab),
% 	term_string(A,Aa), sub_string(Aa,1,_,0,Ab),
% 	writef("symbols.%w\n", [Ab]),
% 	writef("    .style(\"fill\",\"%w\");\n", [C]),
% 	!.
% 
% %  text
% d3intercept(send,A,display,new(B,text(T)),point(X,Y)) :- d3intercept(send,A,display(new(B,text(T)),point(X,Y))).
% d3intercept(send,A,display(new(B,text(T)),point(X,Y))) :-
% 	at_less(A,Ab), at_less(B,Bb),
% 	writef("symbols.%w = symbols.%w.append(\"text\")\n", [Bb, Ab]),
% 	writef("    .attr(\"x\",%w)\n", [X]),
% 	writef("    .attr(\"y\",%w)\n", [Y]),
% 	writef("    .text(\"%w\");\n", [T]),
% 	!.
% 
