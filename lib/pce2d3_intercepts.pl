
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

% ---------------------------------------------------------------------- PICTURE --> svg
construct(This, picture, InitArgs) :-
	% recordz(This,d3{type:svg, symbol:This}),
	writef("\nsymbols.%w = pg.append(\"svg\"); // initargs=%w\nvar svg = symbols.%w;\n", [This, InitArgs, This]),
	d3send(This, size, [size(1234,789)]), % hardcode some defaults
	initargs(This, InitArgs). % initargs() gives full-blown treatment.

% but sometimes we don't need full-blown treatment -- usually the term description is simple.
% ---------------------------------------------------------------------- BOX --> rect
construct(This, box, InitArgs) :-
	[Width,Height] = InitArgs,
	% recordz(This,d3{type:rect, symbol:This, width:Width, height:Height}),
	writef("\nsymbols.%w = svg.append(\"rect\") // initargs=%w\n", [This, InitArgs]),
	writef("    .attr(\"width\", %w)\n",[Width]),
	writef("    .attr(\"height\",%w)\n",[Height]),
	writef("    ;\n",[]).

% ---------------------------------------------------------------------- CIRCLE --> circle
construct(This, circle, InitArgs) :-
	[Radius] = InitArgs,
	% recordz(This,d3{type:circle, symbol:This, radius:Radius}),
	writef("\nsymbols.%w = svg.append(\"circle\") // initargs=%w\n", [This, InitArgs]),
	writef("    .attr(\"r\", %w)\n",[Radius]),
	writef("    ;\n",[]).

% ---------------------------------------------------------------------- TEXT --> text
construct(This, text, [Text]) :-
	% recordz(This,d3{type:text, symbol:This, cdata:CDATA}),
	(sub_string(Text,_,1,_,"\"")
	 -> CDATA = "XXX - FIXME - TEXT CONTAINED DOUBLEQUOTES" ; true ),
	CDATA=Text,
	writef("\nsymbols.%w = svg.append(\"text\") // initargs=%w\n", [This, CDATA]),
	writef("    .text(\"%w\")\n",[CDATA]),
	writef("    ;\n",[]).

	
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

% failure case -- there's a class we don't know about
construct(This, Otherwise, InitArgs) :-
	writef("// ERROR: (%w) don't know how to construct class %w (%w)\n", [This,Otherwise,InitArgs]).

% ----------------------------------------------------------------------
% new > constructors > initargs
% ----------------------------------------------------------------------

initargs(_,    []).
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

:- discontiguous d3send/3.

% we conflate the ideas of sending some new thing to a frame display, and constructing that new thing.
% todo: separate these ideas back out by:
%    new should create an svg element in the symbol table
%    send should append it to the pg group or current svg variable
d3send(_, display, []). % null case

% ---------------------------------------------------------------------- send display new() point()

% we distinguish this case because we need to know a little bit more about the new object to associate its point.
d3send(This, display, [NewArg, PointArg]) :-
	isNewArg(NewArg), isPointArg(PointArg,PointArgs),
	writef("// d3send: %w.send(display) commoncase %w %w\n",[This,NewArg,PointArg]),
	d3sendarg(This, display, NewArg),
	newRef(NewArg, Symbol, Class),
	setPoint(Symbol, Class, PointArgs),
	!.

% ---------------------------------------------------------------------- send display new() point() helpers

isNewArg(X)        :- X =.. [new   | _]. % new(_)
isPointArg(X,Args) :- X =.. [point | Args]. % point(_)

newRef(NewArg, Symbol, Class) :-
%	writef("// will return symbol and class for newArg %w\n",[NewArg]),
	NewArg =.. [_, Ref, TermDescription | _],
%	writef("// extracted TermDescription = %w\n",[TermDescription]),
	TermDescription =.. [Class | _],
	at_less(Ref,Symbol).
%	writef(" // Symbol = %w, Class = %w\n",[Symbol, Class]).

setPoint(This, XYobj, [X,Y]) :-
	( XYobj = box ; XYobj = text ),
	writef("symbols.%w.attr(\"x\",%w);\n", [This,X]),
	writef("symbols.%w.attr(\"y\",%w);\n", [This,Y]).
setPoint(This, circle, [X,Y]) :-
	writef("symbols.%w.attr(\"cx\",%w);\n", [This,X]),
	writef("symbols.%w.attr(\"cy\",%w);\n", [This,Y]).
setPoint(This, Class, PointArgs) :-
	writef("// WARNING: don't know how to locate symbols.%w (class=%w) at %w\n",[This,Class,PointArgs]).

% ---------------------------------------------------------------------- send display something-else

% handle degenerate display cases where we don't have a new and a point together.

% we might be able to ascertain the object by pulling it from the record database.

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
	writef("// WARNING: isolated %w.send_point (%w), not sure who the object is\n", [This, X]),
	!.

% ---------------------------------------------------------------------- send size

d3send(This, size, [size(W,H)]) :-
	writef("symbols.%w.attr(\"width\",%w);\n", [This,W]),
	writef("symbols.%w.attr(\"height\",%w);\n", [This,H]),
	!.

% ---------------------------------------------------------------------- send open

%  location of SVG. maybe this is what should append the svg into the body/pg
d3send(This, open, [point(X,Y)]) :-
	writef("// open() on the X framebuffer is an absolute coordinate relative to the displayport's 0,0, but that doesn't mean anything to the SVG renderer right now. maybe in the future it will, if we want to pervert the HTML canvas displayport into a framebuffer addressable from 0,0. \n"),
	writef("// symbols.%w.x = %w;\n", [This, X]),
	writef("// symbols.%w.y = %w;\n", [This, Y]),
	!.

% ---------------------------------------------------------------------- send fill_pattern

d3send(This, fill_pattern, [colour(C)]) :-
	writef("symbols.%w.style(\"fill\",\"%w\");\n", [This,C]), !.

% ---------------------------------------------------------------------- send font

d3send(This, font, [Arg]) :-
	Arg =.. [ _, Family, Weight, Size ],
	writef("symbols.%w.style(\"font\",\"%w %w %w\");\n", [This, Weight, Size, Family]),
	!.

% ---------------------------------------------------------------------- send something else

d3send(This, Selector, Args) :-
	writef("// %w.send(%w) WARNING: unknown selector (%w)\n",[This,Selector,Args]), !.

% ----------------------------------------------------------------------
% below this line we need to convert to the above general style
% ----------------------------------------------------------------------


