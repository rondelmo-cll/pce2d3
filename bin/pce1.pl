
:- module(pce1, 
		  [ mydraw/1 ]).
:- use_module(library(pce2d3)).

mydraw(JS) :-
	with_output_to(string(JS), mydraw).

mydraw :-
	setupd3,
	new(Pic,picture(myfirstimage)),
	writef("\n// Pic has been bound to %w\n",[Pic]),
	send(Pic, size,    size(400,400)),
	send(Pic, open,    point(200,200)),
	send(Pic, display, new(@bx,box(100,100)),      point(25,25)),
	send(Pic, display, new(@ci,circle(50)),        point(50,50)),
	send(Pic, display, new(@tx,text('Wassup!!!')), point(25,150)),
	send(@ci, fill_pattern, colour(orange)),
	free(Pic), free(@bx), free(@ci), free(@tx),
	!.


ask_name(Name) :-
        new(D, dialog('Register')),
        send(D, append(new(NameItem, text_item(name)))),
        send(D, append(button(ok, message(D, return,
                                          NameItem?selection)))),
        send(D, append(button(cancel, message(D, return, @nil)))),
        send(D, default_button(ok)),
        get(D, confirm, Rval),
        free(D),
        Rval \== @nil,
        Name = Rval.

