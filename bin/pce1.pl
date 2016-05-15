
:- module(pce1, 
		  [ mydraw/1 ]).
:- use_module(library(pce2d3)).

mydraw(JS) :-
	with_output_to(string(JS), mydraw).

mydraw :-
	setupd3,
	new(@pic,picture),
	send(@pic, size,    size(400,400)),
	send(@pic, open,    point(200,200)),
	send(@pic, display, new(@bx,box(100,100)),      point(25,25)),
	send(@pic, display, new(@ci,circle(50)),        point(50,50)),
	send(@pic, display, new(@tx,text('Wassup!!!')), point(25,150)),
	sleep(1),
	send(@ci, fill_pattern, colour(orange)),
	sleep(1),
	free(@pic), free(@bx), free(@ci), free(@tx),
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

showpicture :-
    new(@frame, picture('SPARSAR Poetry Style Analyzer')),
    send(@frame, display, new(T, text(Text)), point(5, 0)),    
    send(T, font, font(times, bold, 24)),
	send(@frame, open),
	send(@frame, display,new(Fr, text('Poetic Rhetoric Devices')), point(5, 50)),
		   send(@frame, display, new(Bi, box(L,R1)),point(150,70)),
		   send(Bi, fill_pattern, colour(red)),
		   send(Fr, font, font(times, bold, 18)),
	send(@frame, display,new(Fr1, text('Metrical Length')), point(5, 150)),
           send(@frame, display, new(Bix, box(H1, G1)),point(150,170)),
		   send(Bix, fill_pattern, colour(green)),
		   send(Fr1, font, font(times, bold, 18)),
	send(@frame, display,new(Fr2, text('Semantic Density')), point(5, 250)),
           send(@frame, display, new(Sdi, box(SDI,G1)),point(150,270)),
		   send(Sdi, fill_pattern, colour(blue)),
		   send(Fr2, font, font(times, bold, 18)),
	send(@frame, display,new(Fr3, text('Phonetic Density Distribution')), point(5, 350)),
           send(@frame, display, new(Sd, box(SD,R1)),point(Pos,370)),
		   send(Sd, fill_pattern, colour(black)),
		   send(Fr3, font, font(times, bold, 18)),
	send(@frame, display,new(Fr4, text('Deep Conceptual Index')), point(5, 450)),
           send(@frame, display, new(Ds, box(DS,R1)),point(200,470)),
		   send(Ds, fill_pattern, colour(brown)),
		   send(Fr4, font, font(times, bold, 18)),
	send(@frame, display,new(Fr5, text('Rhyming Scheme Comparison')), point(5, 570)),
           send(@frame, display, new(Dsi, box(L,L11)),point(200,590)),
		   send(Dsi, fill_pattern, colour(purple)),
		   send(Fr5, font, font(times, bold, 18)),
		   N1 is N + 20.
