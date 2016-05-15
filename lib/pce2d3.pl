
% we wrap the four major documented interfaces: new, send, get, and free.
% if you are running this in full parallel mode,
% you'll want to also reexport everything the original pce library exports; see core.
:- module(pce2d3,[
			  setupd3/0,
			  new/2,
			  free/1,
			  send/2,
			  send/3,
			  send/4,
			  send/5,
			  send/6,
			  send/7,
			  send/8,
			  get/3,
			  get/4,
			  get/5,
			  get/6,
			  get/7,
			  get/8,
			  d3intercept/2,
			  d3intercept/3,
			  d3intercept/4,
			  d3intercept/5,
			  d3intercept/6,
			  d3intercept/7,
			  d3intercept/8,
			  d3intercept/9
		  ]).
:- writeln("init: loading module pce2d3").

%
% order matters:
%

% show only unhandled d3intercepts
:- [pce2d3_intercepts, pce2d3_core].

% % show all d3intercepts
% :- [pce2d3_core, pce2d3_intercepts].




