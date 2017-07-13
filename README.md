# pce2d3
XPCE to D3 translation layer for swi-prolog

Normally, swi-prolog's visualization output writes to an X server using the native XPCE library.

But that's so 1990s. Today we have this thing called the web.

So I wrote a PCE to Javascript D3 translation layer.

That layer intercepts calls to PCE and translates them to the D3 equivalent.

It kind of works. You will need to take it the rest of the way.

If you're doing a lot on the web you should probably know about pengines and SWISH.
