#Flow - composable stories for racket/slideshow

This package aims to add a temporal dimension the concept of a pict

The usual way for making a slideshow with racket/slideshow is to define the static structures in terms of picts, which are highly composable picture and text units, and then define how they appear on and across slides in a presenation file

The slideshow functions, "slide" and "with-steps", are fairly limited in terms of timing the evolution of picts, as opposed to the highly expressive pict objects.  

Here we address this by elevating picts to "flows", which aim to be as composable as picts, yet have a time dimension

A flow can be thought of as a mapping from symbol to pict, and a flow-slide can be thought of as a composition of flows and a list of "frames", or symbols, that can hook into the constituent flows and call upon specific picts.


##Under development - do not use



