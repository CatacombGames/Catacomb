Catacomb
========

This repository contains the source code for Catacomb. The source code is
designed for Borland Turbo Pascal 5.5 and compiled fine at the time of the
release. Use MAKECAT.BAT to compile.

It is released under the GNU GPLv2. Please see COPYING for license details.

This release does not affect the licensing for the game data files. Although
they are a part of the free Gamer's Edge Sampler, this release does not include
the right to commercially exploit the game data and the original copyright
terms continue to hold.

Furthermore, the OBJ files included in this repository for your convenience are
considered game data and shall not be commercially exploited. You are allowed
to use (link) these non-free resources as an exception to the GPL for the
purposes of running the original game. You may distribute such program
following the terms of the GNU GPL to the maximum extent possible.

Compile options
---------------

The game can be made to record a demo file (DEMO.CAT) by uncommenting the
block in CATACOMB.PAS with 'playmode:=demosave;'.

There also exists an editor mode which can be enabled by removing the SAMPLER
define at the top of CATACOMB.PAS. At the demo sequence press E to access the
editor. Note that the editor requires and creates uncompressed level data, so
if done correctly the level data will be scrambled until you open the editor.
