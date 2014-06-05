{ Catacomb Source Code
  Copyright (C) 1993-2014 Flat Rock Software

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write to the Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

Unit SPKlib;

Interface

type
  soundtype= Record
	       start: Word;
	       priority: byte;
	       samples: byte;
	       name: string[11]
	     end;

  soundfile= Record
               id: Array [1..4] of char;   {'SOUN' if good file}
               filelength: word;
	       filler: array [7..16] of byte; {should be 00}
	       sounds: array[1..63] of soundtype;
	       data: array[0..$7BBF] of integer;
             end;


Var
  soundon: boolean;   {true if speaker is to be playing}
  SoundData: ^Soundfile;  {SPKR file, allways PARA aligned}

Procedure StartupSound;
Procedure ShutdownSound;
Procedure PlaySound1 (soundnum:word);
Procedure StopSound;
Procedure WaitEndSound;

Implementation

{$L SOUNDLIB.OBJ}

Procedure StartupSound;
External;

Procedure ShutdownSound;
External;

Procedure PlaySound1 (soundnum:word);
External;

Procedure StopSound;
External;

Procedure WaitEndSound;
External;

Begin
End.

