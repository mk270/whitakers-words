-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936–2010)
--
-- This is a free program, which means it is proper to copy it and pass
-- it on to your friends. Consider it a developmental item for which
-- there is no charge. However, just for form, it is Copyrighted
-- (c). Permission is hereby freely given for any and all use of program
-- and data. You can sell it as your own, but at least tell me.
--
-- This version is distributed without obligation, but the developer
-- would appreciate comments and suggestions.
--
-- All parts of the WORDS system, source code and data files, are made freely
-- available to anyone who wishes to use them, for whatever purpose.

package body english_support_package is
   --use EWDS_DIRECT_IO;
   use Ada.Text_IO;

   package body ewds_record_io is
      package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);
      use Part_Of_Speech_Type_IO;
      use Frequency_Type_IO;
      use Integer_IO;
      spacer : Character := ' ';
      nwidth : constant := 5;

      procedure Get (f : in Ada.Text_IO.File_Type; p : out ewds_record) is
      begin
         Get (f, p.w);
         Get (f, spacer);
         Get (f, p.aux);
         Get (f, spacer);
         Get (f, p.n);
         Get (f, spacer);
         Get (f, p.pofs);
         Get (f, spacer);
         Get (f, p.freq);
         Get (f, spacer);
         Get (f, p.semi);
         Get (f, spacer);
         Get (f, p.kind);
         Get (f, spacer);
         Get (f, p.rank);
      end Get;

      procedure Get (p : out ewds_record) is
      begin
         Get (p.w);
         Get (spacer);
         Get (p.aux);
         Get (spacer);
         Get (p.n);
         Get (spacer);
         Get (p.pofs);
         Get (spacer);
         Get (p.freq);
         Get (spacer);
         Get (p.semi);
         Get (spacer);
         Get (p.kind);
         Get (spacer);
         Get (p.rank);
      end Get;

      procedure Put (f : in Ada.Text_IO.File_Type; p : in ewds_record) is
      begin
         Put (f, p.w);
         Put (f, ' ');
         Put (f, p.aux);
         Put (f, ' ');
         Put (f, p.n);
         Put (f, ' ');
         Put (f, p.pofs);
         Put (f, ' ');
         Put (f, p.freq);
         Put (f, ' ');
         Put (f, p.semi, nwidth);
         Put (f, ' ');
         Put (f, p.kind, nwidth);
         Put (f, ' ');
         Put (f, p.rank, nwidth);
      end Put;

      procedure Put (p : in ewds_record) is
      begin
         Put (p.w);
         Put (' ');
         Put (p.aux);
         Put (' ');
         Put (p.n);
         Put (' ');
         Put (p.pofs);
         Put (' ');
         Put (p.freq);
         Put (' ');
         Put (p.semi, nwidth);
         Put (' ');
         Put (p.kind, nwidth);
         Put (' ');
         Put (p.rank, nwidth);
      end Put;

      procedure Get (s : in String; p : out ewds_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         p.w := s (l + 1 .. l + eword_size);
         l := l + eword_size + 1;
         p.aux := s (l + 1 .. l + aux_word_size);
         l := l + aux_word_size + 1;
         Get (s (l + 1 .. s'Last), p.n, l);
         l := l + 1;
         Get (s (l + 1 .. s'Last), p.pofs, l);
         l := l + 1;
         Get (s (l + 1 .. s'Last), p.freq, l);
         l := l + 1;
         Get (s (l + 1 .. s'Last), p.semi, l);
         l := l + 1;
         Get (s (l + 1 .. s'Last), p.kind, l);
         l := l + 1;
         Get (s (l + 1 .. s'Last), p.rank, last);
      end Get;

      procedure Put (s : out String; p : in ewds_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + eword_size;
         s (l + 1 .. m) :=  p.w;
         l := m + 1;
         s (l) :=  ' ';
         m := l + aux_word_size;
         s (l + 1 .. m) := p.aux;
         l := m + 1;
         s (l) :=  ' ';
         m := l + line_number_width;
         Put (s (l + 1 .. m), p.n);
         s (l) :=  ' ';
         m := l + Part_Of_Speech_Type_IO.Default_Width;
         Put (s (l + 1 .. m), p.pofs);
         s (l) :=  ' ';
         m := l + Frequency_Type_IO.Default_Width;
         Put (s (l + 1 .. m), p.freq);
         s (l) :=  ' ';
         m := l + priority_width;
         Put (s (l + 1 .. m), p.semi, nwidth);
         s (l) :=  ' ';
         m := l + priority_width;
         Put (s (l + 1 .. m), p.kind, nwidth);
         s (l) :=  ' ';
         m := l + priority_width;
         Put (s (l + 1 .. m), p.rank, nwidth);

         s (m + 1 .. s'Last) := (others => ' ');
      end Put;

   end ewds_record_io;

end english_support_package;
