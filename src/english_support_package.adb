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
   use text_io;

   package body ewds_record_io is
      package integer_io is new text_io.integer_io(integer);
      use part_of_speech_type_io;
      use frequency_type_io;
      use integer_io;
      spacer : character := ' ';
      nwidth : constant := 5;

      procedure get(f : in text_io.file_type; p : out ewds_record) is
      begin
         get(f, p.w);
         get(f, spacer);
         get(f, p.aux);
         get(f, spacer);
         get(f, p.n);
         get(f, spacer);
         get(f, p.pofs);
         get(f, spacer);
         get(f, p.freq);
         get(f, spacer);
         get(f, p.semi);
         get(f, spacer);
         get(f, p.kind);
         get(f, spacer);
         get(f, p.rank);
      end get;

      procedure get(p : out ewds_record) is
      begin
         get(p.w);
         get(spacer);
         get(p.aux);
         get(spacer);
         get(p.n);
         get(spacer);
         get(p.pofs);
         get(spacer);
         get(p.freq);
         get(spacer);
         get(p.semi);
         get(spacer);
         get(p.kind);
         get(spacer);
         get(p.rank);
      end get;

      procedure put(f : in text_io.file_type; p : in ewds_record) is
      begin
         put(f, p.w);
         put(f, ' ');
         put(f, p.aux);
         put(f, ' ');
         put(f, p.n);
         put(f, ' ');
         put(f, p.pofs);
         put(f, ' ');
         put(f, p.freq);
         put(f, ' ');
         put(f, p.semi, nwidth);
         put(f, ' ');
         put(f, p.kind, nwidth);
         put(f, ' ');
         put(f, p.rank, nwidth);
      end put;

      procedure put(p : in ewds_record) is
      begin
         put(p.w);
         put(' ');
         put(p.aux);
         put(' ');
         put(p.n);
         put(' ');
         put(p.pofs);
         put(' ');
         put(p.freq);
         put(' ');
         put(p.semi, nwidth);
         put(' ');
         put(p.kind, nwidth);
         put(' ');
         put(p.rank, nwidth);
      end put;

      procedure get(s : in string; p : out ewds_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         p.w := s(l+1..l+eword_size);
         l := l + eword_size + 1;
         p.aux := s(l+1..l+aux_word_size);
         l := l + aux_word_size + 1;
         get(s(l+1..s'last), p.n, l);
         l := l + 1;
         get(s(l+1..s'last), p.pofs, l);
         l := l + 1;
         get(s(l+1..s'last), p.freq, l);
         l := l + 1;
         get(s(l+1..s'last), p.semi, l);
         l := l + 1;
         get(s(l+1..s'last), p.kind, l);
         l := l + 1;
         get(s(l+1..s'last), p.rank, last);
      end get;

      procedure put(s : out string; p : in ewds_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + eword_size;
         s(l+1..m) :=  p.w;
         l := m + 1;
         s(l) :=  ' ';
         m := l + aux_word_size;
         s(l+1..m) := p.aux;
         l := m + 1;
         s(l) :=  ' ';
         m := l + line_number_width;
         put(s(l+1..m), p.n);
         s(l) :=  ' ';
         m := l + part_of_speech_type_io.default_width;
         put(s(l+1..m), p.pofs);
         s(l) :=  ' ';
         m := l + frequency_type_io.default_width;
         put(s(l+1..m), p.freq);
         s(l) :=  ' ';
         m := l + priority_width;
         put(s(l+1..m), p.semi, nwidth);
         s(l) :=  ' ';
         m := l + priority_width;
         put(s(l+1..m), p.kind, nwidth);
         s(l) :=  ' ';
         m := l + priority_width;
         put(s(l+1..m), p.rank, nwidth);

         s(m+1..s'last) := (others => ' ');
      end put;

   end ewds_record_io;

end english_support_package;
