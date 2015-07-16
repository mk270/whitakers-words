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

with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Preface;
package body Latin_Utils.Inflections_Package is

   ---------------------------------------------------------------------------

   function "<" (Left, Right : Decn_Record) return Boolean is
   begin
      if Left.Which < Right.Which  or else
        (Left.Which = Right.Which  and then
        Left.Var < Right.Var)
      then
         return True;
      else
         return False;
      end if;
   end "<";

   ---------------------------------------------------------------------------

   function "<" (left, right : quality_record) return Boolean is
   begin
      if left.pofs = right.pofs  then
         case left.pofs is
            when N =>
               if left.N.Decl.Which < right.N.Decl.Which  or else
                 (left.N.Decl.Which = right.N.Decl.Which  and then
                 left.N.Decl.Var < right.N.Decl.Var)  or else
                 (left.N.Decl.Which = right.N.Decl.Which  and then
                 left.N.Decl.Var = right.N.Decl.Var  and then
                 left.N.Number < right.N.Number) or else
                 (left.N.Decl.Which = right.N.Decl.Which  and then
                 left.N.Decl.Var = right.N.Decl.Var  and then
                 left.N.Number = right.N.Number and then
                 left.N.Of_Case < right.N.Of_Case) or else
                 (left.N.Decl.Which = right.N.Decl.Which  and then
                 left.N.Decl.Var = right.N.Decl.Var  and then
                 left.N.Number = right.N.Number and then
                 left.N.Of_Case = right.N.Of_Case and then
                 left.N.Gender < right.N.Gender)
               then
                  return True;
               end if;
            when Pron =>
               if left.Pron.Decl.Which < right.Pron.Decl.Which  or else
                 (left.Pron.Decl.Which = right.Pron.Decl.Which  and then
                 left.Pron.Decl.Var < right.Pron.Decl.Var)  or else
                 (left.Pron.Decl.Which = right.Pron.Decl.Which  and then
                 left.Pron.Decl.Var = right.Pron.Decl.Var  and then
                 left.Pron.Number < right.Pron.Number) or else
                 (left.Pron.Decl.Which = right.Pron.Decl.Which  and then
                 left.Pron.Decl.Var = right.Pron.Decl.Var  and then
                 left.Pron.Number = right.Pron.Number and then
                 left.Pron.Of_Case < right.Pron.Of_Case) or else
                 (left.Pron.Decl.Which = right.Pron.Decl.Which  and then
                 left.Pron.Decl.Var = right.Pron.Decl.Var  and then
                 left.Pron.Number = right.Pron.Number and then
                 left.Pron.Of_Case = right.Pron.Of_Case and then
                 left.Pron.Gender < right.Pron.Gender)
               then
                  return True;
               end if;
            when Pack =>
               if left.Pack.Decl.Which < right.Pack.Decl.Which  or else
                 (left.Pack.Decl.Which = right.Pack.Decl.Which  and then
                 left.Pack.Decl.Var < right.Pack.Decl.Var)  or else
                 (left.Pack.Decl.Which = right.Pack.Decl.Which  and then
                 left.Pack.Decl.Var = right.Pack.Decl.Var  and then
                 left.Pack.Number < right.Pack.Number) or else
                 (left.Pack.Decl.Which = right.Pack.Decl.Which  and then
                 left.Pack.Decl.Var = right.Pack.Decl.Var  and then
                 left.Pack.Number = right.Pack.Number and then
                 left.Pack.Of_Case < right.Pack.Of_Case) or else
                 (left.Pack.Decl.Which = right.Pack.Decl.Which  and then
                 left.Pack.Decl.Var = right.Pack.Decl.Var  and then
                 left.Pack.Number = right.Pack.Number and then
                 left.Pack.Of_Case = right.Pack.Of_Case and then
                 left.Pack.Gender < right.Pack.Gender)
               then
                  return True;
               end if;
            when Adj =>
               if left.Adj.Decl.Which < right.Adj.Decl.Which  or else
                 (left.Adj.Decl.Which = right.Adj.Decl.Which  and then
                 left.Adj.Decl.Var < right.Adj.Decl.Var)  or else
                 (left.Adj.Decl.Which = right.Adj.Decl.Which  and then
                 left.Adj.Decl.Var = right.Adj.Decl.Var  and then
                 left.Adj.Number < right.Adj.Number) or else
                 (left.Adj.Decl.Which = right.Adj.Decl.Which  and then
                 left.Adj.Decl.Var = right.Adj.Decl.Var  and then
                 left.Adj.Number = right.Adj.Number and then
                 left.Adj.Of_Case < right.Adj.Of_Case) or else
                 (left.Adj.Decl.Which = right.Adj.Decl.Which  and then
                 left.Adj.Decl.Var = right.Adj.Decl.Var  and then
                 left.Adj.Number = right.Adj.Number and then
                 left.Adj.Of_Case = right.Adj.Of_Case and then
                 left.Adj.Gender < right.Adj.Gender)  or else
                 (left.Adj.Decl.Which = right.Adj.Decl.Which  and then
                 left.Adj.Decl.Var = right.Adj.Decl.Var  and then
                 left.Adj.Number = right.Adj.Number and then
                 left.Adj.Of_Case = right.Adj.Of_Case and then
                 left.Adj.Gender = right.Adj.Gender  and then
                 left.Adj.Comparison < right.Adj.Comparison)
               then
                  return True;
               end if;
            when Adv =>
               return left.Adv.Comparison < right.Adv.Comparison;
            when V =>
               if (left.V.Con.Which < right.V.Con.Which)  or else
                 (left.V.Con.Which = right.V.Con.Which  and then
                 left.V.Con.Var < right.V.Con.Var)  or else
                 (left.V.Con.Which = right.V.Con.Which  and then
                 left.V.Con.Var = right.V.Con.Var  and then
                 left.V.Number < right.V.Number) or else
                 (left.V.Con.Which = right.V.Con.Which  and then
                    left.V.Con.Var = right.V.Con.Var  and then
                    left.V.Number = right.V.Number and then
                    left.V.Tense_Voice_Mood.Tense <
                    right.V.Tense_Voice_Mood.Tense) or else
                 (left.V.Con.Which = right.V.Con.Which  and then
                    left.V.Con.Var = right.V.Con.Var  and then
                    left.V.Number = right.V.Number and then
                    left.V.Tense_Voice_Mood.Tense =
                    right.V.Tense_Voice_Mood.Tense and then
                    left.V.Tense_Voice_Mood.Voice <
                    right.V.Tense_Voice_Mood.Voice) or else
                 (left.V.Con.Which = right.V.Con.Which  and then
                    left.V.Con.Var = right.V.Con.Var  and then
                    left.V.Number = right.V.Number and then
                    left.V.Tense_Voice_Mood.Tense =
                    right.V.Tense_Voice_Mood.Tense and then
                    left.V.Tense_Voice_Mood.Voice =
                    right.V.Tense_Voice_Mood.Voice and then
                    left.V.Tense_Voice_Mood.Mood <
                    right.V.Tense_Voice_Mood.Mood)  or else
                 (left.V.Con.Which = right.V.Con.Which and then
                    left.V.Con.Var = right.V.Con.Var and then
                    left.V.Number = right.V.Number and then
                    left.V.Tense_Voice_Mood.Tense =
                    right.V.Tense_Voice_Mood.Tense and then
                    left.V.Tense_Voice_Mood.Voice =
                    right.V.Tense_Voice_Mood.Voice and then
                    left.V.Tense_Voice_Mood.Mood  =
                    right.V.Tense_Voice_Mood.Mood  and then
                    left.V.Person < right.V.Person)
               then
                  return True;
               end if;
            when Vpar =>
               if left.Vpar.Con.Which < right.Vpar.Con.Which  or else
                 (left.Vpar.Con.Which = right.Vpar.Con.Which  and then
                 left.Vpar.Con.Var < right.Vpar.Con.Var)  or else
                 (left.Vpar.Con.Which = right.Vpar.Con.Which  and then
                 left.Vpar.Con.Var = right.Vpar.Con.Var  and then
                 left.Vpar.Number < right.Vpar.Number) or else
                 (left.Vpar.Con.Which = right.Vpar.Con.Which  and then
                 left.Vpar.Con.Var = right.Vpar.Con.Var  and then
                 left.Vpar.Number = right.Vpar.Number and then
                 left.Vpar.Of_Case < right.Vpar.Of_Case) or else
                 (left.Vpar.Con.Which = right.Vpar.Con.Which  and then
                 left.Vpar.Con.Var = right.Vpar.Con.Var  and then
                 left.Vpar.Number = right.Vpar.Number and then
                 left.Vpar.Of_Case = right.Vpar.Of_Case and then
                 left.Vpar.Gender < right.Vpar.Gender)
               then
                  return True;
               end if;
            when Supine =>
               if left.Supine.Con.Which < right.Supine.Con.Which  or else
                 (left.Supine.Con.Which = right.Supine.Con.Which  and then
                 left.Supine.Con.Var < right.Supine.Con.Var)  or else
                 (left.Supine.Con.Which = right.Supine.Con.Which  and then
                 left.Supine.Con.Var = right.Supine.Con.Var  and then
                 left.Supine.Number < right.Supine.Number) or else
                 (left.Supine.Con.Which = right.Supine.Con.Which  and then
                 left.Supine.Con.Var = right.Supine.Con.Var  and then
                 left.Supine.Number = right.Supine.Number and then
                 left.Supine.Of_Case < right.Supine.Of_Case) or else
                 (left.Supine.Con.Which = right.Supine.Con.Which  and then
                 left.Supine.Con.Var = right.Supine.Con.Var  and then
                 left.Supine.Number = right.Supine.Number and then
                 left.Supine.Of_Case = right.Supine.Of_Case and then
                 left.Supine.Gender < right.Supine.Gender)
               then
                  return True;
               end if;
            when Prep =>
               return left.Prep.Of_Case < right.Prep.Of_Case;
            when Conj =>
               null;
            when Interj =>
               null;
            when Num =>
               if left.Num.Decl.Which < right.Num.Decl.Which  or else
                 (left.Num.Decl.Which = right.Num.Decl.Which  and then
                 left.Num.Decl.Var < right.Num.Decl.Var)  or else
                 (left.Num.Decl.Which = right.Num.Decl.Which  and then
                 left.Num.Decl.Var = right.Num.Decl.Var  and then
                 left.Num.Number < right.Num.Number) or else
                 (left.Num.Decl.Which = right.Num.Decl.Which  and then
                 left.Num.Decl.Var = right.Num.Decl.Var  and then
                 left.Num.Number = right.Num.Number and then
                 left.Num.Of_Case < right.Num.Of_Case) or else
                 (left.Num.Decl.Which = right.Num.Decl.Which  and then
                 left.Num.Decl.Var = right.Num.Decl.Var  and then
                 left.Num.Number = right.Num.Number and then
                 left.Num.Of_Case = right.Num.Of_Case and then
                 left.Num.Gender < right.Num.Gender)  or else
                 (left.Num.Decl.Which = right.Num.Decl.Which  and then
                 left.Num.Decl.Var = right.Num.Decl.Var  and then
                 left.Num.Number = right.Num.Number and then
                 left.Num.Of_Case = right.Num.Of_Case and then
                 left.Num.Gender = right.Num.Gender  and then
                 left.Num.Sort < right.Num.Sort)
               then
                  return True;
               end if;
            when Tackon .. Suffix =>
               null;
            when X =>
               null;
         end case;
      else
         return left.pofs < right.pofs;
      end if;
      return False;
   exception
      when Constraint_Error  =>
         return left.pofs < right.pofs;
   end "<";

   overriding function "<="
     (left, right : Part_Of_Speech_Type)
     return Boolean is
   begin
      if right = left  or else
        (left = Pack and right = Pron)  or else
        right = X
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   function "<="
     (left, right : Decn_Record)
     return Boolean is
   begin
      if right = left  or else
        (right = Decn_Record'(0, 0)  and left.Which /= 9)  or else
        right = Decn_Record'(left.Which, 0)
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<="
     (left, right : Gender_Type)
     return Boolean is
   begin
      if right = left  or else
        right = X     or else
        (right = C  and then (left = M or left = F))
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<="
     (left, right : Case_Type)
     return Boolean is
   begin
      if right = left or else right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<="
     (left, right : Number_Type)
     return Boolean is
   begin
      if right = left or else right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Person_Type) return Boolean is
   begin
      if right = left or else right = 0 then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Comparison_Type) return Boolean is
   begin
      if right = left or else right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   function "<=" (left, right : Tense_Voice_Mood_Record)  return Boolean is
   begin
      if (right.Tense = left.Tense or else right.Tense = X) and then
         (right.Voice = left.Voice or else right.Voice = X) and then
         (right.Mood = left.Mood or else right.Mood = X)
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Noun_Kind_Type)   return Boolean is
   begin
      if right = left or else right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<="
     (left, right : Pronoun_Kind_Type)
     return Boolean is
   begin
      if right = left or else right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Stem_Key_Type)   return Boolean is
   begin            --  Only works for 2 stem parts, not verbs
      if right = left or else right = 0 then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Age_Type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Frequency_Type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   package body Stem_Type_IO is separate;

   package body Decn_Record_IO is separate;

   package body Tense_Voice_Mood_Record_IO is separate;

   package body Noun_Record_IO is separate;

   package body Pronoun_Record_IO is separate;

   package body Propack_Record_IO is separate;

   package body Adjective_Record_IO is separate;

   package body Numeral_Record_IO is separate;

   package body Adverb_Record_IO is separate;

   package body Verb_Record_IO is separate;

   package body Vpar_Record_IO is separate;

   package body Supine_Record_IO is separate;

   package body Preposition_Record_IO is separate;

   package body Conjunction_Record_IO is separate;

   package body interjection_record_io is
      null_interjection_record : interjection_record;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get (f : in File_Type; i : out interjection_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         i := null_interjection_record;
      end Get;

      procedure Get (i : out interjection_record) is
      begin
         i := null_interjection_record;
      end Get;

      procedure Put (f : in File_Type; i : in interjection_record) is
      begin
         null;
      end Put;

      procedure Put (i : in interjection_record) is
      begin
         null;
      end Put;

      procedure Get
        (s    : in String;
         i    : out interjection_record;
         last : out Integer)
      is
         l : constant Integer := s'First - 1;
      begin
         i := null_interjection_record;
         last := l - 1;
      end Get;

      pragma Warnings (Off, "formal parameter ""i"" is not referenced");
      procedure Put (s : out String; i : in interjection_record) is
         pragma Warnings (On, "formal parameter ""i"" is not referenced");
      begin
         s (s'First .. s'Last) := (others => ' ');
      end Put;

   end interjection_record_io;

   package body tackon_record_io is
      null_tackon_record : tackon_record;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get (f : in File_Type; i : out tackon_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         i := null_tackon_record;
      end Get;

      procedure Get (i : out tackon_record) is
      begin
         i := null_tackon_record;
      end Get;

      procedure Put (f : in File_Type; i : in tackon_record) is
      begin
         null;
      end Put;

      procedure Put (i : in tackon_record) is
      begin
         null;
      end Put;

      procedure Get
        (s    : in String;
         i    : out tackon_record;
         last : out Integer)
      is
         l : constant Integer := s'First - 1;
      begin
         i := null_tackon_record;
         last := l - 1;
      end Get;

      pragma Warnings (Off, "formal parameter ""i"" is not referenced");
      procedure Put (s : out String; i : in tackon_record) is
         pragma Warnings (On, "formal parameter ""i"" is not referenced");
      begin
         s (s'First .. s'Last) := (others => ' ');
      end Put;

   end tackon_record_io;

   package body prefix_record_io is

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get (f : in File_Type; p : out prefix_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         p := null_prefix_record;
      end Get;

      procedure Get (p : out prefix_record) is
      begin
         p := null_prefix_record;
      end Get;

      procedure Put (f : in File_Type; p : in prefix_record) is
      begin
         null;
      end Put;

      procedure Put (p : in prefix_record) is
      begin
         null;
      end Put;

      procedure Get
        (s    : in String;
         p    : out prefix_record;
         last : out Integer)
      is
         l : constant Integer := s'First - 1;
      begin
         p := null_prefix_record;
         last := l - 1;
      end Get;

      pragma Warnings (Off, "formal parameter ""p"" is not referenced");
      procedure Put (s : out String; p : in prefix_record) is
         pragma Warnings (On, "formal parameter ""p"" is not referenced");
      begin
         s (s'First .. s'Last) := (others => ' ');
      end Put;

   end prefix_record_io;

   package body suffix_record_io is

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get (f : in File_Type; p : out suffix_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         p := null_suffix_record;
      end Get;

      procedure Get (p : out suffix_record) is
      begin
         p := null_suffix_record;
      end Get;

      procedure Put (f : in File_Type; p : in suffix_record) is
      begin
         null;
      end Put;

      procedure Put (p : in suffix_record) is
      begin
         null;
      end Put;

      procedure Get
        (s : in String;
         p : out suffix_record;
         last : out Integer)
      is
         l : constant Integer := s'First - 1;
      begin
         p := null_suffix_record;
         last := l - 1;
      end Get;

      pragma Warnings (Off, "formal parameter ""p"" is not referenced");
      procedure Put (s : out String; p : in suffix_record) is
         pragma Warnings (On, "formal parameter ""p"" is not referenced");
      begin
         s (s'First .. s'Last) := (others => ' ');
      end Put;

   end suffix_record_io;

   package body quality_record_io is
      use Part_Of_Speech_Type_IO;
      use Noun_Record_IO;
      use Pronoun_Record_IO;
      use Propack_Record_IO;
      use Adjective_Record_IO;
      use Numeral_Record_IO;
      use Adverb_Record_IO;
      use Verb_Record_IO;
      use Vpar_Record_IO;
      use Supine_Record_IO;
      use Preposition_Record_IO;
      use Conjunction_Record_IO;
      use interjection_record_io;
      use tackon_record_io;
      use prefix_record_io;
      use suffix_record_io;
      spacer : Character := ' ';

      noun  : Noun_Record;
      pronoun : Pronoun_Record;
      propack : Propack_Record;
      adjective : Adjective_Record;
      adverb : Adverb_Record;
      verb : Verb_Record;
      vparticiple : Vpar_Record;
      supin : Supine_Record;
      preposition : Preposition_Record;
      conjunction : Conjunction_Record;
      interjection : interjection_record;
      numeral : Numeral_Record;
      tackn : tackon_record;
      prefx : prefix_record;
      suffx : suffix_record;

      procedure Get (f : in File_Type; p : out quality_record) is
         ps : Part_Of_Speech_Type := X;
      begin
         Get (f, ps);
         Get (f, spacer);
         case ps is
            when N =>
               Get (f, noun);
               p := (N, noun);
            when Pron =>
               Get (f, pronoun);
               p := (Pron, pronoun);
            when Pack =>
               Get (f, propack);
               p := (Pack, propack);
            when Adj =>
               Get (f, adjective);
               p := (Adj, adjective);
            when Num =>
               Get (f, numeral);
               p := (Num, numeral);
            when Adv =>
               Get (f, adverb);
               p := (Adv, adverb);
            when V =>
               Get (f, verb);
               p := (V, verb);
            when Vpar =>
               Get (f, vparticiple);
               p := (Vpar, vparticiple);
            when Supine =>
               Get (f, supin);
               p := (Supine, supin);
            when Prep =>
               Get (f, preposition);
               p := (Prep, preposition);
            when Conj =>
               Get (f, conjunction);
               p := (Conj, conjunction);
            when Interj =>
               Get (f, interjection);
               p := (Interj, interjection);
            when Tackon =>
               Get (f, tackn);
               p := (Tackon, tackn);
            when Prefix =>
               Get (f, prefx);
               p := (Prefix, prefx);
            when Suffix =>
               Get (f, suffx);
               p := (Suffix, suffx);
            when X =>
               p := (pofs => X);
         end case;
         return;
      end Get;

      procedure Get (p : out quality_record) is
         ps : Part_Of_Speech_Type := X;
      begin
         Get (ps);
         Get (spacer);
         case ps is
            when N =>
               Get (noun);
               p := (N, noun);
            when Pron =>
               Get (pronoun);
               p := (Pron, pronoun);
            when Pack =>
               Get (propack);
               p := (Pack, propack);
            when Adj =>
               Get (adjective);
               p := (Adj, adjective);
            when Num =>
               Get (numeral);
               p := (Num, numeral);
            when Adv =>
               Get (adverb);
               p := (Adv, adverb);
            when V =>
               Get (verb);
               p := (V, verb);
            when Vpar =>
               Get (vparticiple);
               p := (Vpar, vparticiple);
            when Supine =>
               Get (supin);
               p := (Supine, supin);
            when Prep =>
               Get (preposition);
               p := (Prep, preposition);
            when Conj =>
               Get (conjunction);
               p := (Conj, conjunction);
            when Interj =>
               Get (interjection);
               p := (Interj, interjection);
            when Tackon =>
               Get (tackn);
               p := (Tackon, tackn);
            when Prefix =>
               Get (prefx);
               p := (Prefix, prefx);
            when Suffix =>
               Get (suffx);
               p := (Suffix, suffx);
            when X =>
               p := (pofs => X);
         end case;
         return;
      end Get;

      procedure Put (f : in File_Type; p : in quality_record) is
         c : constant Positive := Positive (Col (f));
      begin
         Put (f, p.pofs);
         Put (f, ' ');
         case p.pofs is
            when N =>
               Put (f, p.N);
            when Pron =>
               Put (f, p.Pron);
            when Pack =>
               Put (f, p.Pack);
            when Adj =>
               Put (f, p.Adj);
            when Num =>
               Put (f, p.Num);
            when Adv =>
               Put (f, p.Adv);
            when V =>
               Put (f, p.V);
            when Vpar =>
               Put (f, p.Vpar);
            when Supine =>
               Put (f, p.Supine);
            when Prep =>
               Put (f, p.Prep);
            when Conj =>
               Put (f, p.Conj);
            when Interj =>
               Put (f, p.Interj);
            when Tackon =>
               Put (f, p.Tackon);
            when Prefix =>
               Put (f, p.Prefix);
            when Suffix =>
               Put (f, p.Suffix);
            when others =>
               null;
         end case;
         Put (f, String'((
           Integer (Col (f)) .. quality_record_io.Default_Width + c - 1
           => ' ')));
         return;
      end Put;

      procedure Put (p : in quality_record) is
         c : constant Positive := Positive (Col);
      begin
         Put (p.pofs);
         Put (' ');
         case p.pofs is
            when N =>
               Put (p.N);
            when Pron =>
               Put (p.Pron);
            when Pack =>
               Put (p.Pack);
            when Adj =>
               Put (p.Adj);
            when Num =>
               Put (p.Num);
            when Adv =>
               Put (p.Adv);
            when V =>
               Put (p.V);
            when Vpar =>
               Put (p.Vpar);
            when Supine =>
               Put (p.Supine);
            when Prep =>
               Put (p.Prep);
            when Conj =>
               Put (p.Conj);
            when Interj =>
               Put (p.Interj);
            when Tackon =>
               Put (p.Tackon);
            when Prefix =>
               Put (p.Prefix);
            when Suffix =>
               Put (p.Suffix);
            when others =>
               null;
         end case;
         Put (String'((
           Integer (Col) .. quality_record_io.Default_Width + c - 1 => ' ')));
         return;
      end Put;

      procedure Get
        (s : in String;
         p : out quality_record;
         last : out Integer)
      is
         l : Integer := s'First - 1;
         ps : Part_Of_Speech_Type := X;
      begin
         Get (s, ps, l);
         last := l;         --  In case it is not set later
         l := l + 1;
         case ps is
            when N =>
               Get (s (l + 1 .. s'Last), noun, last);
               p := (N, noun);
            when Pron =>
               Get (s (l + 1 .. s'Last), pronoun, last);
               p := (Pron, pronoun);
            when Pack =>
               Get (s (l + 1 .. s'Last), propack, last);
               p := (Pack, propack);
            when Adj =>
               Get (s (l + 1 .. s'Last), adjective, last);
               p := (Adj, adjective);
            when Num =>
               Get (s (l + 1 .. s'Last), numeral, last);
               p := (Num, numeral);
            when Adv =>
               Get (s (l + 1 .. s'Last), adverb, last);
               p := (Adv, adverb);
            when V =>
               Get (s (l + 1 .. s'Last), verb, last);
               p := (V, verb);
            when Vpar =>
               Get (s (l + 1 .. s'Last), vparticiple, last);
               p := (Vpar, vparticiple);
            when Supine =>
               Get (s (l + 1 .. s'Last), supin, last);
               p := (Supine, supin);
            when Prep =>
               Get (s (l + 1 .. s'Last), preposition, last);
               p := (Prep, preposition);
            when Conj =>
               Get (s (l + 1 .. s'Last), conjunction, last);
               p := (Conj, conjunction);
            when Interj =>
               Get (s (l + 1 .. s'Last), interjection, last);
               p := (Interj, interjection);
            when Tackon =>
               Get (s (l + 1 .. s'Last), tackn, last);
               p := (Tackon, tackn);
            when Prefix =>
               Get (s (l + 1 .. s'Last), prefx, last);
               p := (Prefix, prefx);
            when Suffix =>
               Get (s (l + 1 .. s'Last), suffx, last);
               p := (Suffix, suffx);
            when X =>
               p := (pofs => X);
         end case;
         return;
      end Get;

      procedure Put (s : out String; p : in quality_record) is
         --  Note that this does not Put with a uniform width
         --  which would require a constant QUALITY_RECORD_IO.DEFAULT_WIDTH
         --  Rather we Put to minimal size with NOUN_RECORD_IO.DEFAULT_WIDTH,
         --  PRONOUN_RECORD_IO,DEFAULT_WIDTH, . ..
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Part_Of_Speech_Type_IO.Default_Width;
         Put (s (l + 1 .. m), p.pofs);
         l := m + 1;
         s (l) :=  ' ';
         case p.pofs is
            when N =>
               m := l + Noun_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.N);
            when Pron =>
               m := l + Pronoun_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.Pron);
            when Pack =>
               m := l + Propack_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.Pack);
            when Adj =>
               m := l + Adjective_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.Adj);
            when Num =>
               m := l + Numeral_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.Num);
            when Adv =>
               m := l + Adverb_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.Adv);
            when V =>
               m := l + Verb_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.V);
            when Vpar =>
               m := l + Vpar_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.Vpar);
            when Supine =>
               m := l + Supine_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.Supine);
            when Prep =>
               m := l + Preposition_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.Prep);
            when Conj =>
               m := l + Conjunction_Record_IO.Default_Width;
               Put (s (l + 1 .. m), p.Conj);
            when Interj =>
               m := l + interjection_record_io.Default_Width;
               Put (s (l + 1 .. m), p.Interj);
            when Tackon =>
               m := l + tackon_record_io.Default_Width;
               Put (s (l + 1 .. m), p.Tackon);
            when Prefix =>
               m := l + prefix_record_io.Default_Width;
               Put (s (l + 1 .. m), p.Prefix);
            when Suffix =>
               m := l + suffix_record_io.Default_Width;
               Put (s (l + 1 .. m), p.Suffix);
            when others =>
               null;
         end case;
         s (m + 1 .. s'Last) := (others => ' ');
      end Put;

   end quality_record_io;

   package body ending_record_io is
      use Integer_IO;
      spacer : Character := ' ';

      sf : ending := (others => ' ');
      blanks : constant ending := (others => ' ');
      n : ending_size_type := 0;

      procedure Get (f : in File_Type; x : out ending_record) is
      begin
         sf := blanks;
         Get (f, n);
         if n = 0  then
            x := null_ending_record;
         else
            Get (f, spacer);             --  Note this means exactly one blank
            Get (f, sf (1 .. n));
            x := (n, sf);
         end if;
      end Get;

      procedure Get (x : out ending_record) is
      begin
         sf := blanks;
         Get (n);
         if n = 0  then
            x := null_ending_record;
         else
            Get (spacer);
            Get (sf (1 .. n));
            x := (n, sf);
         end if;
      end Get;

      procedure Put (f : in File_Type; x : in ending_record) is
      begin
         Put (f, x.size, 1);
         Put (f, ' ');
         Put (f, x.suf (1 .. x.size) & blanks (x.size + 1 .. max_ending_size));
      end Put;

      procedure Put (x : in ending_record) is
      begin
         Put (x.size, 1);
         Put (' ');
         Put (x.suf (1 .. x.size) & blanks (x.size + 1 .. max_ending_size));
      end Put;

      procedure Get
        (s : in String;
         x : out ending_record;
         last : out Integer)
      is
         l : Integer := s'First - 1;
      begin
         sf := blanks;
         Get (s (l + 1 .. s'Last), n, l);
         if n = 0  then
            x := null_ending_record;
            last := l;
         else
            l := l + 1;
            --if S (L+N - 1) = ' '  or else
            --   S (L+N + 1) /= ' '  then
            --if
            --   S (L+N + 1) /= ' '  then
            -- TEXT_IO.PUT_LINE ("ERROR in INFLECTION =>" & S);
            --else
            sf := s (l + 1 .. l + n) & blanks (n + 1 .. max_ending_size);
            last := l + n;
            x := (n, sf (1 .. n) & blanks (n + 1 .. max_ending_size));
            --end if;
         end if;
      exception
         when others =>
            Ada.Text_IO.Put_Line ("ENDING ERRROR " & s);
      end Get;

      procedure Put (s : out String; x : in ending_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + 2;
         Put (s (l + 1 .. m), x.size);
         m := m  + 1;
         s (m) := ' ';
         if x.size > 0  then
            l := m;
            m := l + x.size;
            s (l + 1 .. m) := x.suf (1 .. x.size);
         end if;
         --  Being very careful here, first to fill out to the MAX_ENDING_SIZE
         l := m;
         m := l + max_ending_size - x.size;
         s (l + 1 .. m) := (others => ' ');
         --  Then to fill out the rest of the out String, if any
         s (m + 1 .. s'Last) := (others => ' ');
      end Put;

   end ending_record_io;

   package body Inflection_Record_IO is
      use quality_record_io;
      use Stem_Key_Type_IO;
      use ending_record_io;
      use Age_Type_IO;
      use Frequency_Type_IO;
      spacer : Character := ' ';

      pe : Inflection_Record;

      procedure Get (f : in File_Type; p : out Inflection_Record) is
      begin
         Get (f, p.qual);
         Get (f, spacer);
         Get (f, p.key);
         Get (f, spacer);
         Get (f, p.ending);
         Get (f, spacer);
         Get (f, p.age);
         Get (f, spacer);
         Get (f, p.freq);
      end Get;

      procedure Get (p : out Inflection_Record) is
      begin
         Get (p.qual);
         Get (spacer);
         Get (p.key);
         Get (spacer);
         Get (p.ending);
         Get (spacer);
         Get (p.age);
         Get (spacer);
         Get (p.freq);
      end Get;

      procedure Put (f : in File_Type; p : in Inflection_Record) is
      begin
         Put (f, p.qual);
         Put (f, ' ');
         Put (f, p.key, 1);
         Put (f, ' ');
         Put (f, p.ending);
         Put (f, ' ');
         Put (f, p.age);
         Put (f, ' ');
         Put (f, p.freq);
      end Put;

      procedure Put (p : in Inflection_Record) is
      begin
         Put (p.qual);
         Put (' ');
         Put (p.key, 1);
         Put (' ');
         Put (p.ending);
         Put (' ');
         Put (p.age);
         Put (' ');
         Put (p.freq);
      end Put;

      procedure Get
        (s    : in String;
         p    : out Inflection_Record;
         last : out Integer)
      is
         l : Integer := s'First - 1;
      begin
         last := 0;
         p := pe;
         Get (s (l + 1 .. s'Last), p.qual, l);
         l := l + 1;
         Get (s (l + 1 .. s'Last), p.key, l);
         l := l + 1;
         Get (s (l + 1 .. s'Last), p.ending, l);
         l := l + 1;
         Get (s (l + 1 .. s'Last), p.age, l);
         l := l + 1;
         Get (s (l + 1 .. s'Last), p.freq, last);
      end Get;

      procedure Put (s : out String; p : in Inflection_Record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + quality_record_io.Default_Width;
         Put (s (l + 1 .. m), p.qual);
         l := m + 1;
         s (l) :=  ' ';
         m := l + 1;
         Put (s (l + 1 .. m), p.key);
         l := m + 1;
         s (l) :=  ' ';
         m := l + ending_record_io.Default_Width;
         Put (s (l + 1 .. m), p.ending);
         l := m + 1;
         s (l) :=  ' ';
         m := l + 1;
         Put (s (l + 1 .. m), p.age);
         l := m + 1;
         s (l) :=  ' ';
         m := l + 1;
         Put (s (l + 1 .. m), p.freq);
         s (m + 1 .. s'Last) := (others => ' ');
      end Put;

   end Inflection_Record_IO;

   -- FIXME this procedure contains four blocks of heavily duplicated code

   procedure establish_inflections_section  is
      --  Loads the inflection array from the file prepared in
      --  FILE_INFLECTIONS_SECTION
      --  If N = 0 (an artifical flag for the section for blank
      --  inflections = 5)
      --  computes the LELL .. LELF indices for use in WORD
      use Inflection_Record_IO;
      use lel_section_io;

      procedure load_lel_indexes is
         --  Load arrays from file
         i  : Integer := 0;
         --IR : INFLECTION_RECORD;
         n, xn : Integer := 0;
         ch, xch : Character := ' ';
         inflections_sections_file : lel_section_io.File_Type;

         -- FIXME: this algebraic type and its values are obviously misnomers
         type Paradigm is (P1, P2, P3, P4);

         function Section_Count (P : Paradigm) return Integer
         is
         begin
            case P is
               when P1 => return 1;
               when P2 => return 2;
               when P3 => return 3;
               when P4 => return 4;
            end case;
         end Section_Count;

         procedure Read_Inflections (P : Paradigm)
         is
            Count : constant Integer := Section_Count (P);
         begin
            lel_section_io.Read (inflections_sections_file,
              lel,
              lel_section_io.Positive_Count (Count));

            i := 1;

            n := lel (i).ending.size;

            ch := lel (i).ending.suf (n);

            xn := n;
            xch := ch;
            lelf (n, ch) := i;

            c1_loop :
            loop
               n1_loop :
               loop
                  case P is
                     when P1 =>
                        exit c1_loop when lel (i) = Null_Inflection_Record;
                     when P2 =>
                        exit c1_loop when lel (i) = Null_Inflection_Record;
                     when P3 =>
                        exit c1_loop when lel (i) = Null_Inflection_Record;
                     when P4 =>
                        exit c1_loop when  lel (i).qual.pofs = Pron  and then
                          (lel (i).qual.Pron.Decl.Which = 1  or
                          lel (i).qual.Pron.Decl.Which = 2);
                  end case;

                  n := lel (i).ending.size;

                  ch := lel (i).ending.suf (n);

                  case P is
                     when P1 =>
                        null;
                     when P2 =>
                        exit n1_loop when ch > 'r';
                     when P3 =>
                        exit n1_loop when ch > 's';
                     when P4 =>
                        null;
                  end case;

                  if ch /= xch  then
                     lell (xn, xch) := i - 1;
                     lelf (n, ch) := i;
                     lell (n, ch) := 0;
                     xch := ch;
                     xn := n;
                  elsif n /= xn  then
                     lell (xn, ch) := i - 1;
                     lelf (n, ch) := i;
                     lell (n, ch) := 0;
                     xn := n;
                     exit n1_loop;
                  end if;

                  i := i + 1;

               end loop n1_loop;

            end loop c1_loop;

            lell (xn, xch) := i - 1;

         end Read_Inflections;

      begin
         Open (inflections_sections_file, In_File, inflections_sections_name);
         number_of_inflections := 0;

         lel_section_io.Read (inflections_sections_file,
           lel,
           lel_section_io.Positive_Count (5));

         i := 1;
         belf (0, ' ') := i;
         bell (0, ' ') := 0;
         loop
            exit when lel (i) = Null_Inflection_Record;
            bel (i) := lel (i);

            bell (0, ' ') := i;
            i := i + 1;
         end loop;

         number_of_inflections := number_of_inflections + i - 1;

         Read_Inflections (P1);
         number_of_inflections := number_of_inflections + i - 1;

         Read_Inflections (P2);
         number_of_inflections := number_of_inflections + i - 1;

         Read_Inflections (P3);
         number_of_inflections := number_of_inflections + i - 1;

         Read_Inflections (P4);

         begin

            n := lel (i).ending.size;

            ch := lel (i).ending.suf (n);

            xn := n;
            xch := ch;
            pelf (n,  ch) := i;
            pell (n,  ch) := 0;

            c_p_loop :
            loop
               n_p_loop :
               loop
                  exit c_p_loop when lel (i) = Null_Inflection_Record;

                  n := lel (i).ending.size;

                  ch := lel (i).ending.suf (n);

                  if ch /= xch  then
                     pell (xn, xch) := i - 1;
                     pelf (n, ch) := i;
                     pell (n, ch) := 0;
                     xch := ch;
                     xn := n;
                  elsif n /= xn  then
                     pell (xn, ch) := i - 1;
                     pelf (n, ch) := i;
                     pell (n, ch) := 0;
                     xn  := n;
                     exit n_p_loop;
                  end if;

                  i := i + 1;
               end loop n_p_loop;
            end loop c_p_loop;
         exception
            when Constraint_Error => null;
         end;

         pell (xn, xch) := i - 1;
         number_of_inflections := number_of_inflections + i - 1;
         Close (inflections_sections_file);
      end load_lel_indexes;

   begin
      Preface.Put ("INFLECTION_ARRAY being loaded");
      Preface.Set_Col (33);
      Preface.Put ("--  ");
      load_lel_indexes;                    --  Makes indexes from array
      Preface.Put (number_of_inflections, 6);
      Preface.Put (" entries");
      Preface.Set_Col (55); Preface.Put_Line ("--  Loaded correctly");
   exception
      when Ada.Text_IO.Name_Error  =>
         New_Line;
         Put_Line ("There is no " & inflections_sections_name & " file.");
         Put_Line ("The program cannot work without one.");
         Put_Line ("Make sure you are in the"
           & " subdirectory containing the files");
         Put_Line ("for inflections, dictionary, addons and uniques.");
         raise give_up;
   end establish_inflections_section;

begin
   --  initialization of body of INFLECTIONS_PACKAGE
   --TEXT_IO.PUT_LINE ("Initializing INFLECTIONS_PACKAGE");

   Part_Of_Speech_Type_IO.Default_Width := Part_Of_Speech_Type'Width;
   Gender_Type_IO.Default_Width := Gender_Type'Width;
   Case_Type_IO.Default_Width := Case_Type'Width;
   Number_Type_IO.Default_Width := Number_Type'Width;
   Person_Type_IO.Default_Width := 1;
   Comparison_Type_IO.Default_Width := Comparison_Type'Width;
   Tense_Type_IO.Default_Width := Tense_Type'Width;
   Voice_Type_IO.Default_Width := Voice_Type'Width;
   Mood_Type_IO.Default_Width := Mood_Type'Width;
   Noun_Kind_Type_IO.Default_Width := Noun_Kind_Type'Width;
   Pronoun_Kind_Type_IO.Default_Width := Pronoun_Kind_Type'Width;
   Verb_Kind_Type_IO.Default_Width := Verb_Kind_Type'Width;
   Numeral_Sort_Type_IO.Default_Width := Numeral_Sort_Type'Width;
   Age_Type_IO.Default_Width := Age_Type'Width;
   Frequency_Type_IO.Default_Width := Frequency_Type'Width;

   Decn_Record_IO.Default_Width :=
     1 + 1 +   --WHICH_TYPE_IO_DEFAULT_WIDTH + 1 +
     1;        --VARIANT_TYPE_IO_DEFAULT_WIDTH;
   Tense_Voice_Mood_Record_IO.Default_Width :=
     Tense_Type_IO.Default_Width + 1 +
     Voice_Type_IO.Default_Width + 1 +
     Mood_Type_IO.Default_Width;

   Noun_Record_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width;

   Pronoun_Record_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width;

   Propack_Record_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width;

   Adjective_Record_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width + 1 +
     Comparison_Type_IO.Default_Width;

   Adverb_Record_IO.Default_Width :=
     Comparison_Type_IO.Default_Width;

   Verb_Record_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Tense_Voice_Mood_Record_IO.Default_Width + 1 +
     Person_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width;

   Vpar_Record_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width + 1 +
     Tense_Voice_Mood_Record_IO.Default_Width;

   Supine_Record_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width;

   Preposition_Record_IO.Default_Width := Case_Type_IO.Default_Width;

   Conjunction_Record_IO.Default_Width := 0;

   interjection_record_io.Default_Width := 0;

   Numeral_Record_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width + 1 +
     Numeral_Sort_Type_IO.Default_Width;

   tackon_record_io.Default_Width := 0;

   prefix_record_io.Default_Width := 0;

   suffix_record_io.Default_Width := 0;

   quality_record_io.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
     Vpar_Record_IO.Default_Width; --  Largest

   ending_record_io.Default_Width := 3 + 1 +
     max_ending_size;

   Inflection_Record_IO.Default_Width := quality_record_io.Default_Width + 1 +
     1  + 1 +
     ending_record_io.Default_Width + 1 +
     Age_Type_IO.Default_Width + 1 +
     Frequency_Type_IO.Default_Width;

end Latin_Utils.Inflections_Package;
