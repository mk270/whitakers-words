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

with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
package body Latin_Utils.Dictionary_Package is

   ---------------------------------------------------------------------------

   MNPC_IO_Default_Width : constant Natural := 6;
   Numeral_Value_Type_IO_Default_Width : constant Natural := 5;

   ---------------------------------------------------------------------------

   function Number_Of_Stems (Part : Part_Of_Speech_Type) return Stem_Key_Type is
   begin
      case Part is
         when N       => return 2;
         when Pron    => return 2;
         when Pack    => return 2;
         when Adj     => return 4;
         when Num     => return 4;
         when Adv     => return 3;
         when V       => return 4;
         when Vpar    => return 0;
         when Supine  => return 0;
         when Prep    => return 1;
         when Conj    => return 1;
         when Interj  => return 1;
         when X       => return 0;
         when Tackon .. Suffix => return 0;
      end case;
   end Number_Of_Stems;

   ---------------------------------------------------------------------------

   package body Parse_Record_IO is separate;
   package body Noun_Entry_IO is separate;
   package body Pronoun_Entry_IO is separate;
   package body Propack_Entry_IO is separate;
   package body Adjective_Entry_IO is separate;
   package body Numeral_Entry_IO is separate;
   package body Adverb_Entry_IO is separate;
   package body Verb_Entry_IO is separate;
   package body Preposition_Entry_IO is separate;
   package body Conjunction_Entry_IO is separate;
   package body Interjection_Entry_IO is separate;
   package body Part_Entry_IO is separate;
   package body Kind_Entry_IO is separate;
   package body Translation_Record_IO is separate;
   package body Dictionary_Entry_IO is separate;

   ---------------------------------------------------------------------------

   function "<" (Left, Right : Part_Entry) return Boolean is
   begin
      if Left.Pofs = Right.Pofs  then
         case Left.Pofs is
            when N =>
               if Left.N.Decl < Right.N.Decl  or else
                 (Left.N.Decl = Right.N.Decl  and then
                 Left.N.Gender < Right.N.Gender)  or else
                 ((Left.N.Decl = Right.N.Decl  and
                 Left.N.Gender = Right.N.Gender)  and then
                 Left.N.Kind < Right.N.Kind)
               then
                  return True;
               end if;
            when Pron =>
               if Left.Pron.Decl < Right.Pron.Decl  or else
                 (Left.Pron.Decl = Right.Pron.Decl  and then
                 Left.Pron.Kind < Right.Pron.Kind)
               then
                  return True;
               end if;
            when Pack =>
               if Left.Pack.Decl < Right.Pack.Decl  or else
                 (Left.Pack.Decl = Right.Pack.Decl  and then
                 Left.Pack.Kind < Right.Pack.Kind)
               then
                  return True;
               end if;
            when Adj =>
               if Left.Adj.Decl < Right.Adj.Decl   or else
                 (Left.Adj.Decl = Right.Adj.Decl  and then
                 Left.Adj.Co < Right.Adj.Co)
               then
                  return True;
               end if;
            when Num =>
               if Left.Num.Decl < Right.Num.Decl  or else
                 (Left.Num.Decl = Right.Num.Decl  and then
                 Left.Num.Sort < Right.Num.Sort)  or else
                 ((Left.Num.Decl = Right.Num.Decl)  and then
                 (Left.Num.Sort = Right.Num.Sort)   and then
                 Left.Num.Value < Right.Num.Value)
               then
                  return True;
               end if;
            when Adv =>
               return Left.Adv.Co < Right.Adv.Co;
            when V =>
               if (Left.V.Con < Right.V.Con)  or else
                 (Left.V.Con = Right.V.Con  and then
                 Left.V.Kind < Right.V.Kind)
               then
                  return True;
               end if;
            when Prep =>
               return Left.Prep.Obj < Right.Prep.Obj;
            when Vpar .. Supine =>
               null;
            when X =>
               null;
            when Conj .. Suffix =>
               null;
         end case;
      else
         return Left.Pofs < Right.Pofs;
      end if;
      return False;
   exception
      when Constraint_Error  =>
         return Left.Pofs < Right.Pofs;
   end "<";

   ---------------------------------------------------------------------------

   overriding function "<=" (Left, Right : Area_Type) return Boolean is
   begin
      if Right = Left or else Right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   ---------------------------------------------------------------------------
   -- Used to initialize Latin_Utils Dictionary_Package by setting various vars.
   -- FIXME: Make AT LEAST some of these initializations to happen in spec,
   --   thus eliminating risks of someone modifying them and in result breaking
   --   every nested package.
   procedure Initialize
   is
   begin
      Dictionary_Kind_IO.Default_Width := Dictionary_Kind'Width;

      Area_Type_IO.Default_Width := Area_Type'Width;

      Geo_Type_IO.Default_Width := Geo_Type'Width;

      Frequency_Type_IO.Default_Width := Frequency_Type'Width;

      Source_Type_IO.Default_Width := Source_Type'Width;

      Parse_Record_IO.Default_Width :=
        Stem_Type_IO.Default_Width + 1 +
        Inflection_Record_IO.Default_Width + 1 +
        Dictionary_Kind_IO.Default_Width + 1 +
        MNPC_IO_Default_Width;
      Noun_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Gender_Type_IO.Default_Width + 1 +
        Noun_Kind_Type_IO.Default_Width;
      Pronoun_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Pronoun_Kind_Type_IO.Default_Width;
      Propack_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Pronoun_Kind_Type_IO.Default_Width;
      Adjective_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Comparison_Type_IO.Default_Width;
      Adverb_Entry_IO.Default_Width :=
        Comparison_Type_IO.Default_Width;
      Verb_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Verb_Kind_Type_IO.Default_Width;
      Preposition_Entry_IO.Default_Width := 0;
      Conjunction_Entry_IO.Default_Width := 0;

      Interjection_Entry_IO.Default_Width := 0;
      Numeral_Entry_IO.Default_Width :=
        Decn_Record_IO.Default_Width + 1 +
        Numeral_Sort_Type_IO.Default_Width + 1 +
        Numeral_Value_Type_IO_Default_Width;

      Part_Entry_IO.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
        Numeral_Entry_IO.Default_Width;     --  Largest

      Translation_Record_IO.Default_Width :=
        Age_Type_IO.Default_Width + 1 +
        Area_Type_IO.Default_Width + 1 +
        Geo_Type_IO.Default_Width + 1 +
        Frequency_Type_IO.Default_Width + 1 +
        Source_Type_IO.Default_Width;

      Dictionary_Entry_IO.Default_Width := 4 * (Max_Stem_Size + 1) +
        Part_Entry_IO.Default_Width + 1 +
        Translation_Record_IO.Default_Width + 1 +
        Max_Meaning_Size;
   end Initialize;

   ---------------------------------------------------------------------------

begin
   Initialize;
end Latin_Utils.Dictionary_Package;
