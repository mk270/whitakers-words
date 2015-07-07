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

with Strings_Package; use Strings_Package;
package body Dictionary_Package is
   use Stem_Key_Type_IO;

   MNPC_IO_Default_Width : constant Natural := 6;
   Numeral_Value_Type_IO_Default_Width : constant Natural := 5;
   --PART_WIDTH : NATURAL;

   function Number_Of_Stems (Part : Part_Of_Speech_Type) return Stem_Key_Type is
   begin
      case Part is
         when n       => return 2;
         when pron    => return 2;
         when pack    => return 2;
         when adj     => return 4;
         when num     => return 4;
         when adv     => return 3;
         when v       => return 4;
         when vpar    => return 0;
         when supine  => return 0;
         when prep    => return 1;
         when conj    => return 1;
         when interj  => return 1;
         when x       => return 0;
         when tackon .. suffix => return 0;
      end case;
   end Number_Of_Stems;

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

   function "<" (left, right : Part_Entry) return Boolean is
   begin
      if left.pofs = right.pofs  then
         case left.pofs is
            when n =>
               if left.n.Decl < right.n.Decl  or else
                 (left.n.Decl = right.n.Decl  and then
                 left.n.Gender < right.n.Gender)  or else
                 ((left.n.Decl = right.n.Decl  and
                 left.n.Gender = right.n.Gender)  and then
                 left.n.Kind < right.n.Kind)
               then
                  return True;
               end if;
            when pron =>
               if left.pron.Decl < right.pron.Decl  or else
                 (left.pron.Decl = right.pron.Decl  and then
                 left.pron.Kind < right.pron.Kind)
               then
                  return True;
               end if;
            when pack =>
               if left.pack.Decl < right.pack.Decl  or else
                 (left.pack.Decl = right.pack.Decl  and then
                 left.pack.Kind < right.pack.Kind)
               then
                  return True;
               end if;
            when adj =>
               if left.adj.Decl < right.adj.Decl   or else
                 (left.adj.Decl = right.adj.Decl  and then
                 left.adj.Co < right.adj.Co)
               then
                  return True;
               end if;
            when num =>
               if left.num.Decl < right.num.Decl  or else
                 (left.num.Decl = right.num.Decl  and then
                 left.num.Sort < right.num.Sort)  or else
                 ((left.num.Decl = right.num.Decl)  and then
                 (left.num.Sort = right.num.Sort)   and then
                 left.num.Value < right.num.Value)
               then
                  return True;
               end if;
            when adv =>
                  return left.adv.Co < right.adv.Co;
            when v =>
               if (left.v.Con < right.v.Con)  or else
                 (left.v.Con = right.v.Con  and then
                 left.v.Kind < right.v.Kind)
               then
                  return True;
               end if;
            when prep =>
               return left.prep.Obj < right.prep.Obj;
            when others =>
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

   package body Part_Entry_IO is separate;

   package body Kind_Entry_IO is separate;

   package body Translation_Record_IO is separate;

   package body Dictionary_Entry_IO is separate;

   overriding function "<=" (left, right : Area_Type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

begin     --  initialization of body of DICTIONARY_PACKAGE
   --TEXT_IO.PUT_LINE("Initializing DICTIONARY_PACKAGE");

   Dictionary_Kind_IO.Default_Width := Dictionary_Kind'Width;

   --NUMERAL_VALUE_TYPE_IO.DEFAULT_WIDTH := 5;

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

   --  Should make up a MAX of PART_ENTRY + KIND_ENTRY (same POFS) WIDTHS

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

   --TEXT_IO.PUT_LINE("Initialized  DICTIONARY_PACKAGE");
end Dictionary_Package;
