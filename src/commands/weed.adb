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

with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
procedure Weed (W : in out String;
                Pofs : in Part_Of_Speech_Type) is

   --  In contrast to the Latin phase where the prioritization takes
   --  is at runtime for the English most of the work is done beforehand
   --  both the setting of a priority class for each entry in the scan
   --  of DICTLINE and the WEEDing/TRIMming done herein

   --  There may not be much reason to WEED
   --  If there are a hundred "the", does it matter.  No one should Input "the"
   --  But it is a matter of logic and secondary effects (special on "the")

   Kill : Boolean := False;
begin

   --  Conjunctions
   if (Pofs /= Conj) and then
     (W = "and"   or
     W = "or"    or
     W = "but"   or
     W = "if")
   then

      Kill := True;
   end if;

   --  Prepositions
   if (Pofs /= Prep) and then
     (W = "of"    or
     W = "to"    or
     W = "in"    or
     W = "into"   or
     W = "with"  or
     W = "w"   or
     W = "without"   or
     W = "for"   or
     W = "per"   or
     W = "on"    or
     W = "upon"   or
     W = "by"    or
     W = "from"  or
     W = "between"   or
     W = "at"    or
     W = "towards"   or
     W = "under"   or
     W = "before"   or
     W = "against"   or
     W = "around"   or
     W = "through"  or
     W = "after"   or
     W = "like"   or
     W = "similar"   or
     W = "than"   or
     W = "as")

   then

      Kill := True;
   end if;

   if
     (Pofs /= N) and then
     (--  General nouns
     W = "person"   or
     W = "man"      or
     W = "men"   or
     W = "woman"   or
     W = "member"   or
     W = "species"   or
     W = "instrument"   or
     W = "word"   or
     W = "words"   or
     --W = "shape"   or
     W = "parts"   or
     W = "title"   or
     W = "office"   or
     W = "thing"   or
     W = "day"   or
     W = "land"   or
     W = "plant"   or
     W = "plants"   or
     W = "tree"   or
     W = "fish"   or
     W = "stone"   or
     W = "stones"   or
     W = "gem"     or
     W = "vessel"   or
     W = "pieces"   or
     W = "animal"   or
     W = "bird"   or
     W = "measure" or
     W = "inhabitant"   or
     W = "place"   or
     W = "tribe"   or
     W = "group"   or
     W = "official"   or
     W = "thing"   or
     W = "things"   or
     W = "something"   or
     --W = "matter"   or
     W = "law")

   then
      Kill := True;

   end if;

   if

     W = "something"   or
     W = "quality"   or
     W = "heap"   or
     W = "amount"   or
     W = "money"   or
     W = "part"   or
     W = "front"   or
     W = "preparation"   or
     W = "purpose"   or
     W = "bit"   or
     W = "way"   or
     W = "maker"   or
     W = "material"  or
     W = "action"   or
     W = "act"   or
     W = "form"   or
     W = "point"   or
     W = "right"   or
     W = "order"   or
     W = "area"   or
     W = "rest"   or
     W = "cover"   or

     --  Common nouns

     W = "Rome"   or
     W = "rome"   or
     W = "praenomen"   or
     W = "gens"   or
     W = "offering"   or
     W = "note"   or
     W = "water"   or
     W = "ear"   or
     W = "end"   or
     W = "ritual"   or
     W = "rite"   or
     W = "hair"   or
     W = "time"   or
     W = "charactistic"   or
     W = "building"   or
     W = "sea"   or
     W = "ship"

   then
      Kill := True;

   end if;

   if

     (Pofs /= Adj) and then
     (--Adjectives

     W = "some"   or
     W = "several" or
     W = "another"   or
     W = "male"   or
     W = "legal"   or
     W = "female"   or
     W = "official"   or
     W = "no"   or
     W = "wild"   or
     W = "dark"   or
     W = "sacred"   or
     W = "Roman"   or
     W = "roman"   or
     W = "precious"   or
     W = "short"   or
     W = "long"   or
     W = "low"   or
     W = "young"   or
     W = "old"   or
     W = "large"   or
     W = "light"   or
     W = "round"   or
     W = "high"   or
     W = "near"   or
     W = "little"   or
     W = "small")
   then
      Kill := True;

   end if;

   if

     (Pofs /= Adj) and then
     (--More Adjectives
     W = "more"   or
     W = "military"  or
     W = "many"   or
     W = "suitable"   or
     W = "hot"   or
     W = "used"   or
     W = "joint"   or
     W = "proper"   or
     W = "great"   or  --  great-great uncle
     W = "full"   or
     W = "sexual"   or
     W = "public"   or
     W = "white"   or
     W = "secret"   or
     W = "hard"   or
     W = "good"   or
     W = "fine"   or
     W = "common"
     )
   then
      Kill := True;

   end if;

   if

     (Pofs /= Adv) and then
     (
     W = "up"    or
     W = "out"   or
     --W = "away"   or
     W = "over"   or
     W = "down"   or
     W = "back"   or
     W = "forth"   or
     W = "foward"   or
     W = "about"   or
     W = "together"   or
     W = "off"     or

     --Adverbs (pure)
     W = "much"   or
     W = "throughly"   or
     W = "closly"   or
     W = "well"   or
     W = "very"   or
     W = "not"   or
     W = "too"   or
     W = "also"   or
     W = "when"   or
     W = "where"   or
     W = "then"   or
     W = "there"   or
     W = "so")

   then
      Kill := True;

   end if;

   if
     (Pofs /= Pron) and then
     (Pofs /= Pack) and then
     (

     --  Pronouns and indefinites
     W = "one"   or
     W = "ones"   or
     W = "he"   or
     W = "any"   or
     W = "anyone"   or
     W = "anything"   or
     W = "each"   or
     W = "every"   or
     W = "other"   or
     W = "you"   or
     W = "who"   or
     W = "whatever"   or
     W = "oneself"   or
     W = "self"   or
     W = "all"   or
     W = "it"   or
     W = "this"   or
     W = "she"   or
     W = "such"   or
     W = "what"   or
     W = "which"   or
     W = "that"   or
     W = "same")
   then
      Kill := True;
   end if;

   if W = "kind"   or
     W = "manner"   or
     W = "variety"   or

     --  Posessives
     W = "its"   or
     W = "own"   or
     W = "his"   or
     W = "ones"  or
     W = "one's" or

     W = "pertaining"   or
     W = "belonging"   or
     W = "containing"   or
     W = "consisting"   or
     W = "relating"   or
     W = "resembling"   or
     W = "abounding"   or
     W = "concerned"   or
     W = "producing"   or
     W = "connected"   or
     W = "made"   or
     W = "used"   or
     W = "having"
   then
      Kill := True;
   end if;

   if
     (Pofs /= V) and then
     (--  Verbs
     W = "take"   or
     W = "make"  or
     W = "go"   or --      !!
     W = "bring"   or
     W = "cut"   or
     W = "Put"   or
     W = "set"   or
     W = "grow"   or
     W = "give"   or
     W = "cause"   or
     W = "turn"   or
     W = "fall"   or
     W = "hold"   or
     W = "keep"   or
     W = "construct"   or
     W = "throw"   or
     W = "lay"   or
     W = "remove"   or
     W = "produce"   or
     W = "use"   or
     W = "order"   or
     W = "provide"   or

     W = "being"   or
     W = "making"   or
     W = "lacking")
   then
      Kill := True;
   end if;

   if
     --  Compounding verbs
     W = "have"  or
     W = "has"   or
     W = "had"   or
     W = "was"   or
     W = "be"    or
     W = "become"   or
     W = "can"   or
     W = "do"   or
     W = "may"   or
     W = "must"   or
     W = "let"   or

     --  Supporting verbs
     W = "is"   or
     W = "been"   or
     --W = "attempt"   or
     W = "begin"                    --or
   then
      Kill := True;
   end if;

   if Kill then
      for I in W'Range  loop
         W (I) := '\';
      end loop;
   end if;
end Weed;
