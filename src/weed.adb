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

with inflections_package; use inflections_package;
procedure weed(w : in out string;
               pofs : in part_of_speech_type) is

   --  In contrast to the Latin phase where the prioritization takes is at runtime
   --  for the English most of the work is done beforehand
   --  both the setting of a priority class for each entry in the scan of DICTLINE
   --  and the WEEDing/TRIMming done herein

   --  There may not be much reason to WEED
   --  If there are a hundred "the", does it matter.  No one should input "the"
   --  But it is a matter of logic and secondary effects (special on "the")

   kill : boolean := false;
begin

   --  Conjunctions
   if (pofs /= conj) and then
     (w = "and"   or
        w = "or"    or
        w = "but"   or
        w = "if"  )
   then

      kill := true;
   end if;

   --  Prepositions
   if (pofs /= prep) and then
     (w = "of"    or
        w = "to"    or
        w = "in"    or
        w = "into"   or
        w = "with"  or
        w = "w"   or
        w = "without"   or
        w = "for"   or
        w = "per"   or
        w = "on"    or
        w = "upon"   or
        w = "by"    or
        w = "from"  or
        w = "between"   or
        w = "at"    or
        w = "towards"   or
        w = "under"   or
        w = "before"   or
        w = "against"   or
        w = "around"   or
        w = "through"  or
        w = "after"   or
        w = "like"   or
        w = "similar"   or
        w = "than"   or
        w = "as"   )

   then

      kill := true;
   end if;

   if
     (pofs /= n) and then
     (--  General nouns
      w = "person"   or
        w = "man"      or
        w = "men"   or
        w = "woman"   or
        w = "member"   or
        w = "species"   or
        w = "instrument"   or
        w = "word"   or
        w = "words"   or
        --W = "shape"   or
        w = "parts"   or
        w = "title"   or
        w = "office"   or
        w = "thing"   or
        w = "day"   or
        w = "land"   or
        w = "plant"   or
        w = "plants"   or
        w = "tree"   or
        w = "fish"   or
        w = "stone"   or
        w = "stones"   or
        w = "gem"     or
        w = "vessel"   or
        w = "pieces"   or
        w = "animal"   or
        w = "bird"   or
        w = "measure" or
        w = "inhabitant"   or
        w = "place"   or
        w = "tribe"   or
        w = "group"   or
        w = "official"   or
        w = "thing"   or
        w = "things"   or
        w = "something"   or
        --W = "matter"   or
        w = "law"         )

   then
      kill := true;

   end if;

   if

     w = "something"   or
     w = "quality"   or
     w = "heap"   or
     w = "amount"   or
     w = "money"   or
     w = "part"   or
     w = "front"   or
     w = "preparation"   or
     w = "purpose"   or
     w = "bit"   or
     w = "way"   or
     w = "maker"   or
     w = "material"  or
     w = "action"   or
     w = "act"   or
     w = "form"   or
     w = "point"   or
     w = "right"   or
     w = "order"   or
     w = "area"   or
     w = "rest"   or
     w = "cover"   or

     --  Common nouns

     w = "Rome"   or
     w = "rome"   or
     w = "praenomen"   or
     w = "gens"   or
     w = "offering"   or
     w = "note"   or
     w = "water"   or
     w = "ear"   or
     w = "end"   or
     w = "ritual"   or
     w = "rite"   or
     w = "hair"   or
     w = "time"   or
     w = "charactistic"   or
     w = "building"   or
     w = "sea"   or
     w = "ship"

   then
      kill := true;

   end if;

   if

     (pofs /= adj) and then
     (--Adjectives

      w = "some"   or
        w = "several" or
        w = "another"   or
        w = "male"   or
        w = "legal"   or
        w = "female"   or
        w = "official"   or
        w = "no"   or
        w = "wild"   or
        w = "dark"   or
        w = "sacred"   or
        w = "Roman"   or
        w = "roman"   or
        w = "precious"   or
        w = "short"   or
        w = "long"   or
        w = "low"   or
        w = "young"   or
        w = "old"   or
        w = "large"   or
        w = "light"   or
        w = "round"   or
        w = "high"   or
        w = "near"   or
        w = "little"   or
        w = "small"         )
   then
      kill := true;

   end if;

   if

     (pofs /= adj) and then
     (--More Adjectives
      w = "more"   or
        w = "military"  or
        w = "many"   or
        w = "suitable"   or
        w = "hot"   or
        w = "used"   or
        w = "joint"   or
        w = "proper"   or
        w = "great"   or  --  great-great uncle
        w = "full"   or
        w = "sexual"   or
        w = "public"   or
        w = "white"   or
        w = "secret"   or
        w = "hard"   or
        w = "good"   or
        w = "fine"   or
        w = "common"
     )
   then
      kill := true;

   end if;

   if

     (pofs /= adv) and then
     (
      w = "up"    or
        w = "out"   or
        --W = "away"   or
        w = "over"   or
        w = "down"   or
        w = "back"   or
        w = "forth"   or
        w = "foward"   or
        w = "about"   or
        w = "together"   or
        w = "off"     or

        --Adverbs (pure)
        w = "much"   or
        w = "throughly"   or
        w = "closly"   or
        w = "well"   or
        w = "very"   or
        w = "not"   or
        w = "too"   or
        w = "also"   or
        w = "when"   or
        w = "where"   or
        w = "then"   or
        w = "there"   or
        w = "so"          )

   then
      kill := true;

   end if;

   if
     (pofs /= pron) and then
     (pofs /= pack) and then
     (

      --  Pronouns and indefinites
      w = "one"   or
        w = "ones"   or
        w = "he"   or
        w = "any"   or
        w = "anyone"   or
        w = "anything"   or
        w = "each"   or
        w = "every"   or
        w = "other"   or
        w = "you"   or
        w = "who"   or
        w = "whatever"   or
        w = "oneself"   or
        w = "self"   or
        w = "all"   or
        w = "it"   or
        w = "this"   or
        w = "she"   or
        w = "such"   or
        w = "what"   or
        w = "which"   or
        w = "that"   or
        w = "same"    )  then
      kill := true;

   end if;

   if (
             w = "kind"   or
               w = "manner"   or
               w = "variety"   or

               --  Posessives
               w = "its"   or
               w = "own"   or
               w = "his"   or
               w = "ones"  or
               w = "one's" or

               w = "pertaining"   or
               w = "belonging"   or
               w = "containing"   or
               w = "consisting"   or
               w = "relating"   or
               w = "resembling"   or
               w = "abounding"   or
               w = "concerned"   or
               w = "producing"   or
               w = "connected"   or
               w = "made"   or
               w = "used"   or
               w = "having"
      ) then
      kill := true;

   end if;

   if
     (pofs /= v) and then
     (--  Verbs
      w = "take"   or
        w = "make"  or
        w = "go"   or --      !!
        w = "bring"   or
        w = "cut"   or
        w = "put"   or
        w = "set"   or
        w = "grow"   or
        w = "give"   or
        w = "cause"   or
        w = "turn"   or
        w = "fall"   or
        w = "hold"   or
        w = "keep"   or
        w = "construct"   or
        w = "throw"   or
        w = "lay"   or
        w = "remove"   or
        w = "produce"   or
        w = "use"   or
        w = "order"   or
        w = "provide"   or

        w = "being"   or
        w = "making"   or
        w = "lacking"     )

   then
      kill := true;

   end if;

   if

     --  Compounding verbs
     w = "have"  or
     w = "has"   or
     w = "had"   or
     w = "was"   or
     w = "be"    or
     w = "become"   or
     w = "can"   or
     w = "do"   or
     w = "may"   or
     w = "must"   or
     w = "let"   or

     --  Supporting verbs
     w = "is"   or
     w = "been"   or
     --W = "attempt"   or
     w = "begin"                    --or

   then
      kill := true;

   end if;

   if kill then
      for i in w'range  loop
         w(i) := '\';
      end loop;
   end if;

end weed;
