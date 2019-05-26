with Ada.Finalization;

private with Gnat.Regpat;

use Ada;

package Text_Scanners.Regexps is
   pragma SPARK_Mode (On);

   type Regexp is new Finalization.Limited_Controlled with private;

   type Language_Style is (C_Style, Ada_Style);

   function ID_Regexp (Additional_ID_Chars : String := "_";
                       Basic_ID_Chars      : String := "a-zA-Z0-9";
                       Begin_ID_Chars      : String := "a-zA-Z")
                       return Regexp;

   function ID_Regexp (Style : Language_Style) return Regexp;

   function Number_Regexp (Style : Language_Style := Ada_Style) return Regexp;

   function Float_Regexp (Style : Language_Style := Ada_Style)  return Regexp;

   function String_Regexp (Quote_Char : Character) return Regexp;

   function String_Regexp (Style : Language_Style := Ada_Style) return Regexp;


   function Fixed_String (Str : String) return Regexp;

private
   pragma SPARK_Mode (Off);

   type Matcher_Access is access Gnat.Regpat.Pattern_Matcher;

   type Regexp is new Finalization.Limited_Controlled
     with
      record
         Matcher : Matcher_Access;
      end record;
end Text_Scanners.Regexps;
