with Ada.Finalization;

private with Gnat.Regpat;

use Ada;

package Text_Scanners.Regexps is
   pragma SPARK_Mode (On);

   type Regexp is  private;

   type Language_Style is (C_Style, Ada_Style);

   Eof_Regexp : constant Regexp;
   -- Special regexp that matches nothing and it is used to mark
   -- the token used to represent the end of input

   function Is_Eof(X : Regexp) return Boolean;

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

   function Compile(Regexp_Spec : String) return Regexp;

   type Match_Data is private;


   function Has_Matched(Item :Match_Data) return Boolean;

   function Get_Matched (Source : String;
                         Matching : Match_Data)
                         return String
     with Pre => Has_Matched(Matching);

   function Last_Matched (Matching: Match_Data) return Positive
     with Pre => Has_Matched(Matching);

   function First_Matched (Matching: Match_Data) return Positive
     with Pre => Has_Matched(Matching);

   procedure Match(Matcher  : Regexp;
                   Matching : out Match_Data;
                   Input    : String);
private
   type Regexp is
      record
         Is_Eof_Regexp : Boolean;
         Matcher : Gnat.Regpat.Pattern_Matcher(Gnat.Regpat.Max_Program_Size);
      end record;

   type Match_Data is
      record
         Data : Gnat.Regpat.Match_Array(0..0);
      end record;

   Eof_Regexp : constant Regexp := (Is_Eof_Regexp => True,
                                    Matcher => Gnat.Regpat.Never_Match);
end Text_Scanners.Regexps;
