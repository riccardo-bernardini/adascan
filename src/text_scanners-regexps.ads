with Ada.Finalization;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Holders;
with Text_Scanners.Regexp_Holders;
private with Gnat.Regpat;

use Ada;

package Text_Scanners.Regexps is
   pragma SPARK_Mode (On);

   type Regexp is tagged private;

   type Match_Result is
      record
         First : Natural := 0;
         Last  : Natural := 0;
      end record;

   No_Match : constant Match_Result := (0, 0);

   function Match (Self : Regexp;
                   Data : String)
                   return Match_Result
     with
       Post =>
         Match'Result = No_Match or
         (Match'Result.Last >= Match'Result.First
          and Match'Result.First >= Data'First
          and Match'Result.Last <= Data'Last);


   function Is_Eof_Regexp (X : Regexp) return Boolean;

   function EOF return Regexp
     with
       Post => Is_Eof_Regexp (EOF'Result);

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

   function Parse (Str : String) return Regexp;

   type Comment_Specs is private;
   --  The scanner can be programmed to recognize different types of "comment
   --  styles."  Currently it recognizes single delimited comments that
   --  end at the end of line (e.g., Shell, C++, Ada) or doubly delimited
   --  comments (e.g., C).  The delimiter description is given in a
   --  string of type Comment_Specs.
   --  Comment_Specs strings can be created using the functions
   --  Single_Delimeter_Comments and Double_Delimeter_Comments.

   No_Comment : constant Comment_Specs;
   --  Special specs used for the "no comment" case.  If this is used,
   --  the scanner does not recognize any type of comment.

   type Comment_Format is (Void, End_At_EOL, End_At_Delimiter);

   function Format (Spec : Comment_Specs) return Comment_Format;

   function Single_Delimeter_Comments (Start : String) return Comment_Specs
     with
       Post => Format (Single_Delimeter_Comments'Result) = End_At_EOL;

   function Double_Delimeter_Comments (Start, Stop : String) return Comment_Specs
     with
       Post => Format (Double_Delimeter_Comments'Result) = End_At_Delimiter;

   function Comment_Start (Specs : Comment_Specs) return String
     with
       Pre => Format (Specs) /= Void;

   function Comment_End (Specs : Comment_Specs) return String
     with
       Pre => Format (Specs) = End_At_Delimiter;

   type Comment_Style is
     (
      Shell_Like,        --  Begin at '#' ends at end-of-line
      Ada_Like,          --  Begin at '--' ends at end-of-line
      LaTeX_Like,        --  Begin at '%' ends at end-of-line
      C_Like,            --  Begin at '/*' ends at '*/'
      C_Plus_Plus_Like,  --  Begin at '//' ends at end-of-line
      Asm_Like           --  Begin at '//' ends at end-of-line
     );
   --  Used together with the function Comment_Like to create some very
   --  common comment conventions

   function Comment_Like (Style : Comment_Style) return Comment_Specs;


private
   pragma Warnings (Off, "no Global Contract available");

   use type GNAT.Regpat.Pattern_Matcher;

--     package Regexp_Holders is
--       new Ada.Containers.Indefinite_Holders (Gnat.Regpat.Pattern_Matcher);

   type Regexp is tagged
      record
         R : Regexp_Holders.Holder;
      end record;


   type Comment_Specs (Format : Comment_Format := Void)  is
      record

         Start : Unbounded_String;

         case Format is
            when Void | End_At_EOL =>
               null;

            when End_At_Delimiter =>
               Stop  : Unbounded_String;
         end case;
      end record
     with
       Predicate => (if Format = Void then Start = Null_Unbounded_String);

   No_Comment : constant Comment_Specs := Comment_Specs'(Format => Void,
                                                         Start  => Null_Unbounded_String);

   function Is_Eof_Regexp (X : Regexp) return Boolean
   is (X.R.Is_Empty);

   function EOF return Regexp
   is (Regexp'(R => Regexp_Holders.Empty_Holder));

   function Format (Spec : Comment_Specs) return Comment_Format
   is (Spec.Format);

   function Comment_Start (Specs : Comment_Specs) return String
   is (To_String (Specs.Start));

   function Comment_End (Specs : Comment_Specs) return String
   is (To_String (Specs.Stop));

end Text_Scanners.Regexps;
