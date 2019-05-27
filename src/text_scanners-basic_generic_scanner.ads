pragma SPARK_Mode (On);

with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Maps;


with Text_Scanners.Regexps;
with Text_Scanners.Post_Processors;

--  private
generic
   type Token_Type is (<>);
   type Regexp_Array is array (Token_Type) of Regexps.Regexp;
   type Post_Processor_Array is array (Token_Type) of Post_Processors.Post_Processor;
package Text_Scanners.Basic_Generic_Scanner is
   type Basic_Scanner (<>) is tagged limited private;


   function Create (Input            : String;
                    Token_Regexps    : Regexp_Array;
                    Comment_Delim    : Text_Scanners.Regexps.Comment_Specs;
                    Post_Processing  : Post_Processor_Array)
                    return Basic_Scanner;

   procedure Next (Scanner : in out Basic_Scanner);

   function Current_Token (Scanner : Basic_Scanner) return Token_Type;


   function Current_Value (Scanner : Basic_Scanner) return String;

   function At_EOF (Scanner : Basic_Scanner) return Boolean;
private
   procedure Skip_At_EOF (Scanner : in out Basic_Scanner)
     with
       Post => Scanner.At_EOF;

--     procedure Save_Current_Token_In_History (Scanner : in out Basic_Scanner);


   type History_Entry is
      record
         Token : Token_Type;
         Value : Unbounded_String;
      end record;

   type History_Array is
     array (Natural range <>) of History_Entry;

   type Basic_Scanner (Size : Positive) is tagged limited
      record
         Regexp_Table    : Regexp_Array;
         Input           : String (1 .. Size);
         Cursor          : Positive;
         On_Eof          : Token_Type;
         On_EOF_Valid    : Boolean;
         Current_Token   : Token_Type;
         String_Value    : Unbounded_String;
         Whitespace      : Ada.Strings.Maps.Character_Set;
         Post_Processing : Post_Processor_Array;
         Comment_Style   : Regexps.Comment_Specs;
         First_Scan_Done : Boolean;
      end record;


   function Current_Token (Scanner : Basic_Scanner) return Token_Type
   is (Scanner.Current_Token);


   function Current_Value (Scanner : Basic_Scanner) return String
   is (To_String (Scanner.String_Value));

   function At_EOF (Scanner : Basic_Scanner) return Boolean
   is (Scanner.Cursor > Scanner.Input'Last);


end Text_Scanners.Basic_Generic_Scanner;
