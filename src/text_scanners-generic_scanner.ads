pragma SPARK_Mode;

with Ada.Finalization;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Ada.Containers.Indefinite_Holders;

with Text_Scanners.Regexps;


---------------------
-- Generic_Scanner --
---------------------

generic
   type Token_Type is (<>);
package Text_Scanners.Generic_Scanner with SPARK_Mode => On  is
   type Scanner_Type (<>) is tagged private;

   type Token_Regexp_Array is array (Token_Type) of Regexps.Regexp;
   --  A token regexp array maps each token into the corresponding regexp

   type Token_Array is array (Positive range <>) of Token_Type;

   type String_Preprocessor is interface;

   function Process (Handler : String_Preprocessor;
                     Class   : Token_Type;
                     Value   : String)
                     return String
                     is abstract;

   type Trivial_Processor is new String_Preprocessor with null record;

   function Process (Handler : Trivial_Processor;
                     Class   : Token_Type;
                     Value   : String)
                     return String
   is (Value);

   No_Processing : constant Trivial_Processor;-- := Trivial_Processor'(others => <>);

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

   function Single_Delimeter_Comments (Start : String) return Comment_Specs;

   function Double_Delimeter_Comments (Start, Stop : String) return Comment_Specs;

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


   function New_Scanner (Input          : String;
                         Regexps        : Token_Regexp_Array;
                         History_Size   : Positive := 1024;
                         Comment_Delim  : Comment_Specs := No_Comment;
                         Callback       : String_Preprocessor'Class := No_Processing;
                         Scan           : Boolean := True)
                         return Scanner_Type
     with SPARK_Mode;

   procedure Expect (Scanner   : in out Scanner_Type;
                     Expected  : Token_Type);
   --  If the current token is equal to Expected, eat it; otherwise
   --  raise Unmatched_Token.

   procedure Expect (Scanner  : in out Scanner_Type;
                     Expected : Token_Array);
   --  If the current token is in Expected, eat it; otherwise
   --  raise Unmatched_Token.


   function Eat_If_Equal (Scanner  : in out Scanner_Type;
                          Expected : Token_Type)
                          return Boolean;
   --  If the current token is equal to Expected, eat it and return True;
   --  otherwise return false.  As funny as it may seem, experience
   --  shows that it is very useful.


   function Next_Token (Scanner : in out Scanner_Type) return Token_Type;
   --  Parse the next token and return it

   procedure Next (Scanner : in out Scanner_Type);
   --  Parse a new token

   function Current_Token (Scanner : Scanner_Type) return Token_Type;

   function String_Value (Scanner : Scanner_Type) return String;

   function At_EOF (Scanner : Scanner_Type) return Boolean;

   Unrecognized_Token : exception;
   Unmatched_Token    : exception;
   Unexpected_EOF     : exception;


private
   type Comment_Style_Type is (Void, End_At_EOL, End_Delimeter);

   type Comment_Specs (Style : Comment_Style_Type := Void)  is
      record

         Start : Unbounded_String;

         case Style is
            when Void | End_At_EOL =>
               null;

            when End_Delimeter =>
               Stop  : Unbounded_String;
         end case;
      end record;
   --       with
   --         Predicate => (if Style = Void then Start = Ada.Strings.Unbounded.Null_Unbounded_String);

   No_Comment : constant Comment_Specs := Comment_Specs'(Style => Void,
                                                         Start => Null_Unbounded_String);

   --  type Matcher_Access is
   --    access Regexps.Pattern_Matcher;

   type History_Entry is
      record
         Token : Token_Type;
         Value : Unbounded_String;
      end record;

   --     type Comment_Config_Access is
   --       access Comment_Config;

   type History_Array is
     array (Natural range <>) of History_Entry;

   package Callback_Holder is
     new Ada.Containers.Indefinite_Holders (String_Preprocessor'Class);


   type Scanner_Type (Size         : Positive;
                      History_Size : Positive) is
     tagged
      record
         Regexp_Table    : Token_Regexp_Array;
         Input           : String (1 .. Size);
         Cursor          : Positive;
         On_Eof          : Token_Type;
         On_EOF_Valid    : Boolean;
         Current_Token   : Token_Type;
         String_Value    : Unbounded_String;
         Whitespace      : Ada.Strings.Maps.Character_Set;
         History         : History_Array (0 .. History_Size);
         History_Cursor  : Natural;
         Callbacks       : Callback_Holder.Holder;
         Comment_Style   : Comment_Specs;
         First_Scan_Done : Boolean;
      end record;


   procedure Skip_At_EOF (Scanner : in out Scanner_Type);

   procedure Save_Current_Token_In_History (Scanner : in out Scanner_Type);

   function Unprocessed_Part (Scanner : Scanner_Type) return String
   is (Scanner.Input (Scanner.Cursor .. Scanner.Input'Last));

   No_Processing : constant Trivial_Processor :=
     Trivial_Processor'(others => <>);

end Text_Scanners.Generic_Scanner;
