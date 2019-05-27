pragma SPARK_Mode;

with Ada.Finalization;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Maps;

with Text_Scanners.Regexps;
with Text_Scanners.Post_Processors;

private with Text_Scanners.Basic_Generic_Scanner;

---------------------
-- Generic_Scanner --
---------------------

generic
   type Token_Type is (<>);
package Text_Scanners.Generic_Scanner with SPARK_Mode => On  is
   type Scanner_Type (<>) is new
     Ada.Finalization.Limited_Controlled
   with
     private;


   type Token_Regexp_Array is array (Token_Type) of Regexps.Regexp;
   --  A token regexp array maps each token into the corresponding regexp

   type Token_Array is array (Positive range <>) of Token_Type;

   type Post_Processor_Array is array (Token_Type) of Post_Processors.Post_Processor;

   No_Processing : constant Post_Processor_Array := (others => Post_Processors.No_Processing);

   --  After recognizing a token, the scanner can call a "post-processing"
   --  function whose duty is to modify the token "value" (i.e., the
   --  text representation of the token).  For example, the callback associated
   --  to the "string" token could remove the quotes and do escape replacement
   --  (i.e., replacing strings like \n or %20).
   --  A Post_Processor_Array maps each token into the corresponding
   --  post-processing processor or null, if no pre-processing is required.

   function New_Scanner (Input          : String;
                         Token_Regexps  : Token_Regexp_Array;
                         History_Size   : Positive := 1024;
                         Comment_Delim  : Regexps.Comment_Specs := Regexps.No_Comment;
                         Callbacks      : Post_Processor_Array := No_Processing;
                         Scan           : Boolean := True)
                         return Scanner_Type;

   procedure Expect (Scanner   : Scanner_Type;
                     Expected  : Token_Type);
   --  If the current token is equal to Expected, eat it; otherwise
   --  raise Unmatched_Token.

   procedure Expect (Scanner  : Scanner_Type;
                     Expected : Token_Array);
   --  If the current token is in Expected, eat it; otherwise
   --  raise Unmatched_Token.


   function Eat_If_Equal (Scanner  : Scanner_Type;
                          Expected : Token_Type)
                          return Boolean;
   --  If the current token is equal to Expected, eat it and return True;
   --  otherwise return false.  Although it can seem funny, it is very
   --  convenient.


   function Next_Token (Scanner : Scanner_Type) return Token_Type;
   --  Parse the next token and return it.  Syntactic sugar for
   --  the sequence of Next and Current_Token. Very convenient.

   procedure Next (Scanner : Scanner_Type);
   --  Parse a new token

   function Current_Token (Scanner : Scanner_Type) return Token_Type;

   function String_Value (Scanner : Scanner_Type) return String;

   function At_EOF (Scanner : Scanner_Type) return Boolean;
private

   pragma SPARK_Mode (Off);

   package Basic_Scan is
     new Text_Scanners.Basic_Generic_Scanner (Token_Type           => Token_Type,
                                              Regexp_Array         => Token_Regexp_Array,
                                              Post_Processor_Array => Post_Processor_Array);



   type Scanner_Access is access Basic_Scan.Basic_Scanner;

   type Scanner_Type is new
     Ada.Finalization.Limited_Controlled
   with
      record
         S : Scanner_Access;
      end record;

   overriding procedure Finalize (Object : in out Scanner_Type);

end Text_Scanners.Generic_Scanner;

--     type True_Scanner_Type (Size         : Positive;
--                             History_Size : Positive) is
--       new Ada.Finalization.Limited_Controlled
--     with
--        record
--           Regexp_Table    : Regexp_Array;
--           Input           : String (1 .. Size);
--           Cursor          : Positive;
--           On_Eof          : Token_Type;
--           On_EOF_Valid    : Boolean;
--           Current_Token   : Token_Type;
--           String_Value    : Unbounded_String;
--           Whitespace      : Ada.Strings.Maps.Character_Set;
--           History         : History_Array (0 .. History_Size);
--           History_Cursor  : Natural;
--  --           Callbacks       : Callback_Holder.Holder;
--           Comment_Style   : Comment_Specs;
--           First_Scan_Done : Boolean;
--        end record;
--
--     procedure Initialize (Item   : in out True_Scanner_Type;
--                           Tokens : Token_Regexp_Array);
--
--     overriding procedure Finalize (Item : in out True_Scanner_Type);

--     type String_Preprocessor is interface;
--
--     function Process (Handler : String_Preprocessor;
--                       Class   : Token_Type;
--
--                       Value   : String)
--                       return String
--                       is abstract;
--
--     type Trivial_Processor is new String_Preprocessor with null record;
--
--     function Process (Handler : Trivial_Processor;
--                       Class   : Token_Type;
--                       Value   : String)
--                       return String
--     is (Value);
