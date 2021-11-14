with Ada.Strings.Maps;

private with Gnat.Regpat;

package Text_Scanners.Regexps is
   pragma SPARK_Mode (On);

   use Ada;
   use type Ada.Strings.Maps.Character_Set;


   type Regexp is  private;

   type Language_Style is (C_Style, Ada_Style);

   type Charset_Spec is new String;

   function To_Character_Set(X:Charset_Spec)
                             return Strings.Maps.Character_Set;

   Eof_Regexp : constant Regexp;
   -- Special regexp that matches nothing and it is used to mark
   -- the token used to represent the end of input

   function Is_Eof(X : Regexp) return Boolean;

   function ID_Regexp (Style : Language_Style) return Regexp;
   --  Return a regexp matching an identifier, according to the
   --  specified style

   function ID_Regexp (Begin_ID_Chars : Charset_Spec := "a-zA-Z_";
                       Body_ID_Chars  : Charset_Spec := "a-zA-Z0-9_")
                       return Regexp;
   --  Return a regexp matching an identifier that begins
   --  with a character in Begin_ID_Chars followed by any sequence of
   --  characters in Body_ID_Chars.

   function Disjoint_Charsets(X, Y: Charset_Spec) return Boolean
   is ((To_Character_Set(X) and To_Character_Set(Y))=Strings.Maps.Null_Set);

   function Sectioned_ID (Section_Delimiters : Charset_Spec;
                          Begin_ID_Chars : Charset_Spec := "a-zA-Z_";
                          Body_ID_Chars  : Charset_Spec := "a-zA-Z0-9_")
                          return Regexp
     with
       Pre =>
         Disjoint_Charsets(Section_Delimiters, Begin_ID_Chars)
         and Disjoint_Charsets(Section_Delimiters, Body_ID_Chars);

   -- Return a regexp matching a "sectioned ID." A sectioned ID
   -- is like an identifier, but it can be "split" in sections
   -- separated by characters in Section_Delimiters. The ID cannot
   -- begin with any Section_Delimiters and it is not possible to
   -- have two consecutive section delimiters, much like '_' in
   -- Ada identifiers.
   --
   -- For example, this returns a regexp matching an Ada-like
   -- identifier
   --
   --    Sectioned_ID("_", "a-zA-Z", "a-zA-Z0-9")
   --
   -- This, instead, matches "record component"-like identifiers
   --
   --    Sectioned_ID(".", "a-zA-Z", "a-zA-Z0-9_")
   --


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
   use Gnat;

   type Regexp(Is_Eof_Regexp: Boolean := False) is
      record
         case Is_Eof_Regexp is
            when False=>
               Matcher : Regpat.Pattern_Matcher(Gnat.Regpat.Max_Program_Size);

            when True=>
               null;
         end case;
      end record;

   type Match_Data is
      record
         Data : Regpat.Match_Array(0..0);
      end record;

   Eof_Regexp : constant Regexp := (Is_Eof_Regexp => True);
end Text_Scanners.Regexps;
