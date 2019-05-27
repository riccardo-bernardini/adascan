pragma SPARK_Mode;

with Text_Scanners.Regexps;
with Text_Scanners.Basic_Generic_Scanner;
with Text_Scanners.Post_Processors;

-----------
-- Prova --
-----------

package Prova is
   type Zorro is (A, B, C);
   type Q is array (Zorro) of Text_Scanners.Regexps.Regexp;
   type AA is  array (Zorro) of Text_Scanners.Post_Processors.Post_Processor;

   package Minnie is
     new Text_Scanners.Basic_Generic_Scanner (Token_Type           => Zorro,
                                              Regexp_Array         => Q,
                                              Post_Processor_Array => Aa);
end Prova;
