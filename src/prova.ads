pragma SPARK_Mode;

with Generic_Scanner;

-----------
-- Prova --
-----------

package Prova is
   type Zorro is (A, B, C);
   package Pluto is
     new Generic_Scanner (Zorro);
end Prova;
