pragma SPARK_Mode;

with Text_Scanners.Regexps;

-----------
-- Prova --
-----------

package Prova is
   type Zorro is (A, B, C);
   type Topo is array (Positive range <>) of Text_Scanners.Regexps.Regexp;

   X : constant Topo := (1 => Text_Scanners.Regexps.ID_Regexp,
                         2 => Text_Scanners.Regexps.Number_Regexp);

end Prova;
