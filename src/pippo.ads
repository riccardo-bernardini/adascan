with Ada.Containers.Indefinite_Holders;

package pippo is
   pragma SPARK_Mode (On);

   type Gigi is private;
private
   pragma SPARK_Mode (Off);

   package Zorro is
     new Ada.Containers.Indefinite_Holders (String);

   type Gigi is
      record
         S : Zorro.Holder;
      end record;
end pippo;
