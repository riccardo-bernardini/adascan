with Ada.Containers.Indefinite_Holders;
with Gnat.Regpat;

use type Gnat.Regpat.Pattern_Matcher;

package Text_Scanners.Regexp_Holders   is
     new Ada.Containers.Indefinite_Holders (Gnat.Regpat.Pattern_Matcher);
