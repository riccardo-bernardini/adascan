--                              -*- Mode: Ada -*-
--  Filename        : test_report.ads
--  Description     : Handy package to print test results
--  Author          : Riccardo Bernardini
--  Created On      : Thu Jul  3 07:49:02 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Beta

--
-- This package provides some procedures which can be handy
-- when writing test code.  Our reference model is the
-- following: a test program contains several "test suites",
-- where each suite is just a collection of sets.  A suite
-- is SUCCESSful if all the tests in the suite passed.
--
-- A variable of type Reporter_Type is an object which can keep
-- track of the passed/not passed test/suites.  The way to
-- use a Reporter_Type is quite simple:
--
--   Reporter : Reporter_Type;               -- Create the object
--
--   while ... loop                          -- first test suite
--
--     new_result(Rep => reporter,           -- Accumulate test
--                OK => expected = actual);  -- results
--   end loop;
--
--   new_suite(reporter);                    -- start a new suite
--
--   while ... loop                          -- second test suite
--
--     if (OK) then
--       success(reporter);                  -- An alternative way
--     else                                  -- of accumulating
--       failure(reporter);                  -- test results
--     end if;
--   end loop;
--
--  final(reporter);                         -- Print final report
--                                           -- and set Exit_Status
--
--
-- Each time New_Suite is called it prints on the stdout
-- a string of type
--
--       Passed ... out of ... tests: (SUCCESS | FAILED)
--


with Ada.Command_Line;

with Ada.Strings.Unbounded, Text_Io;
use  Ada.Strings.Unbounded, Text_Io;

package Test_Report is
   package CL renames Ada.Command_Line;

   type Reporter_Type is tagged private;

   procedure Be_Verbose (This : in out Reporter_Type;
                         Flag : in     Boolean := True);
   --
   -- By default the reports are printed to the standard error.  This
   -- procedure allows one to change such a default.
   --
   procedure Set_Output (This : in out Reporter_Type;
                         File : in     File_Access);

   procedure Set_Tab (This : in out Reporter_Type;
                      Tab  : in     Positive);


   --
   -- Start a new suite.  It is possible to give to the suite
   -- a descriptive name.  This function implicitly closes the
   -- current suite and prints the corresponding results.
   --
   procedure New_Suite (This : in out Reporter_Type;
                        Name : in     String := "");


   --
   -- Register the result of a new test.
   --
   procedure New_Result (This : in out Reporter_Type;
                         Ok   : in     Boolean);

   --
   -- Equivalent to New_Result(This, True);
   --
   procedure Success (This : in out Reporter_Type);

   --
   -- Equivalent to New_Result(This, False);
   --
   procedure Failure (This : in out Reporter_Type);

   --
   -- Print a final report and, if required, sets the
   -- exit status to Success if and only if all the
   -- tests succeded.
   --
   procedure Final (This       : in out Reporter_Type;
                    Set_Status : in     Boolean := True);


   --
   -- Generic procedure to run an "array of tests". Typically
   -- Test_Case will be a record which holds the values necessary
   -- for the tests.  For example, in order to test a "sum" function
   -- one could write
   --
   --    type Sum_Case is
   --      record
   --        Left, Right : Integer;
   --        Expected    : Integer;
   --      end record;
   --
   --    type Sum_Case_Array is
   --        array(positive range <>) of Sum_Case;
   --
   --    Cases : Sum_Case_Array := ((left => 4, right => 3, result => 7),
   --                               (left => 2, right => 3, result => 5),
   --                               (left => 3, right => 5, result => 8));
   --
   --    function Check_Sum(X : Sum_Case)
   --                      return Boolean is
   --    begin
   --      return (Sum(X.Left, X.Right) = X.Result);
   --    end Check_Sum;
   --
   -- Finally, the procedure would be instantiated
   --
   --    procedure Sum_Test is
   --       new Do_Suite (Test_Case       => Sum_Case,
   --                     Test_Case_Array => Sum_Case_Array,
   --                     Check           => Check_Sum);
   --
   -- and run as
   --
   --    Sum_Test(Reporter, Cases);
   --
   generic
      type Test_Case is private;
      type Test_Case_Array is
        array(Positive range <>) of Test_Case;

      with function Check(This_Case : Test_Case) return Boolean;
   procedure Do_Suite (This  : in out Reporter_Type;
                       Cases : in     Test_Case_Array;
                       Name  : in     String := "");

private
   type Reporter_Type is tagged
      record
         Status     : CL.Exit_Status := CL.Success;
         N_Suites   : Natural     := 0;
         N_Suite_OK : Natural     := 0;

         N_Tests    : Natural     := 0;
         N_Test_OK  : Natural     := 0;

         Name       : Unbounded_String;

         Output_To  : File_Access := Standard_Error;

         Verbose    : Boolean     := False;

         Tab        : Positive    := 35;
      end record;
end Test_Report;

-- Maybe Obsolete --
--   procedure Do_Report (This        : in out Reporter_Type;
--                        Num_Trials  : in     Positive;
--                        Num_Success : in     Natural;
--                        Name        : in     String  := "";
--                        Set_Status  : in     Boolean := True);
