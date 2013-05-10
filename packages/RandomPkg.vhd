--
--  File Name:         RandomPkg.vhd
--  Design Unit Name:  RandomPkg
--  Revision:          STANDARD VERSION,  revision 2013.04
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis      email:  jim@synthworks.com
--     *
--
--   * In writing procedures normal, poisson, the following sources were referenced:
--     Wikipedia
--     package rnd2 written by John Breen and Ken Christensen
--     package RNG written by Gnanasekaran Swaminathan
--
--
--  Description:
--    RandomPType, a protected type, defined to hold randomization RandomSeeds and
--    function methods to facilitate randomization with uniform and weighted
--    distributions
--
--  Developed for:
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--  Revision History:
--    Date       Version    Description
--    12/2006:   0.1        Initial revision
--                          Numerous revisions for VHDL Testbenches and Verification
--    02/2009:   1.0        First Public Released Version
--    02/25/2009 1.1        Replaced reference to std_2008 with a reference to
--                          ieee_proposed.standard_additions.all ;
--    06/2010    1.2        Added Normal and Poisson distributions
--    03/2011    2.0        Major clean-up.
--                          Moved RandomParmType and control to here
--    07/2011    2.1        Bug fix to convenience functions for slv, unsigned, and signed.
--    06/2012    2.2        Removed '_' in the name of subprograms FavorBig and FavorSmall
--                          to make more consistent with other subprogram names
--    04/2013    2013.04    Changed DistInt
--                             Now returns input array range.
--                             For literals, no impact. It still returns 0 to N-1 (the default array range)
--                             Impacts named constants, signals, or variables.
--                             Added error checking to weight values
--                          Better Min, Max error handling in Uniform, FavorBig, FavorSmall, Normal, Poisson
--
--
--  Copyright (c) 2006 - 2013 by SynthWorks Design Inc.  All rights reserved.
--
--  Verbatim copies of this source file may be used and
--  distributed without restriction.
--
--  This source file is free software; you can redistribute it
--  and/or modify it under the terms of the ARTISTIC License
--  as published by The Perl Foundation; either version 2.0 of
--  the License, or (at your option) any later version.
--
--  This source is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the Artistic License for details.
--
--  You should have received a copy of the license with this source.
--  If not download it from,
--     http://www.perlfoundation.org/artistic_license_2_0
--

use work.RandomBasePkg.all ;
use work.SortListPkg_int.all ;

use std.textio.all ;

library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;
use ieee.math_real.all ;

-- comment out following 3 lines with VHDL-2008.  Leave in for VHDL-2002 
-- library ieee_proposed ;						                -- remove with VHDL-2008
-- use ieee_proposed.standard_additions.all ;         -- remove with VHDL-2008
-- use ieee_proposed.standard_textio_additions.all ;  -- remove with VHDL-2008


package RandomPkg is
--  Uncomment the following with VHDL-2008 package generics.
--  For now they are defined in the package RandomBasePkg.vhd
--  package RandomGenericPkg is
  --  generic (
  --    type RandomSeedType ;      -- base type for randomization
  --    procedure Uniform (Result : out real ;  Seed : inout RandomSeedType) ;
  --    function  GenRandSeed(IV : integer_vector) return RandomSeedType ;
  --    function  GenRandSeed(I : integer) return RandomSeedType ;
  --    function  GenRandSeed(S : string) return RandomSeedType ;
  --  ) ;

  
  constant NULL_INTV : integer_vector (0 downto 1) := (others => 0);

  -- Supports DistValInt functionality
  type DistRecType is record
    Value  : integer ;
    Weight : integer ;
  end record ;
  type DistType is array (natural range <>) of DistRecType ;


  -- Parameters for randomization
  -- RandomDistType specifies the distribution to use for randomize
  type RandomDistType is (NONE, UNIFORM, FAVOR_SMALL, FAVOR_BIG, NORMAL, POISSON) ;

  type RandomParmType is record
    Distribution : RandomDistType ;
    Mean         : Real ;   -- also used as probability of success
    StdDeviation : Real ;   -- also used as number of trials for binomial
  end record ;

  -- RandomParm IO
  function  to_string(A : RandomDistType) return string ;
  procedure write(variable L: inout line ; A : RandomDistType ) ;
  procedure read(variable L: inout line ; A : out RandomDistType ; good : out boolean ) ;
  procedure read(variable L: inout line ; A : out RandomDistType ) ;
  function  to_string(A : RandomParmType) return string ;
  procedure write(variable L: inout line ; A : RandomParmType ) ;
  procedure read(variable L: inout line ; A : out RandomParmType ; good : out boolean ) ;
  procedure read(variable L: inout line ; A : out RandomParmType ) ;


  type RandomPType is protected
    -- Seed Manipulation
    -- Known ambiguity between InitSeed with string and integer_vector
    -- Recommendation, use:  RV.InitSeed(RV'instance_path) ;
    -- For integer_vector use either: RV.InitSeed(IV => (1,5)) ;
    --   or: RV.InitSeed(integer_vector'(1,5)) ;
    procedure InitSeed (S  : string ) ;
    procedure InitSeed (I  : integer ) ;
    procedure InitSeed (IV : integer_vector ) ;

    -- SetSeed & GetSeed:  Used to save and restore seed values
    procedure SetSeed (RandomSeedIn : RandomSeedType ) ;
    impure function GetSeed return RandomSeedType ;
    -- SeedRandom = SetSeed & GetSeed for SV compatibility
    -- replace with aliases when they work in popular simulators
    procedure SeedRandom (RandomSeedIn : RandomSeedType ) ;
    impure function SeedRandom return RandomSeedType ;
    -- alias SeedRandom is SetSeed [RandomSeedType] ;
    -- alias SeedRandom is GetSeed [return RandomSeedType] ;

    -- Setting Randomization Parameters
    -- Allows RandInt to have distributions other than uniform
    procedure SetRandomParm (RandomParmIn : RandomParmType) ;
    procedure SetRandomParm (
      Distribution : RandomDistType ;
      Mean         : Real := 0.0 ;
      Deviation    : Real := 0.0
    ) ;
    impure function GetRandomParm return RandomParmType ;
    impure function GetRandomParm return RandomDistType ;

    -- For compatibility with previous version - replace with alias
    procedure SetRandomMode (RandomDistIn : RandomDistType) ;
    -- alias SetRandomMode is SetRandomParm [RandomDistType, Real, Real] ;

    --  Base Randomization Distributions
    -- Uniform:   Generate a random number with a Uniform distribution
    impure function Uniform (Min, Max : in real) return real ;
    impure function Uniform (Min, Max : integer) return integer ;
    impure function Uniform (Min, Max : integer ; Exclude: integer_vector) return integer ;

    -- FavorSmall
    --   Generate random numbers with a greater number of small
    --   values than large values
    impure function FavorSmall (Min, Max : real) return real ;
    impure function FavorSmall (Min, Max : integer) return integer ;
    impure function FavorSmall (Min, Max : integer ; Exclude: integer_vector) return integer ;

    -- FavorBig
    --   Generate random numbers with a greater number of large
    --   values than small values
    impure function FavorBig (Min, Max : real) return real ;
    impure function FavorBig (Min, Max : integer) return integer ;
    impure function FavorBig (Min, Max : integer ; Exclude: integer_vector) return integer ;

    -- Normal:   Generate a random number with a normal distribution
    impure function Normal (Mean, StdDeviation : real) return real ;
    -- Normal + RandomVal >= Min and RandomVal < Max
    impure function Normal (Mean, StdDeviation, Min, Max : real) return real ;
    impure function Normal (
      Mean          : real ;
      StdDeviation  : real ;
      Min           : integer ;
      Max           : integer ;
      Exclude       : integer_vector := NULL_INTV
    ) return integer ;

    -- Poisson:  Generate a random number with a poisson distribution
    --   Discrete distribution = only generates integral values
    impure function Poisson (Mean : real) return real ;
    -- Poisson + RandomVal >= Min and RandomVal < Max
    impure function Poisson (Mean, Min, Max : real) return real ;
    impure function Poisson (
      Mean          : real ;
      Min           : integer ;
      Max           : integer ;
      Exclude       : integer_vector := NULL_INTV
    ) return integer ;


    -- real randomization with a range
    impure function RandReal(Min, Max: Real) return real ;

    --  integer randomization with a range
    impure function RandInt (Min, Max : integer) return integer ;
    impure function RandSlv (Min, Max, Size : natural) return std_logic_vector ;
    impure function RandUnsigned (Min, Max, Size : natural) return Unsigned ;
    impure function RandSigned (Min, Max : integer; Size : natural ) return Signed ;

    --  integer randomization with a range and exclude vector
    impure function RandInt (Min, Max : integer; Exclude: integer_vector ) return integer ;
    impure function RandSlv (Min, Max : natural; Exclude: integer_vector; Size : natural ) return std_logic_vector ;
    impure function RandUnsigned (Min, Max : natural; Exclude: integer_vector ; Size : natural ) return Unsigned ;
    impure function RandSigned (Min, Max : integer; Exclude: integer_vector ; Size : natural ) return Signed ;

    -- Randomly select a value within a set of values
    impure function RandInt ( A : integer_vector ) return integer ;
    impure function RandSlv (A : integer_vector ;  Size : natural) return std_logic_vector  ;
    impure function RandUnsigned (A : integer_vector ;  Size : natural) return Unsigned ;
    impure function RandSigned (A : integer_vector ;  Size : natural ) return Signed ;

    -- Randomly select a value within a set of values with exclude values (so can skip last or last n)
    impure function RandInt ( A : integer_vector; Exclude: integer_vector  ) return integer ;
    impure function RandSlv (A : integer_vector; Exclude: integer_vector;  Size : natural) return std_logic_vector  ;
    impure function RandUnsigned (A : integer_vector; Exclude: integer_vector ;  Size : natural) return Unsigned ;
    impure function RandSigned (A : integer_vector; Exclude: integer_vector ;  Size : natural ) return Signed ;

    -- Randomly select between 0 and N-1 based on the specified weight.
    -- where N = number values in weight array
    impure function DistInt ( Weight : integer_vector ) return integer ;
    impure function DistSlv ( Weight : integer_vector ; Size  : natural ) return std_logic_vector ;
    impure function DistUnsigned ( Weight : integer_vector ; Size  : natural ) return unsigned ;
    impure function DistSigned ( Weight : integer_vector ; Size  : natural ) return signed ;

    -- Distribution with just weights and with exclude values
    impure function DistInt ( Weight : integer_vector; Exclude: integer_vector ) return integer ;
    impure function DistSlv ( Weight : integer_vector; Exclude: integer_vector; Size  : natural ) return std_logic_vector ;
    impure function DistUnsigned ( Weight : integer_vector; Exclude: integer_vector; Size  : natural ) return unsigned ;
    impure function DistSigned ( Weight : integer_vector; Exclude: integer_vector; Size  : natural ) return signed ;

    -- Distribution with weight and value
    impure function DistValInt ( A : DistType ) return integer ;
    impure function DistValSlv ( A : DistType ; Size  : natural) return std_logic_vector ;
    impure function DistValUnsigned ( A : DistType ; Size  : natural) return unsigned ;
    impure function DistValSigned ( A : DistType ; Size  : natural) return signed ;

    -- Distribution with weight and value and with exclude values
    impure function DistValInt ( A : DistType; Exclude: integer_vector ) return integer ;
    impure function DistValSlv ( A : DistType; Exclude: integer_vector; Size  : natural) return std_logic_vector ;
    impure function DistValUnsigned ( A : DistType; Exclude: integer_vector; Size  : natural) return unsigned ;
    impure function DistValSigned ( A : DistType; Exclude: integer_vector; Size  : natural) return signed ;

    -- Convenience Functions
    impure function RandReal return real ;  -- 0.0 to 1.0
    impure function RandReal(Max: Real) return real ;  -- 0.0 to Max
    impure function RandInt (Max : integer) return integer ;
    impure function RandSlv (Size : natural) return std_logic_vector ;
    impure function RandSlv (Max, Size : natural) return std_logic_vector ;
    impure function RandUnsigned (Size : natural) return Unsigned ;
    impure function RandUnsigned (Max, Size : natural) return Unsigned ;
    impure function RandSigned (Size : natural) return Signed ;
    impure function RandSigned (Max : integer;  Size : natural ) return Signed ;

  end protected RandomPType ;

end RandomPkg ;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
package body RandomPkg is

  -----------------------------------------------------------------
  --  Local Randomization Support
  -----------------------------------------------------------------

  -----------------------------------------------------------------
  -- Scale   --   Scale a value to be within a given range
  --
  function Scale (A, Min, Max : real) return real is
    variable ValRange : Real ;
  begin
    ValRange := Max - Min ;
    return A * ValRange + Min ;
  end function Scale ;

  function Scale (A : real ;  Min, Max : integer) return integer is
    variable ValRange : real ;
    variable rMin, rMax : real ;
  begin
    rMin := real(Min) - 0.5 ;
    rMax := real(Max) + 0.5 ;
    ValRange := rMax - rMin ;
    return integer(round(A * ValRange + rMin)) ;
  end function Scale ;

  -- create more smaller values
  function FavorSmall (A : real) return real is
  begin
    return 1.0 - sqrt(A) ;
  end FavorSmall ;

  -- create more larger values
  -- alias FavorBig is sqrt[real return real] ;
  function FavorBig   (A : real) return real is
  begin
    return sqrt(A) ;
  end FavorBig ;


  -----------------------------------------------------------------
  --  RandomParmType IO
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  function to_string(A : RandomDistType) return string is
  begin
    return RandomDistType'image(A) ;
  end function to_string ;


  -----------------------------------------------------------------
  procedure write(variable L: inout line ; A : RandomDistType ) is
  begin
    write(L, to_string(A)) ;
  end procedure write ;


  -----------------------------------------------------------------
  procedure read(variable L: inout line ; A : out RandomDistType ; good : out boolean ) is
    variable strval : string(1 to 40) ;
    variable len    : natural ;
  begin
    -- procedure SREAD (L: inout LINE; VALUE: out STRING; STRLEN: out NATURAL);
    sread(L, strval, len) ;
    A := RandomDistType'value(strval(1 to len)) ;
    good := len > 0 ;
  end procedure read ;


  -----------------------------------------------------------------
  procedure read(variable L: inout line ; A : out RandomDistType ) is
    variable good : boolean ;
  begin
      read(L, A, good) ;
      assert good report "read[line, RandomDistType] failed" severity error ;
  end procedure read ;


  -----------------------------------------------------------------
  function to_string(A : RandomParmType) return string is
  begin
    return RandomDistType'image(A.Distribution) & " " &
           to_string(A.Mean, 2) & " " & to_string(A.StdDeviation, 2) ;
  end function to_string ;


  -----------------------------------------------------------------
  procedure write(variable L: inout line ; A : RandomParmType ) is
  begin
    write(L, to_string(A)) ;
  end procedure write ;


  -----------------------------------------------------------------
  procedure read(variable L: inout line ; A : out RandomParmType ; good : out boolean ) is
    variable strval : string(1 to 40) ;
    variable len    : natural ;
    variable igood  : boolean ;
  begin
    loop
      -- procedure SREAD (L: inout LINE; VALUE: out STRING; STRLEN: out NATURAL);
      sread(L, strval, len) ;
      A.Distribution := RandomDistType'value(strval(1 to len)) ;
      igood := len > 0 ;
      exit when not igood ;

      read(L, A.Mean, igood) ;
      exit when not igood ;

      read(L, A.StdDeviation, igood) ;
      exit ;
    end loop ;
    good := igood ;
  end procedure read ;


  -----------------------------------------------------------------
  procedure read(variable L: inout line ; A : out RandomParmType ) is
    variable good : boolean ;
  begin
      read(L, A, good) ;
      assert good report "read[line, RandomParmType] failed" severity error ;
  end procedure read ;



  -----------------------------------------------------------------
  -----------------------------------------------------------------
  type RandomPType is protected body
    --
    -- RandomSeed manipulation
    --
    variable RandomSeed : RandomSeedType := GenRandSeed(integer_vector'(1,7)) ;

    procedure InitSeed (S : string ) is
    begin
      RandomSeed := GenRandSeed(S) ;
    end procedure InitSeed ;

    procedure InitSeed (I : integer ) is
    begin
      RandomSeed := GenRandSeed(I) ;
    end procedure InitSeed ;

    procedure InitSeed (IV : integer_vector ) is
    begin
      RandomSeed := GenRandSeed(IV) ;
    end procedure InitSeed ;

    procedure SetSeed (RandomSeedIn : RandomSeedType ) is
    begin
      RandomSeed := RandomSeedIn ;
    end procedure SetSeed ;

    procedure SeedRandom (RandomSeedIn : RandomSeedType ) is
    begin
      RandomSeed := RandomSeedIn ;
    end procedure SeedRandom ;

    impure function GetSeed return RandomSeedType is
    begin
      return RandomSeed ;
    end function GetSeed ;

    impure function SeedRandom return RandomSeedType is
    begin
      return RandomSeed ;
    end function SeedRandom ;


    --
    --  randomization mode
    --
    variable RandomParm : RandomParmType ;  -- left most values ok for init

    procedure SetRandomParm (RandomParmIn : RandomParmType) is
    begin
      RandomParm := RandomParmIn ;
    end procedure SetRandomParm ;

    procedure SetRandomParm (
      Distribution : RandomDistType ;
      Mean         : Real := 0.0 ;
      Deviation    : Real := 0.0
    ) is
    begin
      RandomParm := RandomParmType'(Distribution, Mean, Deviation) ;
    end procedure SetRandomParm ;


    impure function GetRandomParm return RandomParmType is
    begin
      return RandomParm ;
    end function GetRandomParm ;


    impure function GetRandomParm return RandomDistType is
    begin
      return RandomParm.Distribution ;
    end function GetRandomParm ;


    -- For compatibility with previous version
    procedure SetRandomMode (RandomDistIn : RandomDistType) is
    begin
      SetRandomParm(RandomDistIn);
    end procedure SetRandomMode ;

    
    --
    --  Base Randomization Distributions
    --
    --
    -- Uniform:   Generate a random number with a Uniform distribution
    --
    impure function Uniform (Min, Max : in real) return real is
      variable rRandomVal : real ;
    begin
      assert (Max >= Min) report "%%RandomPkg Uniform: Max < Min" severity FAILURE ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(rRandomVal, Min, Max) ;
    end function Uniform ;

    impure function Uniform (Min, Max : integer) return integer is
      variable rRandomVal : real ;
    begin
      assert (Max >= Min) report "%%RandomPkg Uniform: Max < Min" severity FAILURE ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(rRandomVal, Min, Max) ;
    end function Uniform ;

    impure function Uniform (Min, Max : integer ; Exclude: integer_vector) return integer is
      variable iRandomVal : integer ;
      variable ExcludeList : SortListPType ;
      variable count : integer ;
    begin
      ExcludeList.add(Exclude, Min, Max) ;
      count := ExcludeList.count ;
      iRandomVal := Uniform(Min, Max - count) ;
      -- adjust count, note iRandomVal changes while checking.
      for i in 1 to count loop
        exit when iRandomVal < ExcludeList.Get(i) ;
        iRandomVal := iRandomVal + 1 ;
      end loop ;
      ExcludeList.erase ;
      return iRandomVal ;
    end function Uniform ;


    --
    -- FavorSmall
    --   Generate random numbers with a greater number of small
    --   values than large values
    --
    impure function FavorSmall (Min, Max : real) return real is
      variable rRandomVal : real ;
    begin
      assert (Max >= Min) report "%%RandomPkg FavorSmall: Max < Min" severity FAILURE ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(FavorSmall(rRandomVal), Min, Max) ;  -- real
    end function FavorSmall ;

    impure function FavorSmall (Min, Max : integer) return integer is
      variable rRandomVal : real ;
    begin
      assert (Max >= Min) report "%%RandomPkg FavorSmall: Max < Min" severity FAILURE ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(FavorSmall(rRandomVal), Min, Max) ;  -- integer
    end function FavorSmall ;

    impure function FavorSmall (Min, Max : integer ; Exclude: integer_vector) return integer is
      variable iRandomVal : integer ;
      variable ExcludeList : SortListPType ;
      variable count : integer ;
    begin
      ExcludeList.add(Exclude, Min, Max) ;
      count := ExcludeList.count ;
      iRandomVal := FavorSmall(Min, Max - count) ;
      -- adjust count, note iRandomVal changes while checking.
      for i in 1 to count loop
        exit when iRandomVal < ExcludeList.Get(i) ;
        iRandomVal := iRandomVal + 1 ;
      end loop ;
      ExcludeList.erase ;
      return iRandomVal ;
    end function FavorSmall ;

    
    --
    -- FavorBig
    --   Generate random numbers with a greater number of large
    --   values than small values
    --
    impure function FavorBig (Min, Max : real) return real is
      variable rRandomVal : real ;
    begin
      assert (Max >= Min) report "%%RandomPkg FavorBig: Max < Min" severity FAILURE ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(FavorBig(rRandomVal), Min, Max) ; -- real
    end function FavorBig ;

    impure function FavorBig (Min, Max : integer) return integer is
      variable rRandomVal : real ;
    begin
      assert (Max >= Min) report "%%RandomPkg FavorBig: Max < Min" severity FAILURE ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(FavorBig(rRandomVal), Min, Max) ; -- integer
    end function FavorBig ;

    impure function FavorBig (Min, Max : integer ; Exclude: integer_vector) return integer is
      variable iRandomVal : integer ;
      variable ExcludeList : SortListPType ;
      variable count : integer ;
    begin
      ExcludeList.add(Exclude, Min, Max) ;
      count := ExcludeList.count ;
      iRandomVal := FavorBig(Min, Max - count) ;
      -- adjust count, note iRandomVal changes while checking.
      for i in 1 to count loop
        exit when iRandomVal < ExcludeList.Get(i) ;
        iRandomVal := iRandomVal + 1 ;
      end loop ;
      ExcludeList.erase ;
      return iRandomVal ;
    end function FavorBig ;


    -----------------------------------------------------------------
    -- Normal
    --   Generate a random number with a normal distribution
    --
    -- Use Box Muller, per Wikipedia:
    --    http://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
    --
    -- Use polar method, per Wikipedia:
    --   http://en.wikipedia.org/wiki/Marsaglia_polar_method
    --
    impure function Normal (Mean, StdDeviation : real) return real is
      variable x01, y01 : real ;
      variable StdNormalDist : real ; -- mean 0, variance 1
    begin
      -- add this check to set parameters?
      if StdDeviation < 0.0 then
        report "standard deviation must be >= 0.0" severity failure ;
        return -1.0 ;
      end if ;

      -- Box Muller
      Uniform (x01, RandomSeed) ;
      Uniform (y01, RandomSeed) ;
      StdNormalDist := sqrt(-2.0 * log(x01)) * cos(math_2_pi*y01) ;

      -- Polar form rejected due to mean 50.0, std deviation = 5 resulted
      -- in a median of 49
      -- -- find two Uniform distributed values with range -1 to 1
      -- -- that satisify S = X **2 + Y**2 < 1.0
      -- loop
        -- Uniform (x01, RandomSeed) ;
        -- Uniform (y01, RandomSeed) ;
        -- x := 2.0 * x01 - 1.0 ;  -- scale to -1 to 1
        -- y := 2.0 * y01 - 1.0 ;
        -- s := x*x + y*y ;
        -- exit when s < 1.0 and s > 0.0 ;
      -- end loop ;

      -- -- Calculate Standard Normal Distribution
      -- StdNormalDist := x * sqrt((-2.0 * log(s)) / s) ;

      -- Convert to have Mean and StdDeviation
      return StdDeviation * StdNormalDist + Mean ;
    end function Normal ;


    -- Normal + RandomVal >= Min and RandomVal <= Max
    impure function Normal (Mean, StdDeviation, Min, Max : real) return real is
      variable rRandomVal : real ;
    begin
      if Max < Min then
         report "%%RandomPkg Normal: Max < Min" severity FAILURE ;
      else
        loop
          rRandomVal := Normal (Mean, StdDeviation) ;
          exit when rRandomVal >= Min and rRandomVal <= Max ;
        end loop ;
      end if ; 
      return rRandomVal ; 
    end function Normal ;

    -- Normal + RandomVal >= Min and RandomVal <= Max
    impure function Normal (
      Mean          : real ;
      StdDeviation  : real ;
      Min           : integer ;
      Max           : integer ;
      Exclude       : integer_vector := NULL_INTV
    ) return integer is
      variable iRandomVal : integer ;
    begin
      if Max < Min then
         report "%%RandomPkg Normal: Max < Min" severity FAILURE ;
      else
        loop
          iRandomVal := integer(round(  Normal(Mean, StdDeviation)  )) ;
          exit when iRandomVal >= Min and iRandomVal <= Max and
                     not inside(iRandomVal, Exclude) ;
        end loop ;
      end if ; 
      return iRandomVal ;
    end function Normal ;


    -----------------------------------------------------------------
    -- Poisson
    --   Generate a random number with a poisson distribution
    --   Discrete distribution = only generates integral values
    --
    -- Use knuth method, per Wikipedia:
    --   http://en.wikipedia.org/wiki/Poisson_distribution
    --
    impure function Poisson (Mean : real) return real is
      variable Product      : Real := 1.0;
      variable Bound        : Real := 0.0;
      variable UniformRand  : Real := 0.0 ;
      variable PoissonRand  : Real := 0.0 ;
    begin
      Bound := exp(-1.0 * Mean);
      Product := 1.0 ;

      -- add this check to set parameters?
      if Mean <= 0.0 or Bound <= 0.0 then
        report "Poisson:  Mean < 0 or too large.  Mean = " & real'image(Mean) severity failure ;
        return -1.0 ;
      end if ;

      while (Product >= Bound) loop
        PoissonRand := PoissonRand + 1.0;
        Uniform(UniformRand, RandomSeed) ;
        Product := Product * UniformRand;
      end loop;
      return PoissonRand ;
    end function  Poisson ;  -- no range

    -- Poisson + RandomVal >= Min and RandomVal < Max
    impure function Poisson (Mean, Min, Max : real) return real is
      variable rRandomVal : real ;
    begin
      if Max < Min then
         report "%%RandomPkg Poisson: Max < Min" severity FAILURE ;
      else
        loop
          rRandomVal := Poisson (Mean) ;
          exit when rRandomVal >= Min and rRandomVal <= Max ;
        end loop ;
      end if ; 
      return rRandomVal ;
    end function  Poisson ;

    impure function Poisson (
      Mean          : real ;
      Min           : integer ;
      Max           : integer ;
      Exclude       : integer_vector := NULL_INTV
    ) return integer is
      variable iRandomVal : integer ;
    begin
      if Max < Min then
         report "%%RandomPkg Poisson: Max < Min" severity FAILURE ;
      else
        loop
          iRandomVal := integer(round(  Poisson (Mean)  )) ;
          exit when iRandomVal >= Min and iRandomVal <= Max and
                     not inside(iRandomVal, Exclude) ;
        end loop ;
      end if ; 
      return iRandomVal ;
    end function  Poisson ;


    --
    --  real randomization with a range
    --    Distribution determined by RandomParm
    --
    impure function RandReal(Min, Max: Real) return real is
    begin
      case RandomParm.Distribution is
        when NONE | UNIFORM =>  return Uniform(Min, Max) ;
        when FAVOR_SMALL  =>    return FavorSmall(Min, Max) ;
        when FAVOR_BIG    =>    return FavorBig (Min, Max) ;
        when NORMAL =>          return Normal(RandomParm.Mean, RandomParm.StdDeviation, Min, Max) ;
        when POISSON =>         return Poisson(RandomParm.Mean, Min, Max) ;
        when others =>
          report "RandomPkg:  distribution not implemented" severity failure ;
          return real(integer'low) ;
      end case ;
    end function RandReal ;


    --
    --  integer randomization with a range
    --    Distribution determined by RandomParm
    --
    impure function RandInt (Min, Max : integer) return integer is
    begin
      case RandomParm.Distribution is
        when NONE | UNIFORM =>  return Uniform(Min, Max) ;
        when FAVOR_SMALL  =>    return FavorSmall(Min, Max) ;
        when FAVOR_BIG    =>    return FavorBig (Min, Max) ;
        when NORMAL =>          return Normal(RandomParm.Mean, RandomParm.StdDeviation, Min, Max) ;
        when POISSON =>         return Poisson(RandomParm.Mean, Min, Max) ;
        when others =>
          report "RandomPkg:  distribution not implemented" severity failure ;
          return integer'low ;
      end case ;
    end function RandInt ;

    impure function RandSlv (Min, Max, Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(Min, Max), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (Min, Max, Size : natural) return Unsigned is
    begin
      return to_unsigned(RandInt(Min, Max), Size) ;
    end function RandUnsigned ;

    impure function RandSigned (Min, Max : integer; Size : natural ) return Signed is
    begin
      return to_signed(RandInt(Min, Max), Size) ;
    end function RandSigned ;


    --
    --  integer randomization with a range and exclude vector
    --    Distribution determined by RandomParm
    --
    impure function RandInt (Min, Max : integer; Exclude: integer_vector ) return integer is
      variable iRandomVal : integer ;
    begin
      case RandomParm.Distribution is
        when NONE | UNIFORM =>  return  Uniform(Min, Max, Exclude) ;
        when FAVOR_SMALL  =>    return  FavorSmall(Min, Max, Exclude) ;
        when FAVOR_BIG    =>    return  FavorBig (Min, Max, Exclude) ;
        when NORMAL =>          return  Normal(RandomParm.Mean, RandomParm.StdDeviation, Min, Max, Exclude) ;
        when POISSON =>         return  Poisson(RandomParm.Mean, Min, Max, Exclude) ;
        when others =>
          report "RandomPkg:  distribution not implemented" severity failure ;
          return integer'low ;
      end case ;
    end function RandInt ;

    impure function RandSlv (Min, Max : natural; Exclude: integer_vector; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(Min, Max, Exclude), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (Min, Max : natural; Exclude: integer_vector; Size  : natural ) return Unsigned is
    begin
      return to_unsigned(RandInt(Min, Max, Exclude), Size) ;
    end function RandUnsigned ;

    impure function RandSigned (Min, Max : integer; Exclude: integer_vector; Size  : natural ) return Signed is
    begin
      return to_signed(RandInt(Min, Max, Exclude), Size) ;
    end function RandSigned ;


    --
    -- Randomly select a value within a set of values
    --    Distribution determined by RandomParm
    --
    impure function RandInt ( A : integer_vector ) return integer is
      alias A_norm : integer_vector(1 to A'length) is A ;
    begin
      return A_norm( RandInt(1, A'length) ) ;
    end function RandInt ;

    impure function RandSlv (A : integer_vector ;  Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(A), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (A : integer_vector ;  Size : natural) return Unsigned is
    begin
      return to_unsigned(RandInt(A), Size) ;
    end function RandUnsigned ;

    impure function RandSigned (A : integer_vector ;  Size : natural ) return Signed is
    begin
      return to_signed(RandInt(A), Size) ;
    end function RandSigned ;


    --
    --  Randomly select a value within a set of values with exclude values (so can skip last or last n)
    --    Distribution determined by RandomParm
    --
    impure function RandInt ( A : integer_vector; Exclude: integer_vector ) return integer is
      alias A_norm : integer_vector(1 to A'length) is A ;
      variable ExcludeIndexList : SortListPType ;
      variable iVal : integer ;
    begin
      -- convert exclude list into indices of A_norm to exclude
      -- necessary to preserve ordering of the distribution (such as NORMAL)
      for i in A_norm'range loop
        if inside(A_norm(i), Exclude) then
          ExcludeIndexList.add(i) ;
        end if ;
      end loop ;
      -- Randomize an index into A_Norm with exclude index list
      iVal := RandInt(1, A'length, ExcludeIndexList.to_array (EraseList => TRUE)) ;
      -- return the value at the randomized index
      return A_norm(iVal) ;
    end function RandInt ;

    impure function RandSlv (A : integer_vector; Exclude: integer_vector;  Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(A, Exclude), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (A : integer_vector; Exclude: integer_vector;  Size : natural) return Unsigned is
    begin
      return to_unsigned(RandInt(A, Exclude), Size) ;
    end function RandUnsigned ;

    impure function RandSigned (A : integer_vector; Exclude: integer_vector;  Size : natural ) return Signed is
    begin
      return to_signed(RandInt(A, Exclude), Size) ;
    end function RandSigned ;


    --
    --  Basic Discrete Distributions
    --    Always uses Uniform
    --
    impure function DistInt ( Weight : integer_vector ) return integer is
      variable DistArray : integer_vector(weight'range) ;
      variable sum : integer ;
      variable iRandomVal : integer ;
    begin
      DistArray := Weight ;
      sum := 0 ;
      for i in DistArray'range loop
        DistArray(i) := DistArray(i) + sum ; 
        if DistArray(i) < sum then
          report "DistInt failed: negative weight or sum > 31 bits"
            severity failure ; 
          return DistArray'low ; -- allows debugging vs integer'left, out of range
        end if ; 
        sum := DistArray(i) ;
      end loop ;
      if sum >= 1 then 
        iRandomVal := Uniform(1, sum) ;
        for i in DistArray'range loop
          if iRandomVal <= DistArray(i) then
            return i ;
          end if;
        end loop ;
        report "DistInt: randomization failed" severity failure ; 
      else
        report "DistInt: No randomizatoin weights" severity failure ; 
      end if ; 
      return DistArray'low ; -- allows debugging vs integer'left, out of range
    end function DistInt ;

    impure function DistSlv ( Weight : integer_vector ; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(DistInt(Weight), Size)) ;
    end function DistSlv ;

    impure function DistUnsigned ( Weight : integer_vector ; Size  : natural ) return unsigned is
    begin
      return to_unsigned(DistInt(Weight), Size) ;
    end function DistUnsigned ;

    impure function DistSigned ( Weight : integer_vector ; Size  : natural ) return signed is
    begin
      return to_signed(DistInt(Weight), Size) ;
    end function DistSigned ;

    
    --
    --  Basic Distributions with exclude values (so can skip last or last n)
    --    Always uses Uniform via DistInt
    --
    impure function DistInt ( Weight : integer_vector; Exclude: integer_vector ) return integer is
      variable DistArray : integer_vector(weight'range) ;
      variable ExcludeTemp : integer ;
    begin
      DistArray := Weight ;
      for i in Exclude'range loop
        ExcludeTemp := Exclude(i) ;
        if ExcludeTemp >= DistArray'low and ExcludeTemp <= DistArray'high then
          DistArray(ExcludeTemp) := 0 ;
        end if ;
      end loop ;
      return DistInt(DistArray) ;
    end function DistInt ;

    impure function DistSlv ( Weight : integer_vector; Exclude: integer_vector; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(DistInt(Weight, Exclude), Size)) ;
    end function DistSlv ;

    impure function DistUnsigned ( Weight : integer_vector; Exclude: integer_vector; Size  : natural ) return unsigned is
    begin
      return to_unsigned(DistInt(Weight, Exclude), Size) ;
    end function DistUnsigned ;

    impure function DistSigned ( Weight : integer_vector; Exclude: integer_vector; Size  : natural ) return signed is
    begin
      return to_signed(DistInt(Weight, Exclude), Size) ;
    end function DistSigned ;


    --
    --  Distribution for sparse values
    --    Always uses Uniform via DistInt
    --
    impure function DistValInt ( A : DistType ) return integer is
      variable DistArray : integer_vector(0 to A'length -1) ;
      alias DistRecArray : DistType(DistArray'range) is A;
    begin
      for i in DistArray'range loop
        DistArray(i) := DistRecArray(i).Weight ;
      end loop ;
      return DistRecArray(DistInt(DistArray)).Value ;
    end function DistValInt ;

    impure function DistValSlv ( A : DistType ; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(DistValInt(A), Size)) ;
    end function DistValSlv ;

    impure function DistValUnsigned ( A : DistType ; Size  : natural ) return unsigned is
    begin
      return to_unsigned(DistValInt(A), Size) ;
    end function DistValUnsigned ;

    impure function DistValSigned ( A : DistType ; Size  : natural ) return signed is
    begin
      return to_signed(DistValInt(A), Size) ;
    end function DistValSigned ;


    --
    --  Distribution for sparse values with exclude values (so can skip last or last n)
    --    Always uses Uniform via DistInt
    --
    impure function DistValInt ( A : DistType; Exclude: integer_vector ) return integer is
      variable DistArray : integer_vector(0 to A'length -1) ;
      alias DistRecArray : DistType(DistArray'range) is A;
    begin
      for i in DistRecArray'range loop
        if inside(DistRecArray(i).Value, exclude) then
          DistArray(i) := 0 ;   -- exclude
        else
          DistArray(i) := DistRecArray(i).Weight ;
        end if;
      end loop ;
      return DistRecArray(DistInt(DistArray)).Value ;
    end function DistValInt ;

    impure function DistValSlv ( A : DistType; Exclude: integer_vector; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(DistValInt(A, Exclude), Size)) ;
    end function DistValSlv ;

    impure function DistValUnsigned ( A : DistType; Exclude: integer_vector; Size  : natural ) return unsigned is
    begin
      return to_unsigned(DistValInt(A, Exclude), Size) ;
    end function DistValUnsigned ;

    impure function DistValSigned ( A : DistType; Exclude: integer_vector; Size  : natural ) return signed is
    begin
      return to_signed(DistValInt(A, Exclude), Size) ;
    end function DistValSigned ;


    --
    -- Convenience Functions.  Resolve into calls into the other functions
    --
    impure function RandReal return real is
      variable RandomValue : real ;
    begin
      return RandReal(0.0, 1.0) ;
    end function RandReal ;

    impure function RandReal(Max: Real) return real is  -- 0.0 to Max
    begin
      return RandReal(0.0, Max) ;
      -- assert Max >= 0.0 report "RandReal: Range Error" severity FAILURE ;
      -- return RandReal * Max ;
    end function RandReal ;

    impure function RandInt (Max : integer) return integer is
    begin
      return RandInt(0, Max) ;
    end function RandInt ;

    impure function RandSlv (Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(0, 2**Size-1), Size)) ;
    end function RandSlv ;

    impure function RandSlv (Max, Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(0, Max), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (Size : natural) return Unsigned is
    begin
      return to_unsigned(RandInt(0, 2**Size-1), Size) ;
    end function RandUnsigned ;

    impure function RandUnsigned (Max, Size : natural) return Unsigned is
    begin
      return to_unsigned(RandInt(0, Max), Size) ;
    end function RandUnsigned ;

    impure function RandSigned (Size : natural) return Signed is
    begin
      return to_signed(RandInt(-2**(Size-1), 2**(Size-1)-1), Size) ;
    end function RandSigned ;

    impure function RandSigned (Max : integer;  Size : natural ) return Signed is
    begin
      -- chose 0 to Max rather than -Max to +Max to be same as RandUnsigned, either seems logical
      return to_signed(RandInt(0, Max), Size) ;
    end function RandSigned ;

  end protected body RandomPType ;

end RandomPkg ;