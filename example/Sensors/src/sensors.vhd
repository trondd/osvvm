-- ######################################################################################
-- # File: sensors.vhd
-- # Version: 1.1
-- # Dependencies: RandomPkg
-- #               CoveragePkg (revision 2.3 or newer)
-- # Functionality: This unit emulates reading from 8x8 matrix of sensors.
-- # Sensors are randomly filled with data (normal distribution, Mean and SD controlled
-- # with generics). Index values for reading from sensor matrix are either generated
-- # using uniform RNG or intelligent randomization feature of CoveragePkg (selection
-- # is controlled by generic).
-- # Coverage is collected for sensor index pairs and for sensor data. Current coverage
-- # results are used to control index generation (intelligent mode) and to flatten
-- # sensor data distribution for faster coverage.
-- # The test prints report in the console and saves coverage databases in two files.
-- ######################################################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.CoveragePkg.all;
use work.RandomPkg.all;

use std.textio.all;

entity sensors is
    generic
    (
        Intelligent : boolean := True;      -- Intelligent or Uniform index randomization
        DataMean : real := 128.0;           -- Mean value of sensor data distribution
        DataSDstart: real := 32.0;          -- SD of sensor data distribution
        DataSDinc: real := 4.0              -- increment of SD value auto-adjustment
    );
end entity sensors;


architecture behavior of sensors is
    signal END_TEST : boolean := false;         -- Flag that ends clock generation
    signal CLK: std_logic;                      -- Universal clocking signal
    
    type sensorsPType is protected              -- Type handling setting/retrieving sensor data
        procedure set ( X,Y,Val : integer );
        impure function get ( X,Y : integer ) return integer;
    end protected;

    type sensorsPType is protected body
        type data_array is array (0 to 7, 0 to 7) of integer;
        variable data : data_array := (others=>(others=>0));
        
        procedure set ( X,Y,Val : integer ) is  -- sets sensor data at position (X,Y)
        begin
            data(X, Y) := Val;
        end;
        
        impure function get ( X,Y : integer ) return integer is
        begin                                   -- gets sensor data from position (X,Y)
            return data(X, Y);
        end;
    	
    end protected body;
    
    shared variable sensors : sensorsPType;     -- 8x8 sensor matrix
    
--    shared variable Rsens   : RandomPType;      -- Object for generating one sensor data
--    shared variable Rxy     : RandomPType;      -- Object for generating sensor indices

    shared variable DCov  : CovPType;           -- Object for sensor data coverpoint
    shared variable XYCov : CovPType;           -- Object for sensor address cross
    
    procedure Message ( str : string ) is       -- prints string argument to the console
        variable buf : LINE;
    begin
        write(buf, str);
        writeline(output, buf);
    end;
    
begin
-- This process provides initialization of random and coverage objects.
-- Data Collection loop runs until full data coverage is achieved.
-- Message is displayed within the loop when index coverage is achieved.
-- When all tests are done, END_TEST flag turns off clock generation.
-- Messages describing verification progress and final reports are printed from here.
  CollectCv : process
        variable Rxy : RandomPType;             -- object for generating sensor indices
        variable X,Y,SensorData : integer;      -- index and sensor data buffers
        variable buf : LINE;                    -- line buffer for direct text IO
        variable xycnt, dcnt : integer;         -- loop counters
        variable bin_1d: CovBinBaseType;        -- 1 dimensional bin data buffer
        variable bin_2d: CovMatrix2BaseType;    -- 2 dimensional bin data buffer
        
    begin
        Rxy.InitSeed(Rxy'instance_name);        -- seed array index random variable Rxy'instance_name
        DCov.AddBins(GenBin(0, 255, 16));           -- 16 bins (0 to 15, 16 to 31, ...)
        XYCov.AddCross(GenBin(0,7), GenBin(0,7));   -- 8x8 bin matrix.
        XYCov.InitSeed(XYCov'instance_name);    -- seed for intelligent coverage
        Message("*** Initialized Randomization and Coverage Structures ***");
        Message("  Data mean value set to " & real'image(DataMean));
        Message("  Data SD value set to " & real'image(DataSDstart));

        dcnt  := 0;                             -- initialize collection loop counter
        xycnt := 0;                             -- initialize index coverage counter
        Collect: while not DCov.IsCovered loop  -- collection loop runs until data covered
           wait until rising_edge(CLK);         -- sampling event detection
           if Intelligent then
              (X, Y) := XYCov.RandCovPoint;     -- randomize uncovered indices
           else
              X := Rxy.RandInt(0,7);            -- generate X array index
              Y := Rxy.RandInt(0,7);            -- generate Y array index
           end if;
           DCov.ICover(sensors.get(X, Y));      -- collect coverpoint data
           XYCov.ICover((X, Y));                -- collect cross data
           dcnt := dcnt + 1;                    -- incr. collection loop counter
           if xycnt=0 and XYCov.IsCovered then  -- index coverage just achieved
		      xycnt := dcnt;
              Message("# Index Coverage achieved after " & integer'image(xycnt) 
                       & " iterations.");
           end if;
        end loop Collect;
        Message("$ Sensor Data Coverage achieved after " & integer'image(dcnt) 
                 & " iterations.");

        END_TEST <= true;       -- We are done with testing - only reporting left...
        Message("*** Complete Coverage achieved !!! ***");
        Message("*******   Reporting Stage...   *******");
        
        -- DCov.WriteBin;  -- writing extensive report for coverpoint would be very long!
        Message("$$$ Sensor Data Coverage Results $$$");
        DmpD: for i in 1 to 16 loop     -- print 16 lines of data coverage results
            bin_1d := DCov.GetBin(i);   -- get 1 bin data
            Message("  Bin: " & integer'image(bin_1d.BinVal(1).min)     -- print bin value range
                     & " to " & integer'image(bin_1d.BinVal(1).max) 
                     & "; Count: " & integer'image(bin_1d.Count)    );  -- print bin count
        end loop DmpD;
        DCov.WriteCovDb ("quicktest_Dcovdb.txt", OpenKind => WRITE_MODE );  -- dump database
        Message("Database written to 'quicktest_Dcovdb.txt' text file.");
        
        --XYCov.WriteBin; -- writing extensive report for cross would be very long
        Message("####### Sensor Matrix Index Coverage Results #######");
        DmpRows: for i in 0 to 7 loop               -- sweep rows
            DmpY: for j in 0 to 7 loop              -- sweep columns
                bin_2d := XYCov.GetBin(1+i*8+j);    -- get 1 bin data
                write(buf, bin_2d.Count, FIELD=>6); -- write 6 characters per sensor
            end loop DmpY;                          -- buffer full now
            writeline(output, buf);                 -- dump buffer to the console
        end loop DmpRows;
        XYCov.WriteCovDb ("quicktest_XYcovdb.txt", OpenKind => WRITE_MODE );-- dump database
        Message("Database written to 'quicktest_XYcovdb.txt' text file.");
        Message("*** Goodbye! :-) ***");
        
        wait;                                   -- end of report - halt process
    end process CollectCv;    

-- This process generates sensor data for the entire matrix on every 64th falling clock edge.
-- If sensor index values are covered but sensor data is not covered yet, the process increases 
--  SD parameter of Normal distribution to speed up data coverage goal achievement.
-- Mean value, initial SD and SD increment are controlled by generics and can be changed 
--  from simulator command line without recompilation (-Gname=value).
  GenSensData : process
        variable Rsens : RandomPType;           -- object for generating one sensor data
        variable cnt6b : unsigned(5 downto 0) := (others=>'1');
        variable DataSD : real := DataSDstart;
    begin
        Rsens.InitSeed(Rsens'instance_name);    -- seed sensor data random variable
		wait for 0 ns;							-- force order of process execution
        GenSens: while not DCov.IsCovered loop
            wait until falling_edge(CLK);
            cnt6b := cnt6b + 1;         -- increment 6-bit counter
            if cnt6b=0 then             -- generate sensor data every 64th clock
                if not DCov.IsCovered and DataSDinc/= 0.0 then
                  -- When data coverage not yet achieved and SD incrementation allowed,
                  --  print current hole count and increment SD
                    Message("  Data coverage holes count is: " & integer'image(DCov.CountCovHoles));
                    DataSD := DataSD + DataSDinc;
                    Message("    Standard Deviation for data generation increased to " & real'image(DataSD));
                end if;
                for i in 0 to 7 loop
                    for j in 0 to 7 loop
                        sensors.set(i,j, Rsens.Normal(DataMean, DataSD, 1, 255));
                    end loop;
                end loop;
            end if;
        end loop GenSens;
        wait;
    end process GenSensData;
    
-- This process generates CLK signal (100MHz, 50%) as long as END_TEST flag is False
  ClkGen: process
    begin
        while not END_TEST loop         -- stop clock when test done
            CLK <= '0'; wait for 5 ns;
            CLK <= '1'; wait for 5 ns;
        end loop;
        wait;                           -- stop process forever
    end process ClkGen;

end architecture behavior;
