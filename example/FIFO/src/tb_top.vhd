--------------------------------------------------------------------
-- (c) Aldec, Inc.
-- All rights reserved.
--
-- FIFO example with Randomization and Coverage Packages
-- Date Modified: Dec-20-2011
--
--  Verbatim copies of this source file may be used and
--  distributed without restriction.
--------------------------------------------------------------------

library ieee;
USE ieee.std_logic_1164.ALL;
--USE ieee.std_logic_unsigned.all;
USE ieee.numeric_std.ALL;
use ieee.math_real.all;
use std.textio.all;

use work.RandomBasePkg.all ; 
use work.RandomPkg.all ;
use work.CoveragePkg.all;


entity tb_top is
	generic
		(
		log_file: string  := "res.log";
		data_width  : integer := 8;
		clk_period : time := 40 ns;
		TimeOut : time := 200 us;
		fifo_dpth :integer := 128
	);
	
	
end tb_top;


architecture behavior of tb_top is
	
	file l_file: TEXT open write_mode is log_file;
	
	------- Component Declaration for the Unit Under Test (UUT)
	
	component fifo
		generic(
			data_width : INTEGER := 8;
			fifo_depth : INTEGER := 128
		);
		port(
			rst : in STD_LOGIC;
			wr_en : in STD_LOGIC;
			wr_clk : in STD_LOGIC;
			wr_data : in STD_LOGIC_VECTOR (data_width-1 downto 0);
			rd_en : in STD_LOGIC;
			rd_clk : in STD_LOGIC;
			rd_data : out STD_LOGIC_VECTOR (data_width-1 downto 0);
			empty : out STD_LOGIC;
			full : out STD_LOGIC
		);
	end component;
	
	----------------------------------------------------------------------------------------------------------------
	
	--the number of coverage bins (coverage points)
	constant numcb : integer := 3;
	
	signal reset     :std_logic := '1';
	signal clk    :std_logic := '0';
	signal fifo_re   :std_logic ;   
	signal dataout   :std_logic_vector(data_width-1 downto 0);		
	signal end_sim   :std_logic  :='0';
	signal full      :std_logic;
	signal empty     :std_logic;
	signal burstnum :integer := 0;
	signal wordnum   :integer := 0;
	
	--new changes
	signal cnt      :unsigned (1 downto 0) := "00";
	type state_type is (S_IDLE,S_DELAY,S_READ);  
	signal state: state_type; 
	signal rd_clk   :std_logic;
	
	--coverage objects
	shared variable cp1_full : CovPType;
	shared variable cp2_empty : CovPType;
	shared variable cp3_cross_we_full    : CovPType;
	shared variable cp4_illegal_re_empty   : CovPType;
	
	
	type FifoWrInfType is record 
		we     : std_logic ;
		datain   : std_logic_vector( data_width-1 downto 0 );
	end record;
	signal FifoWrInf : FifoWrInfType := (we => '0', datain => (others=>'0')) ;
	
	
	procedure GloablReset (signal reset : out std_logic ) is
	begin 
		reset <='1' ;
		wait for 80 ns;
		reset <='0' ;
	end procedure;
	
	procedure FifoWriteWord (
		word : in std_logic_vector(data_width-1 downto 0);
		signal	FifoWrInf : out FifoWrInfType ) is 
	begin		
		--setting the bus for fifo
		FifoWrInf.we <= '1';
		FifoWrInf.datain <= word;
		wait until rising_edge(clk);					
		--clearing write enable 
		FifoWrInf.we <= '0';
		
	end procedure;
	
	procedure FifoRandBurstWrite( 
		len : in integer;
		signal	FifoWrInf : out FifoWrInfType) is
		variable RV : RandomPType ;
		variable wordgen : std_logic_vector(data_width-1 downto 0);
	begin			
		--Sending len number of words to the fifo
		for i in 1 to len loop
			--Creating the random value to be sent to fifo
			wordgen := RV.RandSlv(0, 255, 8);
			--writing the word to fifo
			FifoWriteWord(wordgen, FifoWrInf);
		end loop;
	end procedure;	
	
	impure function getFC return Integer is
		--getFC function calculates the FC value as a persentage 
		variable tmp : real;
	begin
		--the illegal bins are not taken into account
		tmp := (real(to_integer(cp1_full.IsCovered)+to_integer(cp2_empty.IsCovered)+to_integer(cp3_cross_we_full.IsCovered))/real(numcb)) * 100.0;
		return integer(tmp);
	end function getFC;
	
	--adjustknobs procedure analyzes all bins and adjuct the constraints for random data to improve the probability of hitting 
	--uncovered bins
	procedure adjustknobs
		(	
		minlen : inout integer ;
		maxlen : inout integer ; 
		mindelay: inout integer ;
		maxdelay : inout integer
		)
		is	
		variable tmp : real;
	begin
		--the algorith below is just used as an example and 
		--by no means is the most effecient in adjusting left and right range for values generation		
		
		if (cp1_full.IsCovered = FALSE or cp3_cross_we_full.IsCovered = FALSE) then
			----reduce mindelay
			tmp := real(mindelay); --to real type
			tmp := tmp - tmp*0.3; --reducing by 30%
			--make sure mindelay is always >= 1
			if (integer(tmp) >= 1) then
				mindelay := integer(tmp); --back to integer
			else
				mindelay := 1; --to avoid going under 1
			end if;
			--reduce maxdelay
			tmp := real(maxdelay); --to real type
			tmp := tmp - tmp*0.3; --reducing by 30%
			--make sure maxdelay is always > then mindelay
			if (integer(tmp) > mindelay) then
				maxdelay := integer(tmp); --back to integer
			else
				maxdelay := mindelay + 1; --to avoid going under mindelay
			end if;
			
			--and increase the packet length
			tmp := real(maxlen); --to real type
			tmp := tmp + tmp*0.5; --increase by 50%			
			maxlen := integer(tmp); --back to integer			
			
		else 
			if (cp2_empty.IsCovered = FALSE) then
				-- to empty the fifo increase the delay between packets
				--increase mindelay
				tmp := real(mindelay); --to real type
				tmp := tmp + tmp*0.3; --increase by 30%
				mindelay := integer(tmp); --back to integer
				
				--increase maxdelay
				tmp := real(maxdelay); --to real type
				tmp := tmp + tmp*0.3; --increase by 30%
				maxdelay := integer(tmp); --back to integer
				
				--and reduce the packet lengh
				--reduce minlen
				tmp := real(minlen); --to real type
				tmp := tmp - tmp*0.3; --reducing by 30%				
				if (integer(tmp) >= 1) then
					minlen := integer(tmp); --back to integer
				else
					minlen := 1; --to avoid going under value 1
				end if;
				
				--reduce maxlen
				tmp := real(maxlen); --to real type
				tmp := tmp - tmp*0.3; --reducing by 30%				
				if (integer(tmp) > minlen) then
					maxlen := integer(tmp); --back to integer
				else
					maxlen := minlen + 1; --to avoid going under value 1
				end if;
			end if;
		end if;
		
	end procedure adjustknobs;
	
	procedure Message ( str : string ) is
		variable buf : LINE;
	begin
		write(buf, str);
		writeline(output, buf);
	end;
	
	
begin
	----------------------------------------------------------------------------------------------------------------	
	
	DUT: fifo
	generic map(
		data_width => 8,
	fifo_depth => 128)
	port map(
		rst => reset, 
		wr_en => FifoWrInf.we, 
		wr_clk => clk, 
		wr_data => FifoWrInf.datain, 
		rd_en => fifo_re, 
		rd_clk => rd_clk, 
		rd_data => dataout, 
		empty => empty,
		full => full
	);
	
	----------------------------------------------------------------------------------------------------------------
	
	TestFlow: process
		--This process implements writing to FIFO and also checking 
		--the FC results and adjusting the randomization if necessary
		variable delay : integer;
		variable len : integer;
		variable RV : RandomPType ; 
		variable FC_prev : integer:=0;
		variable FC : integer;
		variable minlen : integer := 10;
		variable maxlen : integer := 64;
		variable mindelay: integer := 1;
		variable maxdelay : integer := 20;
	begin
		--initializing the generator with the seed
		RV.InitSeed(RV'instance_name) ; 
		
		--reseting the DUT
		GloablReset(reset);
		
		--Main loop
		while (end_sim = '0') loop
			
			--random delay between sending the next packet			
			delay := RV.RandInt(mindelay, maxdelay);
			wait for delay * clk_period;
			
			-- Generating the random size packet and sending it to the fifo
			len := RV.RandInt(minlen, maxlen);
			FifoRandBurstWrite(len,FifoWrInf);
			--counting number of workds transfered (for statistics)
			wordnum <= wordnum + len;
			
			--checking the functional coverage
			FC := getFC;
			if FC = FC_prev then
				adjustknobs(minlen, maxlen, mindelay, maxdelay);				
			end if;
			--updating the FC_prev
			FC_prev := FC;
			--incrementing the packet counter ( for statistics)
			burstnum <= burstnum +1;
		end loop;
		
		wait;
	end process; 
	----------------------------------------------------------------------------------------------------------------	
	CoverageMonitor: process 		
		variable we_integer:integer;
		variable full_integer:integer; 	
		variable re_integer,empty_integer:integer;
		variable re_empty_illegal: std_logic;
		variable re_empty_illegal_integer: integer;
		variable RV : RandomPType ;
		variable bin_1d: CovBinBaseType;
		variable bin_2d: CovMatrix2BaseType;
		variable status_notFull, status_notEmpty, status_notWeFull: bit;
	begin		
		--creating bins for cover points
		--coverage point 1 - for fifo's full signal (High). The coverage goal is set to 3
		cp1_full.AddBins(3,GenBin(1));
		--coverage point 2 - for fifo's empty signal (High). The coverage goal is set to 3
		cp2_empty.AddBins(3,GenBin(1));
		--coverage point 3 - cross coverage for simultaneous FifoWrInfType.we and full signal. The coverage goal is set to 3
		cp3_cross_we_full.AddCross(3,GenBin(1), GenBin(1));
		
		-- coverage point 4 - creating illegal bin when empty and re are high at the same time
		cp4_illegal_re_empty.AddBins(IllegalBin(1));
		
		--setting the initial statuses. Statuses are used for proper coverage event counting towards the goal. For instance,
		--when FIFO gets full we don't want to increment the full coverage point again on the next clock cycle. Instead, we 
		--want to count the event when FIFO transitions from not being full to full.
		status_notFull := '1';
		status_notEmpty := '1';
		status_notWeFull := '1';
		
		--collecting coverage
		MainCovLoop: while not (cp1_full.IsCovered and cp2_empty.IsCovered and cp3_cross_we_full.IsCovered)	loop 
			
			wait until rising_edge(clk) and reset = '0';
			we_integer := to_integer(FifoWrInf.we);
			full_integer := to_integer(full);
			if full = '0' then
				status_notFull := '1';
			end if;
			
			re_integer := to_integer(fifo_re);
			empty_integer := to_integer(empty);
			if empty = '0' then 
				status_notEmpty := '1';
			end if;
			
			if not (FifoWrInf.we = '1' and full = '1') then
				status_notWeFull := '1';
			end if;
			
			re_empty_illegal := fifo_re and empty;
			re_empty_illegal_integer := to_integer(re_empty_illegal);
			
			--check if cp1 is covered
			if (cp1_full.IsCovered = FALSE and status_notFull = '1') then
				--if not then sample it
				cp1_full.ICover(to_integer(full) ) ;				
				if (full = '1') then
					status_notFull := '0';					
					if (cp1_full.IsCovered) then					
						Message("Covered condition *FIFO Full* @ " & time'image(now) );
					else 
						Message("Hit condition *FIFO Full* @ " & time'image(now) );						
					end if;
					cp1_full.WriteBin;
				end if;
			end if;
			
			--check if cp2 is covered
			if (cp2_empty.IsCovered = FALSE and status_notEmpty = '1') then 			
				--if not then sample it
				cp2_empty.ICover(to_integer(empty)) ;				
				if (empty = '1') then
					status_notEmpty := '0';
					if (cp2_empty.IsCovered) then
						Message("Covered condition *FIFO Empty* @ " & time'image(now) );
					else
						Message("Hit condition *FIFO Empty* @ " & time'image(now) );
					end if;
					cp2_empty.WriteBin ;
				end if;
			end if;
			
			
			--check if cp3 is covered
			if (cp3_cross_we_full.IsCovered = FALSE and status_notWeFull = '1') then 			
				--if not then sample it
				cp3_cross_we_full.ICover((we_integer, full_integer));				
				if (FifoWrInf.we = '1' and full = '1') then
					status_notWeFull := '0';
					if (cp3_cross_we_full.IsCovered) then					
						Message("Covered condition *Writing to full FIFO (we and full)* @ " & time'image(now) );
					else 
						Message("Hit condition *Writing to full FIFO (we and full)* @ " & time'image(now) );
					end if;
					cp3_cross_we_full.WriteBin ; 
				end if;
			end if;
			
			--check for the cp4 as long as the simulation is running
			cp4_illegal_re_empty.ICover(re_empty_illegal_integer);
			
			--Check for TimeOut and force exit when now is greater than TimeOut value
			exit MainCovLoop when now >= TimeOut;
			
		end loop;
		
		
		--Final reporting
		if now >= TimeOut then
			Message("TIME OUT. Cannot achieve the target functional coverage. You may try increasing the TimeOut generic in the tb_top instance.");
		else
			Message("SUCCESS. The functional coverage goal  is achieved.");			
		end if;
		Message("Number of burst transfers: " & integer'image(burstnum));
		Message("Total words transferred: " & integer'image(wordnum));
		Message("Coverage Goal: 100%");
		Message("Achieved Coverage: " & integer'image(getFC) & "%");
		
	    cp3_cross_we_full.WriteCovDb ("we_full.txt", OpenKind => WRITE_MODE );
		Message("Database written to 'we_full.txt' text file.");
		cp2_empty.WriteCovDb ("empty.txt", OpenKind => WRITE_MODE );
		Message("Database written to 'empty.txt' text file.");
		cp1_full.WriteCovDb ("full.txt", OpenKind => WRITE_MODE );
		Message("Database written to 'full.txt' text file.");
		
		--let the simulation run for a little longer before stopping it
		wait for 1 us;
		--end the simulation by suspending all the processes
		end_sim <= '1';
		
		wait;
	end process;
	
	
	---------------------------------------------------------------------------------------------------
	readInf:process (rd_clk,reset)
		variable RV :RandomPType;
		variable count: integer :=0;
		--mindelay and maxdelay are set to introduce some delay in CPU reaction to not empty fifo
		constant mindelay : integer:= 5;
		constant maxdelay : integer:= 30;
		
	begin
		--initializing the generator with the seed
		RV.InitSeed(RV'instance_name) ; 		
		
		if reset = '1' then
			state <= S_IDLE;
			--set random delay before CPU starts reading
			count := RV.RandInt(mindelay, maxdelay);
		elsif rising_edge(rd_clk)then
			case state is				
				--in IDLE state CPU waits until fifo becomes not empty				
				when S_IDLE => 
				if(empty ='1') then
					--set random delay before CPU starts reading
					count := RV.RandInt(mindelay, maxdelay);
				else
					state <= S_DELAY;
				end if;   
				--DELAY state inserts a delay before CPU starts actual reading
				when S_DELAY =>
				if(count > 0) then
					--decrement the counter and state in the same state			
					count := count - 1;					
				else
					state <= S_READ;					
				end if;
				--In READ state the read enable strobe will be set to high
				when S_READ =>
				--unless fifo is empty stay in the same state
				if(empty = '1') then  
					state <= S_IDLE;
				end if;
				
				when others  =>
				state <= S_IDLE;				
				
			end case;
		end if;
	end process;
	
	fifo_re <= '1' when (state = S_READ and empty = '0') else '0';	--added empty='0' to instantly disable reading when empty goes High.
	-------------------------------------------------------------------------------------------	
	clkGen : process
	begin
		if (end_sim = '0') then
			clk <= '0';
			wait for clk_period/2;
			clk <= '1';
			wait for clk_period/2;
		else 
			wait;--suspend the simulation
		end if;
	end process;
	-------------------------------------------------------------------------------------------	
	clk_slow:process (reset, clk)
	begin		
		if rising_edge(clk) then
			if (reset = '1') then 
				cnt <= (others => '0');
			else
				cnt <= cnt + 1;
			end if;
		end if;
	end process;
	
	rd_clk <= '1' when cnt = "11" else '0';
	
	
	
end architecture; 