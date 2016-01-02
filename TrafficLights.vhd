LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

ENTITY SevenSegment IS PORT (
   
   dataIn      :  IN  std_logic_vector(3 DOWNTO 0);   -- The 4 bit data to be displayed
   blanking    :  IN  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  OUT std_logic_vector(6 DOWNTO 0)    -- 7-bit outputs to a 7-segment
); 
END SevenSegment;

ARCHITECTURE Behavioral OF SevenSegment IS

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

BEGIN

   with blanking & dataIn SELECT --  gfedcba        b3210      -- D7S
      segmentsOut(6 DOWNTO 0) <=    "1000000" WHEN "00000",    -- [0]
                                    "1111001" WHEN "00001",    -- [1]
                                    "0100100" WHEN "00010",    -- [2]      +---- a ----+
                                    "0110000" WHEN "00011",    -- [3]      |           |
                                    "0011001" WHEN "00100",    -- [4]      |           |
                                    "0010010" WHEN "00101",    -- [5]      f           b
                                    "0000010" WHEN "00110",    -- [6]      |           |
                                    "1111000" WHEN "00111",    -- [7]      |           |
                                    "0000000" WHEN "01000",    -- [8]      +---- g ----+
                                    "0010000" WHEN "01001",    -- [9]      |           |
                                    "0001000" WHEN "01010",    -- [A]      |           |
                                    "0000011" WHEN "01011",    -- [b]      e           c
                                    "0100111" WHEN "01100",    -- [c]      |           |
                                    "0100001" WHEN "01101",    -- [d]      |           |
                                    "0000110" WHEN "01110",    -- [E]      +---- d ----+
                                    "0001110" WHEN "01111",    -- [F]
                                    "1111111" WHEN OTHERS;     -- [ ]

END Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY Lab5 IS
   PORT(
      
      clock_50   : IN  STD_LOGIC;                     -- 50 MHz clock
      sw         : IN  STD_LOGIC_VECTOR(17 DOWNTO 0); -- 18 dip switches on the board
      ledr       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0); -- LEDs, many Red ones are available
      ledg       : OUT STD_LOGIC_VECTOR( 8 DOWNTO 0); -- LEDs, many Green ones are available
      hex0, hex2 : OUT STD_LOGIC_VECTOR( 6 DOWNTO 0)  -- seven segments to display numbers
);
END Lab5;

ARCHITECTURE SimpleCircuit OF Lab5 IS

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   COMPONENT SevenSegment PORT(        -- Declare the 7 segment component to be used
      dataIn      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      blanking    : IN  STD_LOGIC;
      segmentsOut : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
   );
   END COMPONENT;
----------------------------------------------------------------------------------------------------
   CONSTANT CLK_DIV_SIZE: INTEGER := 25;     -- size of vectors for the counters

   SIGNAL Main1HzCLK:   STD_LOGIC; -- main 1Hz clock to drive FSM
   SIGNAL OneHzBinCLK:  STD_LOGIC; -- binary 1 Hz clock
   SIGNAL OneHzModCLK:  STD_LOGIC; -- modulus1 Hz clock
   SIGNAL FlHzModCLK:   STD_LOGIC;
   SIGNAL bin_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset binary counter to zero
   SIGNAL mod_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL tenHz_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod_terminal: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   
   TYPE STATES IS (STATE0, STATE1, STATE2, STATE3, STATE4, STATE5);   -- list all the STATES
   SIGNAL state, next_state:  STATES;                 -- current and next state signals od type STATES
   
   SIGNAL state_number: STD_LOGIC_VECTOR(3 DOWNTO 0); -- binary state number to display on seven-segment
   SIGNAL state_time: UNSIGNED(3 DOWNTO 0);       	  -- binary state time to display on seven-segment
   SIGNAL state_counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL wait_counterEW: UNSIGNED(3 DOWNTO 0);
   SIGNAL OperationMode: STD_LOGIC;
   SIGNAL DefaultSide: STD_LOGIC;
   SIGNAL CarSensorNS: STD_LOGIC;
   SIGNAL CarSensorEW: STD_LOGIC; 
----------------------------------------------------------------------------------------------------

BEGIN

   BinCLK: PROCESS(clock_50)
   BEGIN
      IF (rising_edge(clock_50)) THEN 
         bin_counter <= bin_counter + 1; -- binary counter increments on rising clock edge
      END IF;
   END PROCESS;
   OneHzBinCLK <= std_logic(bin_counter(CLK_DIV_SIZE-1)); -- binary counter MSB
   LEDG(2) <= OneHzBinCLK;
---------------------------------------------------------------------------------------------------
		 
   ModCLK: PROCESS(clock_50) 
   BEGIN
      IF (rising_edge(clock_50))  THEN -- modulus counter increments on rising clock edge
         IF (tenHz_counter = "0001001100010010110011111") THEN  -- half period
            FlHzModCLK <= NOT FlHzModCLK;                 		-- toggle (10Hz clock)
            tenHz_counter <= to_unsigned(0,CLK_DIV_SIZE); 		-- reset counter and set variable size to 25 bits
         ELSE
            tenHz_counter <= tenHz_counter + 1;
         END IF;
     END IF;
     --FlHzModCLK <= clock_50;
     IF (rising_edge(FlHzModCLK)) THEN -- modulus counter increments on rising clock edge
         IF (mod_counter = "100") THEN       			-- half period
            OneHzModCLK <= NOT OneHzModCLK;             -- toggle (1Hz clock)
            mod_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter and set variable size to 25 bits
         ELSE
            mod_counter <= mod_counter + 1;
         END IF;
     END IF;
		
   END PROCESS;
   LEDG(1) <= OneHzModCLK;
----------------------------------------------------------------------------------------------------
   LEDG(0) <= FlHzModCLK;
----------------------------------------------------------------------------------------------------
	OperationMode <= sw(13);
	DefaultSide <= sw(16);
	CarSensorNS <= sw(15); 
	CarSensorEW <= sw(14); 
	

   FSM: PROCESS(state) -- main FSM
   BEGIN
      
      CASE state IS              -- 6 states of the state machine are described with their conditions below
         WHEN STATE0 =>
            state_number <= "0000";
            IF (state_time = "0010" AND OperationMode = '0' AND DefaultSide = '0' AND CarSensorEW = '0') THEN
               next_state <= STATE1;
			ELSIF(state_time = "0010" AND OperationMode = '0') THEN
               ledr(11) <= '0';
			   ledr(0) <= '1';
               ledg(7) <= '0';
               ledg(8) <= FlHzModCLK;
       --     ELSIF(OperationMode = '0') THEN
		--	   next_state <= STATE1;
			ELSE
				next_state <= STATE0;
			END IF;
			               
         WHEN STATE1 =>      -------STATE 1
            state_number <= "0001";
            IF (state_time = "0110" AND OperationMode = '0' AND DefaultSide = '0' AND CarSensorEW = '0') THEN 
               next_state <= STATE2;
            ELSIF(state_time = "0110" AND OperationMode = '0') THEN
               ledr(0) <= '1';
               ledr(11) <= '0';
               ledg(7) <= '0';
               ledg(8) <='1'; 
        --    ELSIF(OperationMode = '0') THEN
		--	   next_state <= STATE2; 
			   ELSE
				next_state <= STATE1;             
            END IF;
            
         WHEN STATE2 =>
            state_number <= "0010";
            IF (state_time = "1000" AND OperationMode = '0' AND DefaultSide = '0'  AND CarSensorEW = '0' ) THEN
				next_state <= STATE3;
            ELSIF(state_time = "1000" AND OperationMode = '0') THEN
               ledr(11) <= FlHzModCLK;
               ledr(0) <= '1';
               ledg(8) <= '0';
               ledg(7) <= '0'; 
		--	ELSIF (OperationMode = '0') THEN
		--	    next_state <= STATE3;
			    ELSE
				next_state <= STATE2;    
            END IF;
            
         WHEN STATE3 =>
            state_number <= "0011";
            IF ( state_time = "1010" AND OperationMode = '1' AND DefaultSide = '1' AND CarSensorNS = '0') THEN
               next_state <= STATE4;
            ELSIF(state_time = "1010" AND OperationMode = '0') THEN
               ledr(11) <= '1';
               ledr(0) <= '0';
               ledg(7) <= FlHzModCLK;
               ledg(8) <= '0'; 
       --     ELSIF(OperationMode = '0') THEN
		--	   next_state <= STATE4; 
			ELSE
				next_state <= STATE3;     
            END IF;
         
         WHEN STATE4 =>
            state_number <= "0100";
            IF (state_time = "1110" AND OperationMode = '1' AND DefaultSide = '1' AND CarSensorNS = '0' )  THEN
               next_state <= STATE5;
            ELSIF(state_time = "1110" AND OperationMode ='0') THEN
               ledr(11) <= '1';
               ledr(0) <= '0';
               ledg(7) <= '1';
               ledg(8) <= '0';
        --    ELSIF(OperationMode = '0') THEN
		--	   next_state <= STATE5;
			ELSE
				next_state <= STATE4;
            END IF;
            
         WHEN OTHERS => -- STATE5
            state_number <= "0101";
            IF (state_time = "0000" AND OperationMode = '1' AND DefaultSide = '1' AND CarSensorNS = '0') THEN
               next_state <= STATE0;
            ELSIF(state_time = "0000" AND OperationMode = '0') THEN
               ledr(11) <= '1';
               ledr(0) <= FlHzModCLK;
               ledg(7) <= '0';
               ledg(8) <= '0';
         --   ELSIF(OperationMode = '0') THEN
			--   next_state <= STATE0;
			ELSE
				next_state <= STATE5;
            END IF;
            
        
      END CASE;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   SeqLogic: PROCESS(OneHzModCLK, state) -- creats sequential logic to latch the state
   BEGIN
      IF (rising_edge(OneHzModCLK)) THEN
            state <= next_state;                   -- on the rising edge of clock the current state is updated with next state
            
        -- IF(state_time = "0000" AND OperationMode = '1' AND DefaultSide = '1' AND CarSensorNS = '0') THEN
		--	state_time <= "1110";
        -- ELSIF (state_time = "1000" AND OperationMode = '1' AND DefaultSide = '0' AND CarSensorEW = '0')  THEN
		--	state_time <= "0110";
	   --  ELSE
            state_time <= state_time + 1;			--used to increment time for traffic light counter in seconds
       --     wait_counterEW <= wait_counterEW + 1;
	   --  END IF;
	        state_counter <= state_counter + 1;    -- on the rising edge of clock the current counter is incremented if state is STATE1
		--    wait_counterEW <= wait_counterEW + 1;
		 
      END IF;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   D7S0: SevenSegment PORT MAP( state_number, '0', hex0 );
   D7S2: SevenSegment PORT MAP( std_logic_vector(state_counter), '0', hex2 );

END SimpleCircuit;