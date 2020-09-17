----------------------------------------------------------------------------------
-- Simple testbench for checksum
-- uses a FIFO to delay the answer
-- G.Sutter UAM - Electratraining
-- (Modified 2019 FBD)
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.tb_fifo_pkg.all;

entity chksum_tb is
end chksum_tb;

architecture Behavioral of chksum_tb is

  function Add1Comp (In1, In2 : in unsigned (15 downto 0)) return unsigned is
   variable Sum1, Sum0: unsigned(16 downto 0);
   --variable Sum_c1: unsigned (15 downto 0);
 begin
    Sum0 :=  ('0' & unsigned(In1)) + ('0' & unsigned(In2));
    Sum1 :=  ('0' & unsigned(In1)) + ('0' & unsigned(In2)) + 1;
    --Sum_c1 := Sum0(16) ? sum0(15 downto 0) : sum1(15 downto 0); return Sum_c1;
    if (Sum0(16) = '1') then
       return (Sum1(15 downto 0));
    else 
       return (Sum0(15 downto 0));
    end if;
 end Add1Comp;
 
   function calc_chksum (PktData0, PktData1, PktData2, PktData3, PktData4, 
                         PktData5, PktData6, PktData7: in std_logic_vector (15 downto 0)) return unsigned is
     variable Sum: unsigned(15 downto 0):= (others => '0');
   begin
      Sum := Add1Comp(unsigned(PktData0), unsigned(PktData1));
      Sum := Add1Comp(Sum, unsigned(PktData2));
      Sum := Add1Comp(Sum, unsigned(PktData3));
      Sum := Add1Comp(Sum, unsigned(PktData4));
      Sum := Add1Comp(Sum, unsigned(PktData5));      
      Sum := Add1Comp(Sum, unsigned(PktData6));
      Sum := Add1Comp(Sum, unsigned(PktData7));  
      return (not(Sum));
   end calc_chksum;
   
   component chksum is
      Port ( 
         Clk       : in std_logic;  -- Clock
         SoP       : in std_logic;  -- Start of Packet: the checksum is performed on the
                                    --                  accompanying data
         PktData0  : in std_logic_vector (15 downto 0); -- Data, 16-bit word 0
         PktData1  : in std_logic_vector (15 downto 0); --   "          "    1
         PktData2  : in std_logic_vector (15 downto 0); --   "          "    2
         PktData3  : in std_logic_vector (15 downto 0); --   "          "    3
         PktData4  : in std_logic_vector (15 downto 0); --   "          "    4
         PktData5  : in std_logic_vector (15 downto 0); --   "          "    5
         PktData6  : in std_logic_vector (15 downto 0); --   "          "    6
         PktData7  : in std_logic_vector (15 downto 0); --   "          "    7
         ChksumOut   : out std_logic_vector (15 downto 0); -- Checksum output
         ChksumValid : out std_logic                       -- ChecksumOut is valid if 1
      );
   end component chksum;
 
   signal endsim: boolean := false;
   signal Clk : std_logic := '0';
   constant CLK_PERIOD : time := 10 ns;
   constant MAX_TEST: integer := 10;

   signal PktData0 : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
   signal PktData1 : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
   signal PktData2 : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
   signal PktData3 : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
   signal PktData4 : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
   signal PktData5 : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
   signal PktData6 : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
   signal PktData7 : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
   signal ChksumOut, Chksum_Checked, Chksum_Gold : STD_LOGIC_VECTOR (15 downto 0);
   
   signal SoP, ChksumValid : std_logic := '0';

   shared variable data_fifo  : FIFO_TYPE := (width=>16, depth=>16, 
                                              read_ptr=>0, write_ptr=>0, 
                                              n_reads=>0, n_writes=>0,
                                              volume=>0, 
                                              empty=>true, full=>false, 
                                              contents=>(others=>(others=>'0')));
											  
begin

    Clk_process : process
    begin
         Clk <= '0';
         wait for CLK_PERIOD/2; 
         Clk <= '1';
         wait for CLK_PERIOD/2;
         if (endsim = true) then wait; end if;  
    end process;

    uut: chksum
    --Generic map (Pipe_mode => "no_pipe")
    Port map ( 
           Clk         => Clk,
           SoP         => SoP,      
           PktData0    => PktData0,
           PktData1    => PktData1,
           PktData2    => PktData2,
           PktData3    => PktData3,
           PktData4    => PktData4,
           PktData5    => PktData5,
           PktData6    => PktData6,
           PktData7    => PktData7,
           ChksumOut   => ChksumOut,
           ChksumValid => ChksumValid);

    
    gen_values: process
        variable Chksum_Golden : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
    begin
        -- Generate packets of data with SoP separation between them:
        for i in 1 to MAX_TEST loop
            wait until rising_edge (Clk); 
            PktData0 <= std_logic_vector(to_unsigned(i*7, 16));
            PktData1 <= std_logic_vector(to_unsigned(i*7+1, 16));
            PktData2 <= std_logic_vector(to_unsigned(i*7+2, 16));
            PktData3 <= std_logic_vector(to_unsigned(i*7+3, 16));
            PktData4 <= std_logic_vector(to_unsigned(i*7+4, 16));
            PktData5 <= std_logic_vector(to_unsigned(i*7+5, 16));
            PktData6 <= std_logic_vector(to_unsigned(i*7+6, 16));
            PktData7 <= std_logic_vector(to_unsigned(i*7+7, 16));
            SoP <= '1';
            -- Small wait to get PktData* assigned and push these inputs and expected output to FIFO:
            wait for CLK_PERIOD * 0.1;
            Chksum_Golden := std_logic_vector(calc_chksum(PktData0, PktData1, PktData2, 
                                       PktData3, PktData4,PktData5, PktData6, PktData7));
            push(data_fifo,Chksum_Golden);
            --report  "Push Golden: " & integer'image(to_integer(unsigned(Chksum_Golden)));
            wait until rising_edge (Clk); 
            SoP <= '0';
            wait for 3.9 * CLK_PERIOD;     
        end loop;
        
        wait for 1.9*CLK_PERIOD;
            
        -- Generate packets of data with SoP at all cycles:
        for i in 1 to MAX_TEST loop
            wait until rising_edge (Clk); 
            PktData0 <= std_logic_vector(to_unsigned(i*11, 16));
            PktData1 <= std_logic_vector(to_unsigned(i*11+1, 16));
            PktData2 <= std_logic_vector(to_unsigned(i*11+2, 16));
            PktData3 <= std_logic_vector(to_unsigned(i*11+3, 16));
            PktData4 <= std_logic_vector(to_unsigned(i*11+4, 16));
            PktData5 <= std_logic_vector(to_unsigned(i*11+5, 16));
            PktData6 <= std_logic_vector(to_unsigned(i*11+6, 16));
            PktData7 <= std_logic_vector(to_unsigned(i*11+7, 16));
            SoP <= '1';
            -- Small wait to get PktData* assigned and push these inputs and expected output to FIFO:
            wait for CLK_PERIOD * 0.1;
            Chksum_Golden := std_logic_vector(calc_chksum(PktData0, PktData1, PktData2, 
                                                   PktData3, PktData4,PktData5, PktData6, PktData7));
            push(data_fifo,Chksum_Golden);                                       
        end loop;
        
        SoP <= '0';        
        wait for 2*CLK_PERIOD;
        endsim <= true;
        wait;
    
    end process;
    
    check_results: process
        variable Chksum_Golden : STD_LOGIC_VECTOR (15 downto 0):=(others => '0');
    begin
        report "Simulation START";
       
        while (endsim = false) loop
            wait until rising_edge(Clk);
            if (ChksumValid = '1') then
                pop(data_fifo,Chksum_Golden);
                --report  "Pop Golden: " & integer'image(to_integer(unsigned(Chksum_Golden)));
                assert ChksumOut = Chksum_Golden 
                report "ERROR!. No equal values. Chk HW " & integer'image(to_integer(unsigned(ChksumOut))) &
                "  chk Golden: " & integer'image(to_integer(unsigned(Chksum_Golden)))
                severity error;                
            end if;     

            -- Make comparison visible in waves:
            Chksum_Checked <= ChksumOut;
            Chksum_Gold    <= Chksum_Golden;
            wait for CLK_PERIOD/4;
        end loop;
       
        wait for 2*CLK_PERIOD;
        report "Simulation END (check possible errors above)";
        wait;
    
    end process check_results;
    
end Behavioral;
