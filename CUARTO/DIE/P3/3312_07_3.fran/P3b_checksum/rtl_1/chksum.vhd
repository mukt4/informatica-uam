----------------------------------------------------------------------------------
-- Simplified checksum module.
-- Computes the check sum of 128 bits (8 x 16 bits)
-- SoP indicates new data to process (Start of Packet)
-- register input and outputs
-- G. Sutter sept 2016. Modif oct 2018. Modif FBD nov 2019.
-- UAM-Electratraining
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity chksum is
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
end chksum;

architecture Behavioral of chksum is

   -- This function performs a 1's-complement addition on 2 16-bit inputs.
   -- 1's complement addition requires adding the carry-out bit back into the sum.
   -- To increase performance two additions are made in parallel (with CIN=0, and CIN=1).
   function Add1Comp (In1, In2 : in std_logic_vector (15 downto 0)) return std_logic_vector is
      variable sum1, sum0: std_logic_vector(16 downto 0);
   begin
      sum0 :=  ('0' & In1) + ('0' & In2);
      sum1 :=  ('0' & In1) + ('0' & In2) + 1;
      -- if there is carry, return the sum with the extra 1 added:
      if (sum0(16) = '1') then
         return (sum1(15 downto 0));
      else 
         return (sum0(15 downto 0));
      end if;
   end Add1Comp;

   signal pktData0_reg : std_logic_vector (15 downto 0);
   signal pktData1_reg : std_logic_vector (15 downto 0);  
   signal pktData2_reg : std_logic_vector (15 downto 0);
   signal pktData3_reg : std_logic_vector (15 downto 0);  
   signal pktData4_reg : std_logic_vector (15 downto 0);
   signal pktData5_reg : std_logic_vector (15 downto 0);  
   signal pktData6_reg : std_logic_vector (15 downto 0);
   signal pktData7_reg : std_logic_vector (15 downto 0);  

   signal chksumValid_reg: std_logic;

   signal chksum_Partial     : std_logic_vector (18 downto 0); 
   signal chksum0123_Partial : std_logic_vector (17 downto 0); 
   signal chksum4567_Partial : std_logic_vector (17 downto 0);
   signal chksum01_Partial   : std_logic_vector (16 downto 0); 
   signal chksum23_Partial   : std_logic_vector (16 downto 0); 
   signal chksum45_Partial   : std_logic_vector (16 downto 0); 
   signal chksum67_Partial   : std_logic_vector (16 downto 0); 
  
   signal s1_SoP                : std_logic;
   signal s1_chksum0123_Partial : std_logic_vector(17 downto 0);
   signal s1_chksum4567_Partial : std_logic_vector(17 downto 0);

begin
   --Input Registers
   inp_reg: process (Clk)
   begin
      if (Clk'event and Clk='1') then
         if SoP = '1' then
            -- Reduce power consumption by freezing datapath when checksum is not needed
            pktData0_reg <= PktData0;
            pktData1_reg <= PktData1;
            pktData2_reg <= PktData2;
            pktData3_reg <= PktData3;
            pktData4_reg <= PktData4;
            pktData5_reg <= PktData5;
            pktData6_reg <= PktData6;
            pktData7_reg <= PktData7;
         end if;
         chksumValid_reg <= SoP;
      end if;
   end process;
 
   -- Chksum_Partial <=   Chksum7 + Chksum6 +... + Chksum0 : add 8 times 16 bits numbers
   -- chksum_Partial <= ("000" & pktData0_reg) + pktData1_reg + pktData2_reg + pktData3_reg 
   --                  + pktData4_reg  + pktData5_reg + pktData6_reg + pktData7_reg;
                     
   chksum0123_Partial <= ("00" & pktData0_reg) + pktData1_reg + pktData2_reg + pktData3_reg;
   chksum4567_Partial <= ("00" & pktData4_reg) + pktData5_reg + pktData6_reg + pktData7_reg;

   stage_1_ffs: process(Clk)
   begin
      if rising_edge(Clk) then
         s1_SoP <= chksumValid_reg;
         if chksumValid_reg = '1' then   
            s1_chksum0123_Partial <= chksum0123_Partial;
            s1_chksum4567_Partial <= chksum4567_Partial;
         end if;
      end if;
   end process;
   
   chksum_Partial <= ("0" & s1_chksum0123_Partial) + s1_chksum4567_Partial;

   -- Add the carries to get the 16-bit 1-complement sum:  
   final_add: process (Clk)
      variable chksumPart_upper : std_logic_vector(15 downto 0); 
   begin
      if (Clk'event and Clk='1') then        
         -- 0-extend to 16-bit the carry part of chksum_Partial (13 bits to the left):
         chksumPart_upper := '0' & x"000" & chksum_Partial(18 downto 16);
                  
         -- Make final one's complement sum and inversion:
         ChksumOut   <= std_logic_vector (not(Add1Comp(chksum_Partial(15 downto 0), chksumPart_upper)));
         ChksumValid <= s1_SoP;
      end if;
   end process;
end Behavioral;
