----------------------------------------------------------------------------------
-- Simplified checksum module.
-- Computes the check sum of 128 bits (8 x 16 bits)
-- SoP indicates new data to process (Start of Packet)
-- register input and outputs
-- G. Sutter sept 2016. Modif oct 2018. Modif FBD nov 2019.
-- UAM-Electratraining
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-- Date: 28 Nov 2019
-- Authors: - Francisco Alcudia
--          - Tomás Higuera
-- 4 stages pipeline version of simplified checksum module.
-----------------------------------------------------------------------------------
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
   
   -- Registers for capturing the result of the first stage.
   signal s1_SoP              : std_logic;
   signal s1_chksum01_Partial : std_logic_vector(16 downto 0);
   signal s1_chksum23_Partial : std_logic_vector(16 downto 0);
   signal s1_chksum45_Partial : std_logic_vector(16 downto 0);
   signal s1_chksum67_Partial : std_logic_vector(16 downto 0);
   
   -- Registers for capturing the result of the second stage.
   signal s2_SoP                : std_logic;
   signal s2_chksum0123_Partial : std_logic_vector(17 downto 0);
   signal s2_chksum4567_Partial : std_logic_vector(17 downto 0);
   
   -- Registers for capturing the result of the third stage.
   signal s3_SoP            : std_logic;
   signal s3_chksum_Partial : std_logic_vector(18 downto 0);
   
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
   
   -- Combinational logic of the first stage
   chksum01_Partial <= ("0" & pktData0_reg) + pktData1_reg; 
   chksum23_Partial <= ("0" & pktData2_reg) + pktData3_reg;
   chksum45_Partial <= ("0" & pktData4_reg) + pktData5_reg;
   chksum67_Partial <= ("0" & pktData6_reg) + pktData7_reg;
   
   -- Capture of the first stage
   stage_1_ffs: process(Clk)
   begin
      if rising_edge(Clk) then
         s1_SoP <= chksumValid_reg;
         if chksumValid_reg = '1' then
            -- Reduce power consumption by freezing datapath when checksum is not needed
            s1_chksum01_Partial <= chksum01_Partial;
            s1_chksum23_Partial <= chksum23_Partial;
            s1_chksum45_Partial <= chksum45_Partial;
            s1_chksum67_Partial <= chksum67_Partial;
         end if;
      end if;
   end process;
   
   -- Combinational logic of the second stage
   chksum0123_Partial <= ("0" & s1_chksum01_Partial) + s1_chksum23_Partial;
   chksum4567_Partial <= ("0" & s1_chksum45_Partial) + s1_chksum67_Partial;
   
   -- Capture of the second stage
   stage_2_ffs: process(Clk)
   begin
      if rising_edge(Clk) then
         s2_SoP <= s1_SoP;
         if s1_SoP = '1' then
            s2_chksum0123_Partial <= chksum0123_Partial;
            s2_chksum4567_Partial <= chksum4567_Partial;
         end if;
      end if;
   end process;
   
   -- Combinational logic of the third stage
   chksum_Partial <= ("0" & s2_chksum0123_Partial) + s2_chksum4567_Partial;
   
   -- Capture of the third stage
   stage_3_ffs: process(Clk)
   begin
      if rising_edge(Clk) then
         s3_SoP <= s2_SoP;
         if s2_SoP = '1' then
            s3_chksum_Partial <= chksum_Partial;
         end if;
      end if;
   end process;
   
   -- Add the carries to get the 16-bit 1-complement sum:  
   final_add: process (Clk)
      variable chksumPart_upper : std_logic_vector(15 downto 0);
   begin
      if (Clk'event and Clk='1') then
         ChksumValid <= s3_SoP;
         if s3_SoP = '1' then
            -- 0-extend to 16-bit the carry part of chksum_Partial (13 bits to the left):
            chksumPart_upper := '0' & x"000" & s3_chksum_Partial(18 downto 16);
            -- Make final one's complement sum and inversion:
            ChksumOut <= std_logic_vector (not(Add1Comp(s3_chksum_Partial(15 downto 0), chksumPart_upper)));
         end if;
      end if;
   end process;
end Behavioral;
