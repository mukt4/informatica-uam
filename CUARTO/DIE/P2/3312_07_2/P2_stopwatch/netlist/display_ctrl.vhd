-- Copyright 1986-2018 Xilinx, Inc. All Rights Reserved.
-- --------------------------------------------------------------------------------
-- Tool Version: Vivado v.2018.2 (win64) Build 2258646 Thu Jun 14 20:03:12 MDT 2018
-- Date        : Wed Oct  3 22:37:08 2018
-- Host        : Fernando-VAIO running 64-bit major release  (build 9200)
-- Command     : write_vhdl netlist/display_ctrl.vhd
-- Design      : display_ctrl
-- Purpose     : This VHDL netlist is a functional simulation representation of the design and should not be modified or
--               synthesized. This netlist cannot be used for SDF annotated simulation.
-- Device      : xc7z010clg400-1
-- --------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity display_ctrl is
  port (
    Clk : in STD_LOGIC;
    Reset : in STD_LOGIC;
    Tenths : in STD_LOGIC_VECTOR ( 3 downto 0 );
    Seconds : in STD_LOGIC_VECTOR ( 3 downto 0 );
    Tens : in STD_LOGIC_VECTOR ( 2 downto 0 );
    UpdateDisplay : in STD_LOGIC;
    SClk : out STD_LOGIC;
    RClk : out STD_LOGIC;
    Ser : out STD_LOGIC
  );
  attribute NotValidForBitStream : boolean;
  attribute NotValidForBitStream of display_ctrl : entity is true;
end display_ctrl;

architecture STRUCTURE of display_ctrl is
  signal \FSM_onehot_state[1]_i_1_n_0\ : STD_LOGIC;
  signal \FSM_onehot_state[1]_i_2_n_0\ : STD_LOGIC;
  signal \FSM_onehot_state[3]_i_1_n_0\ : STD_LOGIC;
  signal \FSM_onehot_state_reg_n_0_[0]\ : STD_LOGIC;
  attribute RTL_KEEP : string;
  attribute RTL_KEEP of \FSM_onehot_state_reg_n_0_[0]\ : signal is "yes";
  signal \FSM_onehot_state_reg_n_0_[1]\ : STD_LOGIC;
  attribute RTL_KEEP of \FSM_onehot_state_reg_n_0_[1]\ : signal is "yes";
  signal \FSM_onehot_state_reg_n_0_[2]\ : STD_LOGIC;
  attribute RTL_KEEP of \FSM_onehot_state_reg_n_0_[2]\ : signal is "yes";
  signal \FSM_onehot_state_reg_n_0_[3]\ : STD_LOGIC;
  attribute RTL_KEEP of \FSM_onehot_state_reg_n_0_[3]\ : signal is "yes";
  signal count : STD_LOGIC_VECTOR ( 4 downto 1 );
  signal \count[0]_i_1_n_0\ : STD_LOGIC;
  signal \count_reg_n_0_[0]\ : STD_LOGIC;
  signal \count_reg_n_0_[1]\ : STD_LOGIC;
  signal \count_reg_n_0_[2]\ : STD_LOGIC;
  signal \count_reg_n_0_[3]\ : STD_LOGIC;
  signal \count_reg_n_0_[4]\ : STD_LOGIC;
  signal \enCounter_reg__0\ : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal eqOp : STD_LOGIC;
  signal in5 : STD_LOGIC_VECTOR ( 23 downto 1 );
  signal plusOp : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal secondsReg : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal shiftReg : STD_LOGIC_VECTOR ( 23 downto 0 );
  signal \shiftReg[11]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[12]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[13]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[14]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[15]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[16]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[17]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[19]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[1]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[20]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[21]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[22]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[23]_i_1_n_0\ : STD_LOGIC;
  signal \shiftReg[23]_i_3_n_0\ : STD_LOGIC;
  signal \shiftReg[3]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[4]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[5]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[6]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[7]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[8]_i_2_n_0\ : STD_LOGIC;
  signal \shiftReg[9]_i_2_n_0\ : STD_LOGIC;
  signal tensReg : STD_LOGIC_VECTOR ( 2 downto 0 );
  signal tenthsReg : STD_LOGIC_VECTOR ( 3 downto 0 );
  attribute FSM_ENCODED_STATES : string;
  attribute FSM_ENCODED_STATES of \FSM_onehot_state_reg[0]\ : label is "capture:0001,shift0:0100,shift1:0010,load:1000";
  attribute KEEP : string;
  attribute KEEP of \FSM_onehot_state_reg[0]\ : label is "yes";
  attribute FSM_ENCODED_STATES of \FSM_onehot_state_reg[1]\ : label is "capture:0001,shift0:0100,shift1:0010,load:1000";
  attribute KEEP of \FSM_onehot_state_reg[1]\ : label is "yes";
  attribute FSM_ENCODED_STATES of \FSM_onehot_state_reg[2]\ : label is "capture:0001,shift0:0100,shift1:0010,load:1000";
  attribute KEEP of \FSM_onehot_state_reg[2]\ : label is "yes";
  attribute FSM_ENCODED_STATES of \FSM_onehot_state_reg[3]\ : label is "capture:0001,shift0:0100,shift1:0010,load:1000";
  attribute KEEP of \FSM_onehot_state_reg[3]\ : label is "yes";
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of \enCounter[0]_i_1\ : label is "soft_lutpair2";
  attribute SOFT_HLUTNM of \enCounter[1]_i_1\ : label is "soft_lutpair2";
  attribute SOFT_HLUTNM of \enCounter[2]_i_1\ : label is "soft_lutpair0";
  attribute SOFT_HLUTNM of \enCounter[3]_i_1\ : label is "soft_lutpair0";
  attribute SOFT_HLUTNM of \shiftReg[17]_i_2\ : label is "soft_lutpair1";
  attribute SOFT_HLUTNM of \shiftReg[19]_i_2\ : label is "soft_lutpair1";
begin
\FSM_onehot_state[1]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"E"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => \FSM_onehot_state[1]_i_2_n_0\,
      O => \FSM_onehot_state[1]_i_1_n_0\
    );
\FSM_onehot_state[1]_i_2\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"AAAAAAAA2AAAAAAA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[2]\,
      I1 => \count_reg_n_0_[4]\,
      I2 => \count_reg_n_0_[0]\,
      I3 => \count_reg_n_0_[1]\,
      I4 => \count_reg_n_0_[2]\,
      I5 => \count_reg_n_0_[3]\,
      O => \FSM_onehot_state[1]_i_2_n_0\
    );
\FSM_onehot_state[3]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"0000000080000000"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[2]\,
      I1 => \count_reg_n_0_[2]\,
      I2 => \count_reg_n_0_[4]\,
      I3 => \count_reg_n_0_[0]\,
      I4 => \count_reg_n_0_[1]\,
      I5 => \count_reg_n_0_[3]\,
      O => \FSM_onehot_state[3]_i_1_n_0\
    );
\FSM_onehot_state_reg[0]\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => Clk,
      CE => eqOp,
      D => \FSM_onehot_state_reg_n_0_[3]\,
      PRE => Reset,
      Q => \FSM_onehot_state_reg_n_0_[0]\
    );
\FSM_onehot_state_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => Clk,
      CE => eqOp,
      CLR => Reset,
      D => \FSM_onehot_state[1]_i_1_n_0\,
      Q => \FSM_onehot_state_reg_n_0_[1]\
    );
\FSM_onehot_state_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => Clk,
      CE => eqOp,
      CLR => Reset,
      D => \FSM_onehot_state_reg_n_0_[1]\,
      Q => \FSM_onehot_state_reg_n_0_[2]\
    );
\FSM_onehot_state_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => Clk,
      CE => eqOp,
      CLR => Reset,
      D => \FSM_onehot_state[3]_i_1_n_0\,
      Q => \FSM_onehot_state_reg_n_0_[3]\
    );
RClk_reg: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => eqOp,
      CLR => Reset,
      D => \FSM_onehot_state_reg_n_0_[3]\,
      Q => RClk
    );
SClk_i_1: unisim.vcomponents.LUT4
    generic map(
      INIT => X"0001"
    )
        port map (
      I0 => \enCounter_reg__0\(2),
      I1 => \enCounter_reg__0\(3),
      I2 => \enCounter_reg__0\(1),
      I3 => \enCounter_reg__0\(0),
      O => eqOp
    );
SClk_reg: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => eqOp,
      CLR => Reset,
      D => \FSM_onehot_state_reg_n_0_[1]\,
      Q => SClk
    );
\count[0]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[2]\,
      I1 => \count_reg_n_0_[0]\,
      O => \count[0]_i_1_n_0\
    );
\count[1]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"28"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[2]\,
      I1 => \count_reg_n_0_[1]\,
      I2 => \count_reg_n_0_[0]\,
      O => count(1)
    );
\count[2]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"2888"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[2]\,
      I1 => \count_reg_n_0_[2]\,
      I2 => \count_reg_n_0_[1]\,
      I3 => \count_reg_n_0_[0]\,
      O => count(2)
    );
\count[3]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"28888888"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[2]\,
      I1 => \count_reg_n_0_[3]\,
      I2 => \count_reg_n_0_[2]\,
      I3 => \count_reg_n_0_[0]\,
      I4 => \count_reg_n_0_[1]\,
      O => count(3)
    );
\count[4]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"2888888888888888"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[2]\,
      I1 => \count_reg_n_0_[4]\,
      I2 => \count_reg_n_0_[3]\,
      I3 => \count_reg_n_0_[1]\,
      I4 => \count_reg_n_0_[0]\,
      I5 => \count_reg_n_0_[2]\,
      O => count(4)
    );
\count_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      CLR => Reset,
      D => \count[0]_i_1_n_0\,
      Q => \count_reg_n_0_[0]\
    );
\count_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      CLR => Reset,
      D => count(1),
      Q => \count_reg_n_0_[1]\
    );
\count_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      CLR => Reset,
      D => count(2),
      Q => \count_reg_n_0_[2]\
    );
\count_reg[3]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      CLR => Reset,
      D => count(3),
      Q => \count_reg_n_0_[3]\
    );
\count_reg[4]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      CLR => Reset,
      D => count(4),
      Q => \count_reg_n_0_[4]\
    );
\enCounter[0]_i_1\: unisim.vcomponents.LUT1
    generic map(
      INIT => X"1"
    )
        port map (
      I0 => \enCounter_reg__0\(0),
      O => plusOp(0)
    );
\enCounter[1]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \enCounter_reg__0\(0),
      I1 => \enCounter_reg__0\(1),
      O => plusOp(1)
    );
\enCounter[2]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"78"
    )
        port map (
      I0 => \enCounter_reg__0\(0),
      I1 => \enCounter_reg__0\(1),
      I2 => \enCounter_reg__0\(2),
      O => plusOp(2)
    );
\enCounter[3]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"7F80"
    )
        port map (
      I0 => \enCounter_reg__0\(1),
      I1 => \enCounter_reg__0\(0),
      I2 => \enCounter_reg__0\(2),
      I3 => \enCounter_reg__0\(3),
      O => plusOp(3)
    );
\enCounter_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => '1',
      CLR => Reset,
      D => plusOp(0),
      Q => \enCounter_reg__0\(0)
    );
\enCounter_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => '1',
      CLR => Reset,
      D => plusOp(1),
      Q => \enCounter_reg__0\(1)
    );
\enCounter_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => '1',
      CLR => Reset,
      D => plusOp(2),
      Q => \enCounter_reg__0\(2)
    );
\enCounter_reg[3]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => '1',
      CLR => Reset,
      D => plusOp(3),
      Q => \enCounter_reg__0\(3)
    );
\secondsReg_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Seconds(0),
      Q => secondsReg(0)
    );
\secondsReg_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Seconds(1),
      Q => secondsReg(1)
    );
\secondsReg_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Seconds(2),
      Q => secondsReg(2)
    );
\secondsReg_reg[3]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Seconds(3),
      Q => secondsReg(3)
    );
\shiftReg[0]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"22020200"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => tenthsReg(3),
      I2 => tenthsReg(2),
      I3 => tenthsReg(0),
      I4 => tenthsReg(1),
      O => shiftReg(0)
    );
\shiftReg[10]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[2]\,
      I1 => in5(10),
      O => shiftReg(10)
    );
\shiftReg[11]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(11),
      I3 => \shiftReg[11]_i_2_n_0\,
      O => shiftReg(11)
    );
\shiftReg[11]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"A8A80020"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => secondsReg(2),
      I2 => secondsReg(1),
      I3 => secondsReg(0),
      I4 => secondsReg(3),
      O => \shiftReg[11]_i_2_n_0\
    );
\shiftReg[12]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(12),
      I3 => \shiftReg[12]_i_2_n_0\,
      O => shiftReg(12)
    );
\shiftReg[12]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00000220"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => secondsReg(3),
      I2 => secondsReg(2),
      I3 => secondsReg(0),
      I4 => secondsReg(1),
      O => \shiftReg[12]_i_2_n_0\
    );
\shiftReg[13]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(13),
      I3 => \shiftReg[13]_i_2_n_0\,
      O => shiftReg(13)
    );
\shiftReg[13]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"20000022"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => secondsReg(3),
      I2 => secondsReg(0),
      I3 => secondsReg(2),
      I4 => secondsReg(1),
      O => \shiftReg[13]_i_2_n_0\
    );
\shiftReg[14]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(14),
      I3 => \shiftReg[14]_i_2_n_0\,
      O => shiftReg(14)
    );
\shiftReg[14]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"20000220"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => secondsReg(3),
      I2 => secondsReg(2),
      I3 => secondsReg(0),
      I4 => secondsReg(1),
      O => \shiftReg[14]_i_2_n_0\
    );
\shiftReg[15]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(15),
      I3 => \shiftReg[15]_i_2_n_0\,
      O => shiftReg(15)
    );
\shiftReg[15]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"000888A8"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => secondsReg(0),
      I2 => secondsReg(2),
      I3 => secondsReg(1),
      I4 => secondsReg(3),
      O => \shiftReg[15]_i_2_n_0\
    );
\shiftReg[16]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"E"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \shiftReg[16]_i_2_n_0\,
      O => shiftReg(16)
    );
\shiftReg[16]_i_2\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"FFFFD400D400D400"
    )
        port map (
      I0 => tensReg(2),
      I1 => tensReg(0),
      I2 => tensReg(1),
      I3 => \FSM_onehot_state_reg_n_0_[0]\,
      I4 => in5(16),
      I5 => \FSM_onehot_state_reg_n_0_[2]\,
      O => \shiftReg[16]_i_2_n_0\
    );
\shiftReg[17]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"EAEAFFEAEAEAEAEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(17),
      I3 => \FSM_onehot_state_reg_n_0_[0]\,
      I4 => \shiftReg[17]_i_2_n_0\,
      I5 => tensReg(2),
      O => shiftReg(17)
    );
\shiftReg[17]_i_2\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"9"
    )
        port map (
      I0 => tensReg(1),
      I1 => tensReg(0),
      O => \shiftReg[17]_i_2_n_0\
    );
\shiftReg[18]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(18),
      I3 => \FSM_onehot_state_reg_n_0_[0]\,
      O => shiftReg(18)
    );
\shiftReg[19]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"EAEAEAEAEAEAFFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(19),
      I3 => \FSM_onehot_state_reg_n_0_[0]\,
      I4 => tensReg(2),
      I5 => \shiftReg[19]_i_2_n_0\,
      O => shiftReg(19)
    );
\shiftReg[19]_i_2\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"B"
    )
        port map (
      I0 => tensReg(0),
      I1 => tensReg(1),
      O => \shiftReg[19]_i_2_n_0\
    );
\shiftReg[1]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(1),
      I3 => \shiftReg[1]_i_2_n_0\,
      O => shiftReg(1)
    );
\shiftReg[1]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"AAA02800"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => tenthsReg(0),
      I2 => tenthsReg(1),
      I3 => tenthsReg(2),
      I4 => tenthsReg(3),
      O => \shiftReg[1]_i_2_n_0\
    );
\shiftReg[20]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"E"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \shiftReg[20]_i_2_n_0\,
      O => shiftReg(20)
    );
\shiftReg[20]_i_2\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"FFFF060006000600"
    )
        port map (
      I0 => tensReg(2),
      I1 => tensReg(0),
      I2 => tensReg(1),
      I3 => \FSM_onehot_state_reg_n_0_[0]\,
      I4 => in5(20),
      I5 => \FSM_onehot_state_reg_n_0_[2]\,
      O => \shiftReg[20]_i_2_n_0\
    );
\shiftReg[21]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"E"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \shiftReg[21]_i_2_n_0\,
      O => shiftReg(21)
    );
\shiftReg[21]_i_2\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"FFFF830083008300"
    )
        port map (
      I0 => tensReg(0),
      I1 => tensReg(2),
      I2 => tensReg(1),
      I3 => \FSM_onehot_state_reg_n_0_[0]\,
      I4 => in5(21),
      I5 => \FSM_onehot_state_reg_n_0_[2]\,
      O => \shiftReg[21]_i_2_n_0\
    );
\shiftReg[22]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"E"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \shiftReg[22]_i_2_n_0\,
      O => shiftReg(22)
    );
\shiftReg[22]_i_2\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"FFFF860086008600"
    )
        port map (
      I0 => tensReg(2),
      I1 => tensReg(0),
      I2 => tensReg(1),
      I3 => \FSM_onehot_state_reg_n_0_[0]\,
      I4 => in5(22),
      I5 => \FSM_onehot_state_reg_n_0_[2]\,
      O => \shiftReg[22]_i_2_n_0\
    );
\shiftReg[23]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"0001000100010000"
    )
        port map (
      I0 => \enCounter_reg__0\(0),
      I1 => \enCounter_reg__0\(1),
      I2 => \enCounter_reg__0\(3),
      I3 => \enCounter_reg__0\(2),
      I4 => \FSM_onehot_state_reg_n_0_[2]\,
      I5 => \FSM_onehot_state_reg_n_0_[0]\,
      O => \shiftReg[23]_i_1_n_0\
    );
\shiftReg[23]_i_2\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"E"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \shiftReg[23]_i_3_n_0\,
      O => shiftReg(23)
    );
\shiftReg[23]_i_3\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"FFFFF400F400F400"
    )
        port map (
      I0 => tensReg(1),
      I1 => tensReg(2),
      I2 => tensReg(0),
      I3 => \FSM_onehot_state_reg_n_0_[0]\,
      I4 => in5(23),
      I5 => \FSM_onehot_state_reg_n_0_[2]\,
      O => \shiftReg[23]_i_3_n_0\
    );
\shiftReg[2]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(2),
      I3 => \FSM_onehot_state_reg_n_0_[0]\,
      O => shiftReg(2)
    );
\shiftReg[3]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(3),
      I3 => \shiftReg[3]_i_2_n_0\,
      O => shiftReg(3)
    );
\shiftReg[3]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"A8A80020"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => tenthsReg(2),
      I2 => tenthsReg(1),
      I3 => tenthsReg(0),
      I4 => tenthsReg(3),
      O => \shiftReg[3]_i_2_n_0\
    );
\shiftReg[4]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(4),
      I3 => \shiftReg[4]_i_2_n_0\,
      O => shiftReg(4)
    );
\shiftReg[4]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00000220"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => tenthsReg(3),
      I2 => tenthsReg(2),
      I3 => tenthsReg(0),
      I4 => tenthsReg(1),
      O => \shiftReg[4]_i_2_n_0\
    );
\shiftReg[5]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(5),
      I3 => \shiftReg[5]_i_2_n_0\,
      O => shiftReg(5)
    );
\shiftReg[5]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"20000022"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => tenthsReg(3),
      I2 => tenthsReg(0),
      I3 => tenthsReg(2),
      I4 => tenthsReg(1),
      O => \shiftReg[5]_i_2_n_0\
    );
\shiftReg[6]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(6),
      I3 => \shiftReg[6]_i_2_n_0\,
      O => shiftReg(6)
    );
\shiftReg[6]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"20000220"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => tenthsReg(3),
      I2 => tenthsReg(2),
      I3 => tenthsReg(0),
      I4 => tenthsReg(1),
      O => \shiftReg[6]_i_2_n_0\
    );
\shiftReg[7]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(7),
      I3 => \shiftReg[7]_i_2_n_0\,
      O => shiftReg(7)
    );
\shiftReg[7]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"000888A8"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => tenthsReg(0),
      I2 => tenthsReg(2),
      I3 => tenthsReg(1),
      I4 => tenthsReg(3),
      O => \shiftReg[7]_i_2_n_0\
    );
\shiftReg[8]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(8),
      I3 => \shiftReg[8]_i_2_n_0\,
      O => shiftReg(8)
    );
\shiftReg[8]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"22020200"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => secondsReg(3),
      I2 => secondsReg(2),
      I3 => secondsReg(0),
      I4 => secondsReg(1),
      O => \shiftReg[8]_i_2_n_0\
    );
\shiftReg[9]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FFEA"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[1]\,
      I1 => \FSM_onehot_state_reg_n_0_[2]\,
      I2 => in5(9),
      I3 => \shiftReg[9]_i_2_n_0\,
      O => shiftReg(9)
    );
\shiftReg[9]_i_2\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"AAA02800"
    )
        port map (
      I0 => \FSM_onehot_state_reg_n_0_[0]\,
      I1 => secondsReg(0),
      I2 => secondsReg(1),
      I3 => secondsReg(2),
      I4 => secondsReg(3),
      O => \shiftReg[9]_i_2_n_0\
    );
\shiftReg_reg[0]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(0),
      PRE => Reset,
      Q => in5(1)
    );
\shiftReg_reg[10]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(10),
      PRE => Reset,
      Q => in5(11)
    );
\shiftReg_reg[11]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(11),
      PRE => Reset,
      Q => in5(12)
    );
\shiftReg_reg[12]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(12),
      PRE => Reset,
      Q => in5(13)
    );
\shiftReg_reg[13]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(13),
      PRE => Reset,
      Q => in5(14)
    );
\shiftReg_reg[14]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(14),
      PRE => Reset,
      Q => in5(15)
    );
\shiftReg_reg[15]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(15),
      PRE => Reset,
      Q => in5(16)
    );
\shiftReg_reg[16]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(16),
      PRE => Reset,
      Q => in5(17)
    );
\shiftReg_reg[17]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(17),
      PRE => Reset,
      Q => in5(18)
    );
\shiftReg_reg[18]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(18),
      PRE => Reset,
      Q => in5(19)
    );
\shiftReg_reg[19]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(19),
      PRE => Reset,
      Q => in5(20)
    );
\shiftReg_reg[1]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(1),
      PRE => Reset,
      Q => in5(2)
    );
\shiftReg_reg[20]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(20),
      PRE => Reset,
      Q => in5(21)
    );
\shiftReg_reg[21]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(21),
      PRE => Reset,
      Q => in5(22)
    );
\shiftReg_reg[22]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(22),
      PRE => Reset,
      Q => in5(23)
    );
\shiftReg_reg[23]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(23),
      PRE => Reset,
      Q => Ser
    );
\shiftReg_reg[2]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(2),
      PRE => Reset,
      Q => in5(3)
    );
\shiftReg_reg[3]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(3),
      PRE => Reset,
      Q => in5(4)
    );
\shiftReg_reg[4]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(4),
      PRE => Reset,
      Q => in5(5)
    );
\shiftReg_reg[5]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(5),
      PRE => Reset,
      Q => in5(6)
    );
\shiftReg_reg[6]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(6),
      PRE => Reset,
      Q => in5(7)
    );
\shiftReg_reg[7]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(7),
      PRE => Reset,
      Q => in5(8)
    );
\shiftReg_reg[8]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(8),
      PRE => Reset,
      Q => in5(9)
    );
\shiftReg_reg[9]\: unisim.vcomponents.FDPE
     port map (
      C => Clk,
      CE => \shiftReg[23]_i_1_n_0\,
      D => shiftReg(9),
      PRE => Reset,
      Q => in5(10)
    );
\tensReg_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Tens(0),
      Q => tensReg(0)
    );
\tensReg_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Tens(1),
      Q => tensReg(1)
    );
\tensReg_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Tens(2),
      Q => tensReg(2)
    );
\tenthsReg_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Tenths(0),
      Q => tenthsReg(0)
    );
\tenthsReg_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Tenths(1),
      Q => tenthsReg(1)
    );
\tenthsReg_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Tenths(2),
      Q => tenthsReg(2)
    );
\tenthsReg_reg[3]\: unisim.vcomponents.FDCE
     port map (
      C => Clk,
      CE => UpdateDisplay,
      CLR => Reset,
      D => Tenths(3),
      Q => tenthsReg(3)
    );
    
process
begin
   report "--------------------------------------------------------------------";
   report "------------------  USING NETLIST FOR display_ctrl -----------------";
   report "--------------------------------------------------------------------";
   wait;
end process;

end STRUCTURE;
