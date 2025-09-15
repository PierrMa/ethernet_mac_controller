----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 10.09.2025 23:38:58
-- Design Name: 
-- Module Name: mac_rx_sim - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity mac_rx_sim is
--  Port ( );
end mac_rx_sim;

architecture Behavioral of mac_rx_sim is
-- COMPONENT
component mac_rx is
generic(
    NB_BYTE_MAX : integer := 1518;
    NB_BYTE_MIN : integer := 64
);
Port ( 
    -- clock and reset
    clk : in std_logic;
    rst : in std_logic;
    
    -- rgmii_rx
    rgmii_rx_d : in std_logic_vector(3 downto 0); -- data
    rgmii_rx_ctl : in std_logic; -- data_valid + error
    rgmii_rx_clk : in std_logic; -- clk from the PHY
    
    -- system interface
    rx_data_out : out std_logic_vector(7 downto 0);
    rx_data_valid : out std_logic; -- '1' = data valid
    rx_error : out std_logic; -- '1' = error detected
    rx_start : out std_logic; -- '1' = start of frame
    rx_end : out std_logic; -- '1' = end of frame
    --      for debug
    frame_length : out std_logic_vector(15 downto 0);
    crc_ok : out std_logic -- '1' = crc ok
);
end component;

-- CONSTANTS
constant NB_BYTE_MAX : integer := 1518;
constant NB_BYTE_MIN : integer := 64;
constant SYS_CLK_PERIOD : time := 10ns; -- f_clk = 100MhZ
constant PHY_CLK_PERIOD : time := 8ns; -- f_phy = 125MHz
constant RIGHT_FCS : bit :='1'; -- '1' to send the correct FCS, '0' otherwise


-- SIGNALS
signal clk, rst : std_logic := '1';
signal rgmii_rx_d : std_logic_vector(3 downto 0) := (others=>'0');
signal rgmii_rx_ctl : std_logic := '0';
signal rgmii_rx_clk : std_logic := '1';
signal rx_data_out : std_logic_vector(7 downto 0);
signal rx_data_valid : std_logic := '0';
signal rx_error : std_logic;
signal rx_start : std_logic;
signal rx_end : std_logic;
signal frame_length : std_logic_vector(15 downto 0);
signal crc_ok : std_logic;

begin

    mac_rx_inst : entity work.mac_rx
        generic map(
            NB_BYTE_MAX => NB_BYTE_MAX,
            NB_BYTE_MIN => NB_BYTE_MIN)
        port map(
            clk => clk,
            rst => rst,
            rgmii_rx_d => rgmii_rx_d,
            rgmii_rx_ctl => rgmii_rx_ctl,
            rgmii_rx_clk => rgmii_rx_clk,
            rx_data_out => rx_data_out,
            rx_data_valid => rx_data_valid,
            rx_error => rx_error,
            rx_start => rx_start,
            rx_end => rx_end,
            frame_length => frame_length,
            crc_ok => crc_ok);

    clk <= not(clk) after SYS_CLK_PERIOD/2;
    rst <= '0' after SYS_CLK_PERIOD;
    rgmii_rx_clk <= not(rgmii_rx_clk) after PHY_CLK_PERIOD/2;
    
    process
    begin
        wait for PHY_CLK_PERIOD*2.75; -- do nothing during reset
        rgmii_rx_ctl <= '1'; --indicate the begining of the transmission
        
        -- send preamble
        for i in 0 to 6 loop
            rgmii_rx_d <= x"5";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"5";
            wait for PHY_CLK_PERIOD/2;
        end loop;
        
        -- send SFD
        rgmii_rx_d <= x"D";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"5";
        wait for PHY_CLK_PERIOD/2;
        
        -- send destination address (0x12 12 12 12 12 12)
        for i in 0 to 5 loop
            rgmii_rx_d <= x"1";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"2";
            wait for PHY_CLK_PERIOD/2;
        end loop;
        
        -- send source address (0xAB AB AB AB AB AB)
        for i in 0 to 5 loop
            rgmii_rx_d <= x"A";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"B";
            wait for PHY_CLK_PERIOD/2;
        end loop;
        
        -- send length (0x00 10)
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"1";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        
        -- send payload (0x00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F)
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"1";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"2";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"3";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"4";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"5";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"6";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"7";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"8";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"9";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"A";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"B";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"C";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"D";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"E";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"0";
        wait for PHY_CLK_PERIOD/2;
        rgmii_rx_d <= x"F";
        wait for PHY_CLK_PERIOD/2;
        
        -- send FCS (0x3E 0x6D 0x5D 0x16) correct value
        if RIGHT_FCS = '1' then
            rgmii_rx_d <= x"3";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"E";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"6";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"D";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"5";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"D";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"1";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"6";
            wait for PHY_CLK_PERIOD/2;
        else
            rgmii_rx_d <= x"0";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"1";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"2";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"3";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"4";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"5";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"6";
            wait for PHY_CLK_PERIOD/2;
            rgmii_rx_d <= x"7";
            wait for PHY_CLK_PERIOD/2;
        end if;
        
        -- indicate the end of the transmission
        rgmii_rx_ctl <= '0';
        wait for PHY_CLK_PERIOD/2;
    end process;
    
end Behavioral;
