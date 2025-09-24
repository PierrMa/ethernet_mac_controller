----------------------------------------------------------------------------------
-- Company: Pierrelec
-- Engineer: FREDERIC Pierre-Marie
-- 
-- Create Date: 24.09.2025 11:10:56
-- Design Name: 
-- Module Name: top_mac - Behavioral
-- Project Name: Ethernet MAC Controller 
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

entity top_mac is
Port ( 
    -- MAC clock and reset
    clk: in std_logic;
    rst : in std_logic;
    -- System interface
    --   Tx
    tx_clk : in std_logic;
    tx_rst : in std_logic;
    tx_data_in : in std_logic_vector(7 downto 0);
    tx_data_valid : in std_logic;
    tx_start : in std_logic;
    tx_end : in std_logic;
    tx_ready : out std_logic;
    tx_error : out std_logic;
    --   Rx
    rx_data_out : out std_logic_vector(7 downto 0);
    rx_data_valid : out std_logic; -- '1' = data valid
    rx_error : out std_logic; -- '1' = error detected
    rx_start : out std_logic; -- '1' = start of frame
    rx_end : out std_logic; -- '1' = end of frame
    -- RGMII interface
    --   Tx
    rgmii_tx_d : out std_logic_vector(3 downto 0);
    rgmii_tx_ctl : out std_logic;
    rgmii_tx_clk : out std_logic;
    --   Rx
    rgmii_rx_d : in std_logic_vector(3 downto 0); -- data
    rgmii_rx_ctl : in std_logic; -- data_valid + error
    rgmii_rx_clk : in std_logic; -- clk from the PHY
    -- Debug
    --   Tx
    pll_lock : out std_logic;
    clkfbout : out std_logic;
    --   Rx
    frame_length : out std_logic_vector(15 downto 0);
    crc_ok : out std_logic -- '1' = crc ok
);
end top_mac;

architecture Behavioral of top_mac is
constant NB_BYTE_MAX : integer := 1518;
constant NB_BYTE_MIN : integer := 64;
begin

    mac_tx : entity work.mac_tx
    port map(
        -- MAC signals
        clk => clk,
        rst => rst,
        -- System interface
        tx_clk => tx_clk,
        tx_rst => tx_rst,
        tx_data_in => tx_data_in,
        tx_data_valid => tx_data_valid,
        tx_start => tx_start,
        tx_end => tx_end,
        tx_ready => tx_ready,
        tx_error => tx_error,
        -- PHY interface
        rgmii_tx_d => rgmii_tx_d,
        rgmii_tx_ctl => rgmii_tx_ctl,
        rgmii_tx_clk => rgmii_tx_clk,
        -- debub
        pll_lock => pll_lock,
        clkfbout => clkfbout);
        
    mac_rx : entity work.mac_rx
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

end Behavioral;
