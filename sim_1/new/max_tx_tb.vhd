----------------------------------------------------------------------------------
-- Company: Pierrelec
-- Engineer: FREDERIC Pierre-Marie
-- 
-- Create Date: 19.09.2025 11:31:24
-- Design Name: 
-- Module Name: max_tx_tb - Behavioral
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

entity max_tx_tb is
--  Port ( );
end max_tx_tb;

architecture Behavioral of max_tx_tb is
-- CONSTANTS
constant SYS_PERIOD : time := 10ns;
constant MAC_PERIOD : time := 5ns;

-- SIGNALS
signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal tx_clk : std_logic := '1';
signal tx_rst : std_logic := '1';
signal tx_data_in : std_logic_vector(7 downto 0) := (others=>'0');
signal tx_data_valid : std_logic := '0';
signal tx_start : std_logic := '0';
signal tx_end : std_logic := '0';
signal tx_ready : std_logic;
signal tx_error : std_logic;
signal rgmii_tx_d : std_logic_vector(3 downto 0);
signal rgmii_tx_ctl : std_logic;
signal rgmii_tx_clk : std_logic;
signal pll_lock : std_logic;
signal clkfbout : std_logic;

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

    clk <= not(clk) after MAC_PERIOD/2;
    tx_clk <= not(tx_clk) after SYS_PERIOD/2;
    rst <= '0' after MAC_PERIOD;
    tx_rst <= '0' after SYS_PERIOD;
    
    process
    begin
        wait for SYS_PERIOD;
        tx_data_valid <= '1';
        tx_start <= '1';
        -- send destination address (0x12 12 12 12 12 12)
        for i in 0 to 5 loop
            if(i=1) then tx_start <= '0'; end if;
            tx_data_in <= x"12";
            wait for SYS_PERIOD;
        end loop;
        
        -- send source address (0xAB AB AB AB AB AB)
        for i in 0 to 5 loop
            tx_data_in <= x"AB";
            wait for SYS_PERIOD;
        end loop;
        
        -- send length (0x00 10)
        tx_data_in <= x"00";
        wait for SYS_PERIOD;
        tx_data_in <= x"10";
        wait for SYS_PERIOD;
        
        -- send payload (0x00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F)
        tx_data_in <= x"00";
        wait for SYS_PERIOD;
        tx_data_in <= x"01";
        wait for SYS_PERIOD;
        tx_data_in <= x"02";
        wait for SYS_PERIOD;
        tx_data_in <= x"03";
        wait for SYS_PERIOD;
        tx_data_in <= x"04";
        wait for SYS_PERIOD;
        tx_data_in <= x"05";
        wait for SYS_PERIOD;
        tx_data_in <= x"06";
        wait for SYS_PERIOD;
        tx_data_in <= x"07";
        wait for SYS_PERIOD;
        tx_data_in <= x"08";
        wait for SYS_PERIOD;
        tx_data_in <= x"09";
        wait for SYS_PERIOD;
        tx_data_in <= x"0A";
        wait for SYS_PERIOD;
        tx_data_in <= x"0B";
        wait for SYS_PERIOD;
        tx_data_in <= x"0C";
        wait for SYS_PERIOD;
        tx_data_in <= x"0D";
        wait for SYS_PERIOD;
        tx_data_in <= x"0E";
        wait for SYS_PERIOD;
        tx_data_in <= x"0F";
        tx_end <= '1';
        wait for SYS_PERIOD; 
        tx_data_valid <= '0';
        tx_end <= '0';
    end process;
    
end Behavioral;
