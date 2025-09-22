----------------------------------------------------------------------------------
-- Company: Pierrelec
-- Engineer: FREDERIC Pierre-Marie
-- 
-- Create Date: 17.09.2025 10:40:43
-- Design Name: 
-- Module Name: mac_tx - Behavioral
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
use IEEE.NUMERIC_STD.ALL;
Library UNISIM;
use UNISIM.vcomponents.all;

entity mac_tx is
Port ( 
    -- MAC signals
    clk : std_logic;
    rst : std_logic;
    -- System interface
    tx_clk : in std_logic;
    tx_rst : in std_logic;
    tx_data_in : in std_logic_vector(7 downto 0);
    tx_data_valid : in std_logic;
    tx_start : in std_logic;
    tx_end : in std_logic;
    tx_ready : out std_logic;
    tx_error : out std_logic;
    -- PHY interface
    rgmii_tx_d : out std_logic_vector(3 downto 0);
    rgmii_tx_ctl : out std_logic;
    rgmii_tx_clk : out std_logic;
    -- debub
    pll_lock : out std_logic;
    clkfbout : out std_logic
);
end mac_tx;

architecture Behavioral of mac_tx is
-- CONSTANTS
--   fifo dimensions
constant SYS_FIFO_WIDTH : integer := 10;
constant SYS_FIFO_DEPTH : integer := 2048;
constant PHY_FIFO_WIDTH : integer := 4;
constant PHY_FIFO_DEPTH : integer := 4096;
--   size for each field of the frame (in nibble)
constant PREAMB_LEN : integer := 14;
constant SFD_LEN : integer := 2;
constant DEST_LEN : integer := 12;
constant SRC_LEN : integer := 12;
constant LENGTH_LEN : integer := 4;
constant PAYLOAD_MAX_LEN : integer := 3000;
constant FCS_LEN : integer := 8;
constant NB_BYTE_MAX : integer := 3028;
constant NB_BYTE_MIN : integer := 120;

-- TYPES
type fsm_t is (IDLE, ADD_PREAMB, ADD_SFD, ADD_DEST_ADDR, ADD_SRC_ADDR, ADD_LEN, ADD_PAYLOAD, ADD_PADDING, ADD_FCS, WAIT_FIELD);

-- SIGNALS
--   async_fifo_sys2mac signals
signal sys_w_data : std_logic_vector(SYS_FIFO_WIDTH-1 downto 0); -- sys_w_data(9 downto 2) = tx_data_in, sys_w_data(1) = tx_start, sys_w_data(0) = tx_end;
signal sys_w_data_sync : std_logic_vector(SYS_FIFO_WIDTH-1 downto 0);
signal mac_r_en : std_logic;
signal sys2mac_full : std_logic;
signal sys2mac_empty : std_logic;
--   FSM signals
signal state : fsm_t;
signal index : integer range 0 to 1500 :=0;
signal transmission_err : std_logic;
signal payload_length : std_logic_vector(LENGTH_LEN*4-1 downto 0) := (others=>'0');
signal padding_length : integer range 0 to 46 := 0;
signal crc : std_logic_vector(31 downto 0) := (others => '1');
--   async_fifo_mac2phy signals
signal mac_w_data : std_logic_vector(PHY_FIFO_WIDTH-1 downto 0);
signal mac_w_en : std_logic;
signal phy_r_en : std_logic := '0';
signal mac2phy_full : std_logic;
signal mac2phy_empty : std_logic;
signal mac2phy_index : integer range 0 to 3052 :=0; -- 3052 = (max size of a frame)*2 = 1526*2
signal rgmii_tx_clk_s : std_logic;
--   RGMII signal
signal rgmii_en : std_logic;

-- FUNCTIONS
function compute_crc32(
    data  : std_logic_vector(7 downto 0);
    crc_in : std_logic_vector(31 downto 0)
) return std_logic_vector is
    variable crc  : unsigned(31 downto 0);
    variable din  : unsigned(7 downto 0);
begin
    crc := unsigned(crc_in);
    din := unsigned(data);

    for i in 0 to 7 loop
        if ((crc(0) xor din(i)) = '1') then
            crc := ('0'&crc(31 downto 1)) xor x"EDB88320"; -- right shift and xor with the reflected polynome
        else
            crc := '0'&crc(31 downto 1); -- right shift
        end if;
    end loop;

    return std_logic_vector(crc);
end function;

begin
    -- Async FIFO to handle CDC from the system to the MAC
    sys_w_data(9 downto 2) <= tx_data_in;
    sys_w_data(1) <= tx_start; 
    sys_w_data(0) <= tx_end; 
    async_fifo_sys2mac : entity work.async_fifo
    generic map(
        width => SYS_FIFO_WIDTH,
        depth => SYS_FIFO_DEPTH)
    port map(
        w_data => sys_w_data,
        w_rst => tx_rst,
        w_clk => tx_clk,
        w_en => tx_data_valid,
        r_data => sys_w_data_sync,
        r_rst => rst,
        r_clk => clk,
        r_en => mac_r_en,
        full => sys2mac_full,
        empty => sys2mac_empty);
    
    mac_r_en <= '1' when (state /= IDLE) and  (state /= ADD_PREAMB) and (state /= ADD_SFD) and (state /= ADD_PADDING) and (state /= ADD_FCS) 
    and (index mod 2 = 0) else '0'; -- this condition is because bytes are then treated as nibbles so the high half of a byte is registered when index is even and the low half when index is odd
    
    -- FSM to build the frame
    process(clk,rst)
    begin
        if rst = '1' then 
            state <= IDLE;
        elsif rising_edge(clk) then 
            case(state) is
            when IDLE =>
                transmission_err <= '0';
                crc <= (others=>'1');
                mac_w_en <= '0';
                index <= 0;
                if mac2phy_empty = '1' then -- each time mac2phy FIFO is empty, write PREAMBULE + SFD into it
                    state <= ADD_PREAMB;
                else
                    state <= IDLE;
                end if;
                
            when ADD_PREAMB =>
                if mac2phy_full /= '1' then
                    mac_w_en <= '1';
                    transmission_err <= '0';
                    if index < PREAMB_LEN-1 then 
                        state <= ADD_PREAMB; -- state
                        mac_w_data <= x"A"; -- data
                        index <= index + 1; -- index
                    elsif index = PREAMB_LEN-1 then
                        state <= ADD_SFD;
                        mac_w_data <= x"A";
                        index <= 0;
                    end if;
                else
                    state <= IDLE;
                    
                    mac_w_en <= '0';
                    transmission_err <= '1';
                    
                    index <= 0;
                end if;
            
            when ADD_SFD =>
                if mac2phy_full /= '1' then
                    mac_w_en <= '1';
                    transmission_err <= '0';
                    if index = 0 then 
                        state <= ADD_SFD;
                        mac_w_data <= x"A";
                        index <= index + 1;
                    elsif index = 1 then
                        state <= WAIT_FIELD;
                        
                        mac_w_data <= x"B";
                        crc <= (others=>'1');
                        
                        index <= 0;
                    end if;
                else
                    state <= IDLE;
                    
                    mac_w_en <= '0';
                    transmission_err <= '1';
                end if;
            
            when WAIT_FIELD =>
                if sys2mac_empty /= '1' then -- check if sys2mac fifo is not empty
                    if mac2phy_full /= '1' then-- check if mac2phy fifo is not full
                        if sys_w_data_sync(1) = '1' then -- handling start signal
                            state <= ADD_DEST_ADDR;
                            
                            transmission_err <= '0';
                            mac_w_en <= '1';
                            mac_w_data <= sys_w_data_sync(9 downto 6);
                            crc <= compute_crc32(sys_w_data_sync(9 downto 2),crc); -- compute CRC
                            
                            index <= index+1;
                        else
                            state <= WAIT_FIELD;
                            mac_w_en <= '0';
                            index <= 0;
                        end if;   
                    else
                        state <= IDLE;
                        mac_w_en <= '0';
                        transmission_err <= '1';
                        index <= 0;
                    end if;
                else
                    state <= IDLE;
                    
                    mac_w_en <= '0';
                    transmission_err <= '1';
                    index <= 0;
                end if;
                   
            when ADD_DEST_ADDR =>
                if sys2mac_empty /= '1' then -- check if sys2mac fifo is not empty
                    if mac2phy_full /= '1' then -- check if mac2phy fifo is not full
                        mac_w_en <= '1';
                        transmission_err <= '0';
                        
                        if index = DEST_LEN-1 then
                            state <= ADD_SRC_ADDR;
                            index <= 0;
                        elsif index < DEST_LEN-1 then
                            state <= ADD_DEST_ADDR;
                            index <= index+1;
                        end if;
                        
                        -- byte to nibble transmission
                        if index mod 2 = 0 then
                            mac_w_data <= sys_w_data_sync(9 downto 6);
                            --mac_r_en <= '1';
                            crc <= compute_crc32(sys_w_data_sync(9 downto 2),crc); -- compute CRC
                        elsif index mod 2 = 1 then
                            mac_w_data <= sys_w_data_sync(5 downto 2);
                            --mac_r_en <= '0';
                        end if;
                    else
                        state <= IDLE;
                        
                        mac_w_en <= '0';
                        transmission_err <= '1';
                    end if;
                else
                    state <= IDLE;
                    
                    mac_w_en <= '0';
                    transmission_err <= '1';
                end if;             
            
            when ADD_SRC_ADDR =>
                if sys2mac_empty /= '1' then -- check if sys2mac fifo is not empty
                    if mac2phy_full /= '1' then -- check if mac2phy fifo is not full
                        mac_w_en <= '1';
                        transmission_err <= '0';
                        
                        if index = SRC_LEN-1 then
                            state <= ADD_LEN;
                            index <= 0;
                        elsif index < SRC_LEN-1 then
                            state <= ADD_SRC_ADDR;
                            index <= index+1;
                        end if;
                        
                        -- byte to nibble transmission
                        if index mod 2 = 0 then
                            mac_w_data <= sys_w_data_sync(9 downto 6);
                            crc <= compute_crc32(sys_w_data_sync(9 downto 2),crc); -- compute CRC
                        elsif index mod 2 = 1 then
                            mac_w_data <= sys_w_data_sync(5 downto 2);
                        end if;
                    else
                        state <= IDLE;
                        
                        mac_w_en <= '0';
                        transmission_err <= '1';
                    end if;
                else
                    state <= IDLE;
                    
                    mac_w_en <= '0';
                    transmission_err <= '1';
                end if;
                
            when ADD_LEN =>
                if sys2mac_empty /= '1' then -- check if sys2mac fifo is not empty
                    if mac2phy_full /= '1' then -- check if mac2phy fifo is not full
                        mac_w_en <= '1';
                        
                        if index = LENGTH_LEN-1 then
                            -- check if payload size is correct
                            if to_integer(unsigned(payload_length)) > NB_BYTE_MAX then
                                state <= IDLE;
                                transmission_err <= '1';
                            else
                                state <= ADD_PAYLOAD;
                                transmission_err <= '0';
                            end if;
                            index <= 0;
                        elsif index < LENGTH_LEN-1 then
                            state <= ADD_LEN;
                            index <= index+1;
                        end if;
                        
                        -- byte to nibble transmission
                        if index mod 2 = 0 then
                            mac_w_data <= sys_w_data_sync(9 downto 6);
                            crc <= compute_crc32(sys_w_data_sync(9 downto 2),crc); -- compute CRC
                            transmission_err <= '0';
                        elsif index mod 2 = 1 then
                            mac_w_data <= sys_w_data_sync(5 downto 2);
                            transmission_err <= '0';
                        end if;
                        
                        -- register payload length to use it into the next state
                        if index = 0 then payload_length <= sys_w_data_sync(9 downto 2)&x"00";
                        elsif index = 2 then payload_length(7 downto 0) <= sys_w_data_sync(9 downto 2);
                        end if;
                        
                    else
                        state <= IDLE;
                        transmission_err <= '1';
                    end if;
                else
                    state <= IDLE;
                    transmission_err <= '1';
                end if;
                        
            when ADD_PAYLOAD =>
                if sys2mac_empty /= '1' then -- check if sys2mac fifo is not empty
                    if mac2phy_full /= '1' then-- check if mac2phy fifo is not full
                        if to_integer(unsigned(payload_length)) = 0 or index = to_integer(unsigned(payload_length))*2-1 then -- payload null or last index
                            if to_integer(unsigned(payload_length)) < NB_BYTE_MIN then
                                state <= ADD_PADDING;
                            else
                                state <= ADD_FCS;
                            end if;
                        elsif index = to_integer(unsigned(payload_length))*2-2 then
                            if sys_w_data_sync(0) = '1' then -- end of frame signal
                                state <= ADD_PAYLOAD;
                            else 
                                state <= IDLE;
                            end if;
                        elsif index < to_integer(unsigned(payload_length))*2-1 then
                            state <= ADD_PAYLOAD;
                        end if;    
                        
                        if to_integer(unsigned(payload_length)) > 0 then
                            mac_w_en <= '1';
                            transmission_err <= '0';
                            -- byte to nibble transmission
                            if index mod 2 = 0 then
                                mac_w_data <= sys_w_data_sync(9 downto 6);
                                crc <= compute_crc32(sys_w_data_sync(9 downto 2),crc); -- compute CRC
                            elsif index mod 2 = 1 then
                                mac_w_data <= sys_w_data_sync(5 downto 2);
                            end if;
                            
                            -- handling index
                            if index = to_integer(unsigned(payload_length))*2-2 then
                                if sys_w_data_sync(0) = '1' then -- end of frame signal
                                    transmission_err <= '0';
                                else
                                    transmission_err <= '1';
                                end if;
                            end if;
                            
                            if index = to_integer(unsigned(payload_length))*2-1 then
                                index <= 0;
                            elsif index < to_integer(unsigned(payload_length))*2-1 then
                                index <= index+1;
                            end if;
                        end if; 
                        
                        -- check payload size to decide what is next
                        if to_integer(unsigned(payload_length)) < NB_BYTE_MIN then
                            padding_length <= NB_BYTE_MIN - DEST_LEN - SRC_LEN - LENGTH_LEN - to_integer(unsigned(payload_length))*2;
                        end if;         
                    else
                        state <= IDLE;
                        mac_w_en <= '0';
                        transmission_err <= '1';
                    end if;
                else
                    state <= IDLE;
                    mac_w_en <= '0';
                    transmission_err <= '1';
                end if;
               
            when ADD_PADDING =>
                if mac2phy_full /= '1' then
                    if index = padding_length-1 then
                        state <= ADD_FCS;
                        index <= 0;
                    elsif index < padding_length-1 then
                        state <= ADD_PADDING;
                        index <= index+1;
                    end if;
                    
                    -- byte to nibble transmission
                    mac_w_en <= '1';
                    mac_w_data <= x"0";
                    transmission_err <= '0';
                    
                    -- compute CRC
                    if index mod 2 = 0 then 
                        crc <= compute_crc32(x"00",crc);
                    end if;
                else
                    state <= IDLE;
                    mac_w_en <= '0';
                    transmission_err <= '1';
                end if;
                
            when ADD_FCS => 
                if mac2phy_full /= '1' then-- check if mac2phy fifo is not full
                    if index = FCS_LEN-1 then
                        state <= IDLE;
                        index <= 0;
                    elsif index < FCS_LEN-1 then
                        state <= ADD_FCS;
                        index <= index+1;
                    end if;
                    
                    mac_w_en <= '1';
                    transmission_err <= '0';
                    -- byte to nibble transmission
                    if index = 0 then
                        mac_w_data <= crc(7 downto 4);
                    elsif index = 1 then
                        mac_w_data <= crc(3 downto 0);
                    elsif index = 2 then
                        mac_w_data <= crc(15 downto 12);
                    elsif index = 3 then
                        mac_w_data <= crc(11 downto 8);
                    elsif index = 4 then
                        mac_w_data <= crc(23 downto 20);
                    elsif index = 5 then
                        mac_w_data <= crc(19 downto 16);
                    elsif index = 6 then
                        mac_w_data <= crc(31 downto 28);
                    elsif index = 7 then
                        mac_w_data <= crc(27 downto 24);
                    end if;
                else
                    state <= IDLE;
                    mac_w_en <= '0';
                    transmission_err <= '1';
                end if;
            end case;
        end if;
    end process;
    
    -- PLL to generate a 125MHz clock for RGMII from MAC clock
    PLLE3_BASE_inst : PLLE3_BASE
       generic map (
          CLKFBOUT_MULT => 5,         -- Multiply value for all CLKOUT, (1-19)
          CLKFBOUT_PHASE => 0.0,      -- Phase offset in degrees of CLKFB, (-360.000-360.000)
          CLKIN_PERIOD => 5.0,        -- Input clock period in ns to ps resolution (i.e., 33.333 is 30 MHz).
          -- CLKOUT0 Attributes: Divide, Phase and Duty Cycle for the CLKOUT0 output
          CLKOUT0_DIVIDE => 8,        -- Divide amount for CLKOUT0 (1-128)
          CLKOUT0_DUTY_CYCLE => 0.5,  -- Duty cycle for CLKOUT0 (0.001-0.999)
          CLKOUT0_PHASE => 0.0,       -- Phase offset for CLKOUT0 (-360.000-360.000)
          -- CLKOUT1 Attributes: Divide, Phase and Duty Cycle for the CLKOUT1 output
          CLKOUT1_DIVIDE => 1,        -- Divide amount for CLKOUT1 (1-128)
          CLKOUT1_DUTY_CYCLE => 0.5,  -- Duty cycle for CLKOUT1 (0.001-0.999)
          CLKOUT1_PHASE => 0.0,       -- Phase offset for CLKOUT1 (-360.000-360.000)
          CLKOUTPHY_MODE => "VCO_2X", -- Frequency of the CLKOUTPHY (VCO, VCO_2X, VCO_HALF)
          DIVCLK_DIVIDE => 1,         -- Master division value, (1-15)
          -- Programmable Inversion Attributes: Specifies built-in programmable inversion on specific pins
          IS_CLKFBIN_INVERTED => '0', -- Optional inversion for CLKFBIN
          IS_CLKIN_INVERTED => '0',   -- Optional inversion for CLKIN
          IS_PWRDWN_INVERTED => '0',  -- Optional inversion for PWRDWN
          IS_RST_INVERTED => '0',     -- Optional inversion for RST
          REF_JITTER => 0.0,          -- Reference input jitter in UI (0.000-0.999)
          STARTUP_WAIT => "FALSE"     -- Delays DONE until PLL is locked (FALSE, TRUE)
       )
       port map (
          -- Clock Outputs outputs: User configurable clock outputs
          CLKOUT0 => rgmii_tx_clk_s,         -- 1-bit output: General Clock output
          CLKOUT0B => open,       -- 1-bit output: Inverted CLKOUT0
          CLKOUT1 => open,         -- 1-bit output: General Clock output
          CLKOUT1B => open,       -- 1-bit output: Inverted CLKOUT1
          CLKOUTPHY => open,     -- 1-bit output: Bitslice clock
          -- Feedback Clocks outputs: Clock feedback ports
          CLKFBOUT => clkfbout,       -- 1-bit output: Feedback clock
          LOCKED => pll_lock,           -- 1-bit output: LOCK
          CLKIN => clk,             -- 1-bit input: Input clock
          -- Control Ports inputs: PLL control ports
          CLKOUTPHYEN => '0', -- 1-bit input: CLKOUTPHY enable
          PWRDWN => '0',           -- 1-bit input: Power-down
          RST => rst,                 -- 1-bit input: Reset
          -- Feedback Clocks inputs: Clock feedback ports
          CLKFBIN => clk          -- 1-bit input: Feedback clock
       );
    
    -- Async FIFO to handle CDC from the MAC to the PHY
    asyn_fifo_mac2phy : entity work.async_fifo
    generic map(
        width => PHY_FIFO_WIDTH,
        depth => PHY_FIFO_DEPTH)
    port map(
        w_data => mac_w_data,
        w_rst => rst,
        w_clk => clk,
        w_en => mac_w_en,
        r_data => rgmii_tx_d,
        r_rst => '0',
        r_clk => rgmii_tx_clk_s,
        r_en => phy_r_en,
        full => mac2phy_full,
        empty => mac2phy_empty);
        
    rgmii_tx_clk <= rgmii_tx_clk_s;
    
    -- process to handle the reading of mac2phy fifo
    process (rgmii_tx_clk_s)
    begin
        if rising_edge(rgmii_tx_clk_s) then 
            if(mac2phy_empty = '0') then 
                if(sys_w_data_sync(0)='1') then -- if there is a full tram into sys2mac fifo
                    phy_r_en <= '1';
                end if;
            end if;
        end if;
        if falling_edge(rgmii_tx_clk_s) then 
            if(mac2phy_index = 1) then 
                phy_r_en <= '0';
            end if;
        end if;
    end process;
    
    -- process to handle rgmii_tx_ctl and index
    process (rgmii_tx_clk_s)
    begin
        if rising_edge(rgmii_tx_clk_s) then 
            if (mac2phy_empty = '0') and (sys_w_data_sync(0)='1') then
                mac2phy_index <= PREAMB_LEN + SFD_LEN + DEST_LEN + SRC_LEN + LENGTH_LEN + to_integer(unsigned(payload_length)) + padding_length + FCS_LEN;
            end if;
            if(mac2phy_empty = '0') and (phy_r_en = '1') then
                rgmii_tx_ctl <= '1';
                rgmii_en <= '1'; 
                mac2phy_index <= mac2phy_index - 1;
            end if;
        elsif falling_edge(rgmii_tx_clk_s) then 
            if(mac2phy_empty = '0') and (phy_r_en = '1') then
                rgmii_tx_ctl <= rgmii_en xor transmission_err; 
                mac2phy_index <= mac2phy_index - 1;
            end if;
        end if;
    end process;
    tx_ready <= '1' when state = IDLE and sys2mac_full /= '1';
    tx_error <= transmission_err;
end Behavioral;
