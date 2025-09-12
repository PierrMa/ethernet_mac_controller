----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 08.09.2025 11:15:42
-- Design Name: 
-- Module Name: mac_rx - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

entity mac_rx is
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
end mac_rx;

architecture Behavioral of mac_rx is
    -- CONSTANTS
    --  cdc fifo parameters
    constant FIFO_DEPTH : integer := 2048;
    constant FIFO_WIDTH : integer := 8;
    --  size for each field of the frame (in byte)
    constant PREAMB_LEN : integer := 7;
    constant SFD_LEN : integer := 1;
    constant DEST_LEN : integer := 6;
    constant SRC_LEN : integer := 6;
    constant LENGTH_LEN : integer := 2;
    constant PAYLOAD_MAX_LEN : integer := 1500;
    constant FCS_LEN : integer := 4;
    
    -- TYPES
    type FSM_type is (IDLE,PREAMB, RCV_DEST, RCV_SRC, RCV_LEN, RCV_PAYLOAD, RCV_FCS, CHECK_CRC);
    
    -- SIGNALS
    -- for the conversion from nibble to byte
    signal high_nibble : std_logic_vector(3 downto 0);
    signal rgmii_rx_d8 : std_logic_vector(FIFO_WIDTH-1 downto 0);
    -- for CDC
    signal w_en : std_logic;
    signal rgmii_rx_d8_sync : std_logic_vector(FIFO_WIDTH-1 downto 0);
    signal r_en : std_logic;
    signal full : std_logic;
    signal empty : std_logic;
    -- for the reception
    signal state : FSM_type;
    signal rx_dv : std_logic;  -- ongoing frame transfert
    signal phy_error, phy_error_sync1, phy_error_sync2 : std_logic; -- error get from phy in rgmii_rx_ctl
    signal reception_error : std_logic; -- error during reception
    signal index : integer range 0 to NB_BYTE_MAX;
    signal data_len : std_logic_vector(LENGTH_LEN*8-1 downto 0);
    signal fcs : std_logic_vector(FCS_LEN*8-1 downto 0);
    signal crc_processed : std_logic_vector(31 downto 0);
    signal rx_start_comb, rx_start_seq : std_logic; 
    signal rx_end_comb, rx_end_seq : std_logic; 
    
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
                crc := crc(30 downto 0)&'0' xor x"EDB88320"; -- polynôme inversé
            else
                crc := crc(30 downto 0)&'0';
            end if;
        end loop;
    
        return std_logic_vector(crc);
    end function;
begin
     -- crossing clock domain from rgmii_rx_clk to clk
    async_fifo_phy2mac : entity work.async_fifo
    generic map(
        width => FIFO_WIDTH,
        depth => FIFO_DEPTH
    )
    port map(
        w_data => rgmii_rx_d8,
        w_rst => '0',
        w_clk => rgmii_rx_clk,
        w_en => w_en,
        r_data => rgmii_rx_d8_sync,
        r_rst => rst,
        r_clk => clk,
        r_en => r_en,
        full => full,
        empty => empty
    );
    
    process(clk,rst)
    begin
        if rst = '1' then
            phy_error_sync1 <= '0';
            phy_error_sync2 <= '0';
        elsif rising_edge(clk) then
            phy_error_sync1  <= phy_error;
            phy_error_sync2  <= phy_error_sync1;
        end if;
    end process;
    
    -- Nibble to byte
    process(rgmii_rx_clk)
    begin
        if rising_edge(rgmii_rx_clk) and rgmii_rx_ctl = '1' then 
            high_nibble <= rgmii_rx_d;
        end if;
    end process;
    
    process(rgmii_rx_clk)
    begin
        if falling_edge(rgmii_rx_clk) and rx_dv = '1' and full = '0' then 
            rgmii_rx_d8 <= high_nibble & rgmii_rx_d; 
            w_en <= '1';
        else
            w_en <= '0';
        end if;
    end process;
    
    -- process to extract rx_dv and rx_error from rgmii_rx_ctl
    process(rgmii_rx_clk)
    begin
        if rising_edge(rgmii_rx_clk) then rx_dv <= rgmii_rx_ctl; end if;
    end process;
    process(rgmii_rx_clk)
    begin
        if falling_edge(rgmii_rx_clk) then phy_error <= rx_dv xor rgmii_rx_ctl; end if;
    end process;
    
    -- FSM to handle reception
    process (clk,rst)
    begin
        if rst = '1' then
            state <= IDLE;
            rx_data_out <= (others=>'0');
            rx_start <= '0';
            rx_end <= '0';
            frame_length <= (others=>'0');
            crc_ok <= '0';
            reception_error <= '0';
            rx_data_valid <= '0';
            crc_processed <= (others=>'1');
            r_en <= '0';
        elsif rising_edge(clk) then
        case state is
        when IDLE =>
            rx_data_out <= (others=>'0');
            rx_start <= '0';
            rx_end <= '0';
            frame_length <= (others=>'0');
            crc_ok <= '0';
            reception_error <= '0';
            rx_data_valid <= '0';
            crc_processed <= (others=>'1');
            r_en <= '0';
            
            if empty = '0' then 
                index <= index + 1;
                r_en <= '1';
                state <= PREAMB;
            else
                index <= 0;
            end if;
        
        when PREAMB => -- detect preamble and sfd
            if empty = '0' then 
                r_en <= '1';
                if rgmii_rx_d8_sync = x"D5" and index = PREAMB_LEN+SFD_LEN then -- receive SFD
                    index <= 0; 
                    reception_error <= '0';
                    crc_processed <= (others=>'1'); -- initialize CRC with ones
                    state <= RCV_DEST;
                elsif rgmii_rx_d8_sync = x"55" and index<PREAMB_LEN+SFD_LEN then -- receive preamble
                    index <= index + 1;
                end if;
            else
                reception_error <= '1';
                state <= IDLE;
            end if;
        
        when RCV_DEST => -- parse destination address
            if empty = '0' then  
                r_en <= '1';              
                if index < DEST_LEN-1 then
                    rx_data_out <= rgmii_rx_d8_sync; -- transmit the byte
                    rx_data_valid <= '1';
                    crc_processed <= compute_crc32(rgmii_rx_d8_sync,crc_processed); -- compute CRC
                    index <= index + 1;
                elsif index = DEST_LEN-1 then
                    rx_data_out <= rgmii_rx_d8_sync; -- transmit the byte
                    rx_data_valid <= '1';
                    crc_processed <= compute_crc32(rgmii_rx_d8_sync,crc_processed); -- compute CRC
                    index <= 0;
                    reception_error <= '0';
                    state <= RCV_SRC;
                elsif index >DEST_LEN-1 then 
                    index <= 0;
                    reception_error <= '1';
                    rx_data_valid <= '0';
                    state <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                rx_data_valid <= '0';
                state <= IDLE;
            end if;
            
        when RCV_SRC => -- parse source address
            if empty = '0' then
                r_en <= '1';
                if index < SRC_LEN-1 then 
                    rx_data_out <= rgmii_rx_d8_sync; -- transmit the byte
                    rx_data_valid <= '1';
                    crc_processed <= compute_crc32(rgmii_rx_d8_sync,crc_processed); -- compute CRC
                    index <= index + 1;
                elsif index = SRC_LEN-1 then
                    rx_data_out <= rgmii_rx_d8_sync; -- transmit the byte
                    rx_data_valid <= '1';
                    crc_processed <= compute_crc32(rgmii_rx_d8_sync,crc_processed); -- compute CRC
                    index <= 0;
                    reception_error <= '0';
                    state <= RCV_LEN;
                elsif index >SRC_LEN-1 then 
                    index <= 0;
                    reception_error <= '1';
                    rx_data_valid <= '0';
                    state <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                rx_data_valid <= '0';
                state <= IDLE;
            end if;
            
        when RCV_LEN => -- parse length
            if empty = '0' then
                r_en <= '1';
                if index < LENGTH_LEN-1 then 
                    data_len <= data_len((LENGTH_LEN-1)*8-1 downto 0) & rgmii_rx_d8_sync;
                    rx_data_out <= rgmii_rx_d8_sync; -- transmit the byte
                    rx_data_valid <= '1';
                    crc_processed <= compute_crc32(rgmii_rx_d8_sync,crc_processed); -- compute CRC
                    index <= index + 1;
                elsif index = LENGTH_LEN-1 then
                    data_len <= data_len((LENGTH_LEN-1)*8-1 downto 0) & rgmii_rx_d8_sync;
                    rx_data_out <= rgmii_rx_d8_sync; -- transmit the byte
                    rx_data_valid <= '1';
                    crc_processed <= compute_crc32(rgmii_rx_d8_sync,crc_processed); -- compute CRC
                    index <= 0;
                    reception_error <= '0';
                    state <= RCV_PAYLOAD;
                elsif index >LENGTH_LEN-1 then 
                    index <= 0;
                    reception_error <= '1';
                    rx_data_valid <= '0';
                    state <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                rx_data_valid <= '0';
                state <= IDLE;
            end if;
            
        when RCV_PAYLOAD => -- parse payload
            if empty = '0' then
                r_en <= '1';
                if index < to_integer(unsigned(data_len)) - 1 then
                    rx_data_out <= rgmii_rx_d8_sync; -- transmit the byte
                    rx_data_valid <= '1';
                    crc_processed <= compute_crc32(rgmii_rx_d8_sync,crc_processed); -- compute CRC
                    index <= index + 1;
                elsif index = to_integer(unsigned(data_len)) - 1 then
                    rx_data_out <= rgmii_rx_d8_sync; -- transmit the byte
                    rx_data_valid <= '1';
                    crc_processed <= compute_crc32(rgmii_rx_d8_sync,crc_processed); -- compute CRC
                    index <= 0;
                    reception_error <= '0';
                    state <= RCV_FCS;
                elsif index > to_integer(unsigned(data_len)) - 1 then
                    index <= 0;
                    reception_error <= '1';
                    rx_data_valid <= '0';
                    state <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                rx_data_valid <= '0';
                state <= IDLE;
            end if;
            
        when RCV_FCS => -- parse FCS
            rx_end <= '0';
            frame_length <= std_logic_vector(to_unsigned(DEST_LEN+SRC_LEN+LENGTH_LEN+to_integer(unsigned(data_len)),16));
            rx_data_valid <= '0';
            
            if empty = '0' then
                r_en <= '1';
                if index < FCS_LEN - 1 then
                    fcs <= fcs((FCS_LEN-1)*8-1 downto 0) & rgmii_rx_d8_sync;
                    index <= index + 1;
                elsif index = FCS_LEN - 1 then
                    fcs <= fcs((FCS_LEN-1)*8-1 downto 0) & rgmii_rx_d8_sync;
                    index <= 0;
                    reception_error <= '0';
                    state <= CHECK_CRC;
                elsif index > FCS_LEN - 1 then
                    index <= 0;
                    reception_error <= '1';
                    state <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                state <= IDLE;
            end if;
        
        when CHECK_CRC => -- transmit CRC checking result
            rx_data_valid <= '0';
            reception_error <= '0';
            if fcs = not(crc_processed) then crc_ok <= '1'; else crc_ok <= '0'; end if;
            state <= IDLE;
            
        end case;
        end if;
    end process;

    -- Handling rx_start and rx_end
    rx_start_comb <= '1' when (state = RCV_DEST and index = 0 and empty = '0') else '0';
    rx_end_comb <= '1' when (state = RCV_PAYLOAD and index = to_integer(unsigned(data_len)) - 1 and empty = '0') else '0';
    
    process(clk, rst)
    begin
        if rst = '1' then
            rx_start_seq <= '0';
            rx_start   <= '0';
            rx_end_seq <= '0';
            rx_end   <= '0';
        elsif rising_edge(clk) then -- allow rx_start and rx_end to stay high only for 1 clock cycle
            rx_start_seq <= rx_start_comb;
            rx_start   <= rx_start_comb and not rx_start_seq;
            rx_end_seq <= rx_end_comb;
            rx_end   <= rx_end_comb and not rx_end_seq;
        end if;
    end process;
    
    rx_error <= phy_error_sync2 or reception_error;
end Behavioral;
