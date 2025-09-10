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
    --  size for each field of the frame (in byte)
    constant PREAMB_LEN : integer := 7;
    constant SFD_LEN : integer := 1;
    constant DEST_LEN : integer := 6;
    constant SRC_LEN : integer := 6;
    constant LENGTH_LEN : integer := 2;
    constant PAYLOAD_MAX_LEN : integer := 1500;
    constant FCS_LEN : integer := 4;
    constant CRC_POLY_INV : std_logic_vector(31 downto 0) := x"EDB88320";
    
    -- TYPES
    type FSM_type is (IDLE,PREAMB, RCV_DEST, RCV_SRC, RCV_LEN, RCV_PAYLOAD, RCV_FCS, GET_BYTE, COMPUTE_CRC, CHECK_CRC);
    
    -- SIGNALS
    -- for the conversion from nibble to byte
    signal high_nibble : std_logic_vector(3 downto 0);
    signal rgmii_rx_d8 : std_logic_vector(7 downto 0);
    signal byte_valid : std_logic;
    -- for the reception
    signal actual_st, next_st : FSM_type;
    signal rx_dv : std_logic;  -- ongoing frame transfert
    signal phy_error : std_logic; -- error get from phy in rgmii_rx_ctl
    signal reception_error : std_logic; -- error during reception
    signal index : integer range 0 to NB_BYTE_MAX;
    signal dest_addr : std_logic_vector(DEST_LEN*8-1 downto 0);
    signal src_addr : std_logic_vector(SRC_LEN*8-1 downto 0);
    signal data_len : std_logic_vector(LENGTH_LEN*8-1 downto 0);
    signal payload : std_logic_vector(PAYLOAD_MAX_LEN*8-1 downto 0);
    signal fcs : std_logic_vector(FCS_LEN*8-1 downto 0);
    signal frame : std_logic_vector((DEST_LEN+SRC_LEN+LENGTH_LEN+PAYLOAD_MAX_LEN)*8-1 downto 0);
    signal byte : std_logic_vector(7 downto 0);
    signal bit_count : integer range 0 to 8 := 0;
    signal crc_processed : std_logic_vector(31 downto 0);
    signal frame_length_s : std_logic_vector(15 downto 0);
begin

    -- Nibble to byte
    process(rgmii_rx_clk)
    begin
        if rising_edge(rgmii_rx_clk) then 
            high_nibble <= rgmii_rx_d; 
        end if;
    end process;
    process(rgmii_rx_clk)
    begin
        if falling_edge(rgmii_rx_clk) then 
            rgmii_rx_d8 <= high_nibble & rgmii_rx_d; 
            byte_valid <= '1';
        else
            byte_valid <= '0';
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
            actual_st <= IDLE;
        elsif rising_edge(clk) then
            actual_st <= next_st;
        end if;
    end process;
    
    process (actual_st)
    begin
        case actual_st is
        when IDLE =>
            rx_data_out <= (others=>'0');
            rx_start <= '0';
            rx_end <= '0';
            frame_length_s <= (others=>'0');
            crc_ok <= '0';
            reception_error <= '0';
            rx_data_valid <= '0';
            
            if byte_valid = '1' and rx_dv = '1' then 
                index <= index + 1;
                next_st <= PREAMB;
            else
                index <= 0;
            end if;
        
        when PREAMB => -- detect preamble and sfd
            if byte_valid = '1' and rx_dv = '1' then 
                if rgmii_rx_d8 = x"D5" and index = 8 then -- receive SFD
                    index <= 0; 
                    next_st <= RCV_DEST;
                elsif rgmii_rx_d8 = x"55" and index<8 then -- receive preamble
                    index <= index + 1;
                end if;
            else
                reception_error <= '1';
                next_st <= IDLE;
            end if;
        
        when RCV_DEST => -- parse destination address
            if rx_dv = '1' and byte_valid = '1' then
                if index = 0 then 
                    rx_start <= '1'; -- start signal for system
                else 
                    rx_start <= '0'; 
                end if;
                
                if index < DEST_LEN-1 then 
                    dest_addr <= dest_addr((DEST_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    rx_data_out <= rgmii_rx_d8; -- transmit the byte
                    rx_data_valid <= '1';
                    index <= index + 1;
                elsif index = DEST_LEN-1 then
                    dest_addr <= dest_addr((DEST_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    rx_data_out <= rgmii_rx_d8; -- transmit the byte
                    rx_data_valid <= '1';
                    index <= 0;
                    next_st <= RCV_SRC;
                elsif index >DEST_LEN-1 then 
                    index <= 0;
                    reception_error <= '1';
                    rx_data_valid <= '0';
                    next_st <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                rx_data_valid <= '0';
                next_st <= IDLE;
            end if;
            
        when RCV_SRC => -- parse source address
            if rx_dv = '1' and byte_valid = '1' then
                if index < SRC_LEN-1 then 
                    src_addr <= src_addr((SRC_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    rx_data_out <= rgmii_rx_d8; -- transmit the byte
                    rx_data_valid <= '1';
                    index <= index + 1;
                elsif index = SRC_LEN-1 then
                    src_addr <= src_addr((SRC_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    rx_data_out <= rgmii_rx_d8; -- transmit the byte
                    rx_data_valid <= '1';
                    index <= 0;
                    next_st <= RCV_LEN;
                elsif index >SRC_LEN-1 then 
                    index <= 0;
                    reception_error <= '1';
                    rx_data_valid <= '0';
                    next_st <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                rx_data_valid <= '0';
                next_st <= IDLE;
            end if;
            
        when RCV_LEN => -- parse length
            if rx_dv = '1' and byte_valid = '1' then
                if index < LENGTH_LEN-1 then 
                    data_len <= data_len((LENGTH_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    rx_data_out <= rgmii_rx_d8; -- transmit the byte
                    rx_data_valid <= '1';
                    index <= index + 1;
                elsif index = LENGTH_LEN-1 then
                    data_len <= data_len((LENGTH_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    rx_data_out <= rgmii_rx_d8; -- transmit the byte
                    rx_data_valid <= '1';
                    index <= 0;
                    next_st <= RCV_PAYLOAD;
                elsif index >LENGTH_LEN-1 then 
                    index <= 0;
                    reception_error <= '1';
                    rx_data_valid <= '0';
                    next_st <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                rx_data_valid <= '0';
                next_st <= IDLE;
            end if;
            
        when RCV_PAYLOAD => -- parse payload
            if rx_dv = '1' and byte_valid = '1' then
                if index < to_integer(unsigned(data_len)) - 1 then
                    payload <= payload((PAYLOAD_MAX_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    rx_data_out <= rgmii_rx_d8; -- transmit the byte
                    rx_data_valid <= '1';
                    index <= index + 1;
                elsif index = to_integer(unsigned(data_len)) - 1 then
                    payload <= payload((PAYLOAD_MAX_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    rx_data_out <= rgmii_rx_d8; -- transmit the byte
                    rx_data_valid <= '1';
                    rx_end <= '1';
                    index <= 0;
                    next_st <= RCV_FCS;
                elsif index > to_integer(unsigned(data_len)) - 1 then
                    index <= 0;
                    reception_error <= '1';
                    rx_data_valid <= '0';
                    next_st <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                rx_data_valid <= '0';
                next_st <= IDLE;
            end if;
            
        when RCV_FCS => -- parse FCS
            rx_end <= '0';
            frame <= dest_addr&src_addr&data_len&payload((to_integer(unsigned(data_len)))*8-1 downto 0);
            frame_length_s <= std_logic_vector(to_unsigned(DEST_LEN+SRC_LEN+LENGTH_LEN+to_integer(unsigned(data_len)),16));
            rx_data_valid <= '0';
            
            if rx_dv = '1' and byte_valid = '1' then
                if index < FCS_LEN - 1 then
                    fcs <= fcs((FCS_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    index <= index + 1;
                elsif index = to_integer(unsigned(data_len)) - 1 then
                    fcs <= fcs((FCS_LEN-1)*8-1 downto 0) & rgmii_rx_d8;
                    index <= 0;
                    crc_processed <= (others=>'1'); -- initialize computed CRC
                    next_st <= GET_BYTE;
                elsif index > to_integer(unsigned(data_len)) - 1 then
                    index <= 0;
                    reception_error <= '1';
                    next_st <= IDLE;
                end if;
            else
                index <= 0;
                reception_error <= '1';
                next_st <= IDLE;
            end if;
        
        when GET_BYTE =>
            rx_data_valid <= '0';
            if index < to_integer(unsigned(frame_length_s)) then
                byte <= frame((index+1)*8-1 downto index*8);
                index <= index + 1;
                bit_count <= 0;
                next_st <= COMPUTE_CRC;
            else
                index <= 0;
                reception_error <= '1';
                next_st <= IDLE;
            end if;
        
        when COMPUTE_CRC =>
            rx_data_valid <= '0';
            if bit_count < 8 then
                if (crc_processed(0) xor byte(bit_count)) = '1' then
                  crc_processed <= (crc_processed(31 downto 1)&'0') xor CRC_POLY_INV;
                else
                  crc_processed <= (crc_processed(31 downto 1)&'0');
                end if;
                bit_count <= bit_count + 1;
            elsif bit_count = 8 then
                if index = to_integer(unsigned(frame_length_s)) then
                    next_st <= CHECK_CRC;
                elsif index < to_integer(unsigned(frame_length_s)) then
                    next_st <= GET_BYTE;
                else
                    reception_error <= '1';
                    next_st <= IDLE;
                end if;
            end if;
        
        when CHECK_CRC => -- transmit CRC checking result
            rx_data_valid <= '0';
            if fcs = crc_processed then crc_ok <= '1'; else crc_ok <= '0'; end if;
            next_st <= IDLE;
            
        end case;
    end process;

    frame_length <= frame_length_s;
    rx_error <= phy_error or reception_error;
end Behavioral;
