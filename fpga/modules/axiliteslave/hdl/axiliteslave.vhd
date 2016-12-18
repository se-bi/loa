-------------------------------------------------------------------------------
-- Title      : AXI Slave, synchronous
-------------------------------------------------------------------------------
-- Author     : git@se-bi.de
-------------------------------------------------------------------------------
-- Description: This is an SPI slave that is a busmaster to the local bus.
--              Data can be transfered to and from the bus slaves on the bus.
--
--              The logic of this module is adopted from the Xilinx IP Package
--              Creator creating a new AXI register peripheral. Some signals
--              and ports are renamed according to the package definitions.
--              The processes writting and reading from register are modified
--              to translate the read or write access from the AXI Lite to the
--              local bus.
--
--              The clock for this module is part of the AXI interface.
--
-- Protocol:    The AXI Lite transfers either 32 or 64 bits. This is the data
--              and als address width.
--
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.axiliteslave_pkg.all;
use work.bus_pkg.all;

entity axiliteslave is
  
  generic (
    S_AXI_ADDR_WIDTH : integer := 32;
    S_AXI_DATA_WIDTH : integer := 32
  );
  port (
    s_axi_lite_o  : out s_axi_lite_out;
    s_axi_lite_i  : in  s_axi_lite_in;
    --
    bus_o         : out busmaster_out_type;
    bus_i         : in  busmaster_in_type
  );
  
end entity axiliteslave;

architecture rtl of axiliteslave is

  constant LOA_BUS_ADDR_WL : integer := 15;
  constant LOA_BUS_DATA_WL : integer := 16;
  
  type loa_bus_type is record
    bus_r_addr  : std_logic_vector(LOA_BUS_ADDR_WL -1 downto 0);
    bus_w_addr  : std_logic_vector(LOA_BUS_ADDR_WL -1 downto 0);
    bus_do      : std_logic_vector(LOA_BUS_DATA_WL -1 downto 0);
    bus_re      : std_logic;
    bus_we      : std_logic;
  end record;
  
  signal loa_bus : loa_bus_type;
  
  -- AXI4LITE signals
  signal a_clk    : std_logic;
  signal a_rst_n  : std_logic;
  --
  signal aw_addr  : std_logic_vector(S_AXI_ADDR_WIDTH -1 downto 0);
  signal aw_ready : std_logic;
  signal w_ready  : std_logic;
  signal b_resp   : std_logic_vector(1 downto 0);
  signal b_valid  : std_logic;
  signal ar_addr  : std_logic_vector(S_AXI_ADDR_WIDTH -1 downto 0);
  signal ar_ready : std_logic;
  signal r_data   : std_logic_vector(S_AXI_DATA_WIDTH -1 downto 0);
  signal r_resp   : std_logic_vector(1 downto 0);
  signal r_valid  : std_logic;
  
  signal slv_reg_rden : std_logic;
  signal slv_reg_wren : std_logic;
  signal byte_index : integer;
  
begin
  
  -- I/O Connections assignments
  a_clk   <= s_axi_lite_i.global.a_clk;
  a_rst_n <= s_axi_lite_i.global.a_reset_n;
  --
  s_axi_lite_o.w_addr_ch.aw_ready <= aw_ready;
  s_axi_lite_o.w_data_ch.w_ready  <= w_ready;
  s_axi_lite_o.w_resp_ch.b_resp   <= b_resp;
  s_axi_lite_o.w_resp_ch.b_valid  <= b_valid;
  s_axi_lite_o.r_addr_ch.ar_ready <= ar_ready;
  s_axi_lite_o.r_data_ch.r_data   <= r_data;
  s_axi_lite_o.r_data_ch.r_resp   <= r_resp;
  s_axi_lite_o.r_data_ch.r_valid  <= r_valid;
  
  -- Implement aw_ready generation
  -- aw_ready is asserted for one a_clk clock cycle when both
  -- s_axi_lite_i.w_addr_ch.aw_valid and s_axi_lite_i.w_data_ch.w_valid are asserted. aw_ready is
  -- de-asserted when reset is low.
  process ( a_clk )
  begin
    if rising_edge( a_clk ) then
      if ( a_rst_n = '0' ) then
        aw_ready    <= '0';
      else
        if ( aw_ready = '0' and s_axi_lite_i.w_addr_ch.aw_valid = '1' and s_axi_lite_i.w_data_ch.w_valid = '1' ) then
          -- slave is ready to accept write address when
          -- there is a valid write address and write data
          -- on the write address and data bus. This design
          -- expects no outstanding transactions.
          aw_ready  <= '1';
        else
          aw_ready  <= '0';
        end if;
      end if;
    end if;
  end process;
  
  -- Implement aw_addr latching
  -- This process is used to latch the address when both
  -- s_axi_lite_i.w_addr_ch.aw_valid and s_axi_lite_i.w_data_ch.w_valid are valid.
  process ( a_clk )
  begin
    if rising_edge( a_clk ) then
      if ( a_rst_n = '0' ) then
        aw_addr   <= (others => '0');
      else
        if ( aw_ready = '0' and s_axi_lite_i.w_addr_ch.aw_valid = '1' and s_axi_lite_i.w_data_ch.w_valid = '1' ) then
          -- Write Address latching
          aw_addr <= s_axi_lite_i.w_addr_ch.aw_addr;
        end if;
      end if;
    end if;
  end process;
  
  -- Implement w_ready generation
  -- w_ready is asserted for one a_clk clock cycle when both
  -- s_axi_lite_i.w_addr_ch.aw_valid and s_axi_lite_i.w_data_ch.w_valid are asserted. w_ready is 
  -- de-asserted when reset is low.
  process ( a_clk )
  begin
    if rising_edge( a_clk ) then
      if ( a_rst_n = '0' ) then
        w_ready     <= '0';
      else
        if ( w_ready = '0' and s_axi_lite_i.w_addr_ch.aw_valid = '1' and s_axi_lite_i.w_data_ch.w_valid = '1' ) then
            -- slave is ready to accept write data when
            -- there is a valid write address and write data
            -- on the write address and data bus. This design
            -- expects no outstanding transactions.
            w_ready <= '1';
        else
          w_ready   <= '0';
        end if;
      end if;
    end if;
  end process;
  
  -- Implement memory mapped register select and write logic generation
  -- The write data is accepted and written to memory mapped registers when
  -- aw_ready, s_axi_lite_i.w_data_ch.w_valid, w_ready and s_axi_lite_i.w_data_ch.w_valid are asserted. Write strobes are used to
  -- select byte enables of slave registers while writing.
  -- These registers are cleared when reset (active low) is applied.
  -- Slave register write enable is asserted when valid address and data are available
  -- and the slave is ready to accept the write address and write data.
  loa_bus.bus_we <= w_ready and s_axi_lite_i.w_data_ch.w_valid and aw_ready and s_axi_lite_i.w_addr_ch.aw_valid;

  process ( loa_bus.bus_we, aw_addr )
    variable addr :std_logic_vector(LOA_BUS_ADDR_WL -1 downto 0);
  begin
    addr := ( others => '0' );
    --
    if ( loa_bus.bus_we = '1' ) then
      addr := aw_addr( ADDR_LSB + LOA_BUS_ADDR_WL downto ADDR_LSB );
    end if;
    --
    loa_bus.bus_w_addr <= addr;
  end process;
  
  -- Implement write response logic generation
  -- The write response and response valid signals are asserted by the slave
  -- when w_ready, s_axi_lite_i.w_data_ch.w_valid, w_ready and s_axi_lite_i.w_data_ch.w_valid are asserted.
  -- This marks the acceptance of address and indicates the status of
  -- write transaction.
  process ( a_clk )
  begin
    if rising_edge( a_clk ) then
      if ( a_rst_n = '0' ) then
        b_valid   <= '0';
        b_resp    <= "00"; --need to work more on the responses
      else
        if ( aw_ready = '1' and s_axi_lite_i.w_addr_ch.aw_valid = '1' and w_ready = '1' and s_axi_lite_i.w_data_ch.w_valid = '1' and b_valid = '0' ) then
          b_valid <= '1';
          b_resp  <= "00";
        elsif ( s_axi_lite_i.w_resp_ch.b_ready = '1' and b_valid = '1' ) then -- check if b_ready is asserted while b_valid is high)
          b_valid <= '0';                                                     -- (there is a possibility that b_ready is always asserted high)
        end if;
      end if;
    end if;
  end process;
  
  -- Implement ar_ready generation
  -- ar_ready is asserted for one a_clk clock cycle when
  -- s_axi_lite_i.r_addr_ch.ar_valid is asserted. aw_ready is
  -- de-asserted when reset (active low) is asserted.
  -- The read address is also latched when s_axi_lite_i.r_addr_ch.ar_valid is
  -- asserted. ar_addr is reset to zero on reset assertion.
  process ( a_clk )
  begin
    if rising_edge( a_clk ) then
      if ( a_rst_n = '0' ) then
        ar_ready      <= '0';
        ar_addr       <= (others => '1');
      else
        if ( ar_ready = '0' and s_axi_lite_i.r_addr_ch.ar_valid = '1' ) then
          -- indicates that the slave has acceped the valid read address
          ar_ready    <= '1';
          -- Read Address latching
          ar_addr     <= s_axi_lite_i.r_addr_ch.ar_addr;
        else
          ar_ready <= '0';
        end if;
      end if;
    end if;
  end process;
  
  -- Implement ar_valid generation
  -- r_valid is asserted for one a_clk clock cycle when both
  -- s_axi_lite_i.r_addr_ch.ar_valid and ar_ready are asserted. The slave registers
  -- data are available on the r_data bus at this instance. The
  -- assertion of r_valid marks the validity of read data on the
  -- bus and r_resp indicates the status of read transaction.r_valid
  -- is deasserted on reset (active low). r_resp and r_data are
  -- cleared to zero on reset (active low).
  process ( a_clk )
  begin
    if rising_edge( a_clk ) then
      if ( a_rst_n = '0' ) then
        r_valid   <= '0';
        r_resp    <= "00";
      else
        if ( ar_ready = '1' and s_axi_lite_i.r_addr_ch.ar_valid = '1' and r_valid = '0' ) then
          -- Valid read data is available at the read data bus
          r_valid <= '1';
          r_resp  <= "00"; -- 'OKAY' response
        elsif ( r_valid = '1' and s_axi_lite_i.r_data_ch.r_ready = '1' ) then
          -- Read data is accepted by the master
          r_valid <= '0';
        end if;
      end if;
    end if;
  end process;
  
  -- Implement memory mapped register select and read logic generation
  -- Slave register read enable is asserted when valid address is available
  -- and the slave is ready to accept the read address.
  loa_bus.bus_re <= ar_ready and s_axi_lite_i.r_addr_ch.ar_valid and ( not r_valid ) ;

  -- Output register or memory read data
  process( loa_bus.bus_re, bus_i.data ) is
    variable data : std_logic_vector(S_AXI_DATA_WIDTH -1 downto 0);
  begin
    data    := ( others => '0' );
    --
    if ( loa_bus.bus_re = '1' ) then
      -- When there is a valid read address (s_axi_lite_i.r_addr_ch.ar_valid) with
      -- acceptance of read address by the slave (ar_ready),
      -- output the read dada
      -- Read address mux
      data  := bus_i.data;     -- loa bus read data
    end if;
    --
    r_data  <= data;
  end process;
  
  process( loa_bus.all ) is
    variable addr : std_logic_vector(LOA_BUS_ADDR_WL -1 downto 0);
  begin
    addr := ( others => '0' );
    --
    if ( loa_bus.bus_we = '1' ) then
      addr := loa_bus.bus_w_addr;
    elsif ( loa_bus.bus_re = '1' ) then
      addr := loa_bus.bus_r_addr;
    end if;
    --
    bus_o.addr <= addr;
  end process ;
  
  bus_o.data  <= loa_bus.bus_do;
  bus_o.re    <= loa_bus.bus_re;
  bus_o.we    <= loa_bus.bus_we;
  
end architecture rtl;
