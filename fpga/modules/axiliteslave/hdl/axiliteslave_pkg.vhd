-------------------------------------------------------------------------------
-- Title      : AXI Lite Slave Package Definition
-- Project    :
-------------------------------------------------------------------------------
-- File       : axiliteslave_pkg.vhd
-- Author     : git@se-bi.de
-- Company    :
-- Created    : 2016-11-14
-- Platform   :
-------------------------------------------------------------------------------
-- Description: This packages contains mainly record type definitions based on
--              the AXI Lite specicication. The signals are grouped according
--              the data, address or response channels and directions.
--              The comments are adopted from the Xilinx IP Package Creator.
-------------------------------------------------------------------------------
-- Copyright (c) 2016

library ieee;
use ieee.std_logic_1164.all;

package axiliteslave_pkg is

  type s_axi_lite_global_in is record
    -- Global Clock Signal
    a_clk     : std_logic;
    -- Global Reset Signal. This Signal is Active LOW
    a_reset_n : std_logic;
  end record;

  type s_axi_lite_w_addr_ch_in is record
    -- Write address (issued by master, acceped by Slave)
    aw_addr   : std_logic_vector(S_AXI_ADDR_WIDTH -1 downto 0);
    -- Write channel Protection type. This signal indicates the
    -- privilege and security level of the transaction, and whether
    -- the transaction is a data access or an instruction access.
    aw_prot   : std_logic_vector(2 downto 0);
    -- Write address valid. This signal indicates that the master signaling
    -- valid write address and control information.
    aw_valid  : std_logic;
  end record;

  type s_axi_lite_w_addr_ch_out is record
    -- Write address ready. This signal indicates that the slave is ready
    -- to accept an address and associated control signals.
    aw_ready  : std_logic;
  end record;

  type s_axi_lite_w_data_ch_in is record
    -- Write data (issued by master, acceped by Slave)
    w_data    : std_logic_vector(S_AXI_DATA_WIDTH -1 downto 0);
    -- Write strobes. This signal indicates which byte lanes hold
    -- valid data. There is one write strobe bit for each eight
    -- bits of the write data bus.
    w_strb    : std_logic_vector((S_AXI_DATA_WIDTH/8) -1 downto 0);
    -- Write valid. This signal indicates that valid write
    -- data and strobes are available.
    w_valid   : std_logic;
  end record;

  type s_axi_lite_w_data_ch_out is record
    -- Write ready. This signal indicates that the slave
    -- can accept the write data.
    w_ready   : std_logic;
  end record;

  type s_axi_lite_w_resp_ch_in is record
    -- Response ready. This signal indicates that the master
    -- can accept a write response.
    b_ready   : std_logic;
  end record;

  type s_axi_lite_w_resp_ch_out is record
    -- Write response. This signal indicates the status
    -- of the write transaction.
    b_resp    : std_logic_vector(1 downto 0);
    -- Write response valid. This signal indicates that the channel
    -- is signaling a valid write response.
    b_valid   : std_logic;
  end record;

  type s_axi_lite_r_addr_ch_in is record
    -- Read address (issued by master, acceped by Slave)
    ar_addr   : std_logic_vector(S_AXI_ADDR_WIDTH -1 downto 0);
    -- Protection type. This signal indicates the privilege
    -- and security level of the transaction, and whether the
    -- transaction is a data access or an instruction access.
    ar_prot   : std_logic_vector(2 downto 0);
    -- Read address valid. This signal indicates that the channel
    -- is signaling valid read address and control information.
    ar_valid  : std_logic;
  end record;

  type s_axi_lite_r_addr_ch_out is record
    -- Read address ready. This signal indicates that the slave is
    -- ready to accept an address and associated control signals.
    ar_ready  : std_logic;
  end record;

  type s_axi_lite_r_data_ch_in is record
    -- Read ready. This signal indicates that the master can
    -- accept the read data and response information.
    r_ready   : std_logic;
  end record;

  type s_axi_lite_r_data_ch_out is record
    -- Read data (issued by slave)
    r_data    : std_logic_vector(S_AXI_DATA_WIDTH -1 downto 0);
    -- Read response. This signal indicates the status of the
    -- read transfer.
    r_resp    : std_logic_vector(1 downto 0);
    -- Read valid. This signal indicates that the channel is
    -- signaling the required read data.
    r_valid   : std_logic;
  end record;

  type s_axi_lite_in is record
    global    : s_axi_lite_global_in;
    w_addr_ch : s_axi_lite_w_addr_ch_in;
    w_data_ch : s_axi_lite_w_data_ch_in;
    w_resp_ch : s_axi_lite_w_resp_ch_in;
    r_addr_ch : s_axi_lite_r_addr_ch_in;
    r_data_ch : s_axi_lite_r_data_ch_in;
  end record;

  type s_axi_lite_out is record
    w_addr_ch : s_axi_lite_w_addr_ch_out;
    w_data_ch : s_axi_lite_w_data_ch_out;
    w_resp_ch : s_axi_lite_w_resp_ch_out;
    r_addr_ch : s_axi_lite_r_addr_ch_out;
    r_data_ch : s_axi_lite_r_data_ch_out;
  end record;

   -----------------------------------------------------------------------------
   -- Component declarations
   -----------------------------------------------------------------------------
  component axiliteslave is
    generic (
      S_AXI_ADDR_WIDTH : integer := 32;
      S_AXI_DATA_WIDTH : integer := 32);
    port (
      s_axi_lite_o  : out s_axi_lite_out;
      s_axi_lite_i  : in  s_axi_lite_in;
      --
      bus_o         : out busmaster_out_type;
      bus_i         : in  busmaster_in_type);
		end component axiliteslave;

end package axiliteslave_pkg;
