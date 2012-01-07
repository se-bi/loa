
library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

package utils_pkg is

   -- Calculates the number of bits required to encode the given number
   --
   -- Note that this function is not intended to synthesize directly into
   -- hardware, rather it is used to generate constants for synthesized
   -- hardware.
   --
   -- Example:
   -- entity foo is
   --    generic(
   --       ABC : positive);
   --    port(
   --       xzy : out std_logic_vector(required_bits(ABC) downto 0));
   -- end foo;
   function required_bits (value : natural) return natural;

   ----------------------------------------------------------------------------
   component clock_divider is
      generic (
         DIV : positive);
      port (
         clk_out_p : out std_logic;
         clk       : in  std_logic);
   end component;

   -- Requires MUL <= DIV
   component fractional_clock_divider is
      generic (
         DIV : positive;
         MUL : positive);
      port (
         clk_out_p : out std_logic;
         clk       : in  std_logic);
   end component fractional_clock_divider;
   
end package utils_pkg;

package body utils_pkg is

   function required_bits (value : natural) return natural is
   begin
      if value <= 0 then
         return 0;
      elsif value = 1 then
         return 1;
      elsif value < 8 then
         return integer(ceil(log2(real(value))));
      else
         -- FIXME: Why is this hack necessary?
         -- Otherwise the values for 2**x (x >= 3) is calculated wrong.
         -- E.g.:
         -- required_bits(8) = 3 != 4
         -- required_bits(16) = 4 != 5
         -- see ../tb/utils_tb.vhd
         return integer(ceil(log2(real(value) + 0.5)));
      end if;
   end function;

end package body utils_pkg;