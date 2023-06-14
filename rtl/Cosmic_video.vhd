--
-- A simulation of Universal Cosmic video hardware
--
-- Mike Coates
--
-- version 001 initial release
--
library ieee;
	use ieee.std_logic_1164.all;
	use ieee.std_logic_unsigned.all;
	use ieee.numeric_std.all;

entity COSMIC_VIDEO is
port (
	I_HCNT            : in    std_logic_vector(8 downto 0);
	I_VCNT            : in    std_logic_vector(8 downto 0);
	I_BITMAP  			: in    std_logic_vector(7 downto 0);
	I_COL     			: in    std_logic_vector(2 downto 0); -- colour page
	I_H_FLIP				: in    std_logic;	
	I_S_FLIP				: in    std_logic;
	I_BW   				: in    std_logic;
	I_BLUE            : in    std_logic;
	O_VADDR   			: out   std_logic_vector(12 downto 0);
	--
	dn_addr           : in    std_logic_vector(15 downto 0);
	dn_data           : in    std_logic_vector(7 downto 0);
	dn_wr             : in    std_logic;
	dn_ld	     			: in    std_logic;
	--
	O_RED             : out   std_logic_vector(3 downto 0);
	O_GREEN           : out   std_logic_vector(3 downto 0);
	O_BLUE            : out   std_logic_vector(3 downto 0);
	PIX_CLK           : in    std_logic;
	CLK		         : in    std_logic;
	CPU_ENA	         : in    std_logic;
	GAME              : in    std_logic_vector(7 downto 0);
   PAUSED            : in    std_logic
);
end;

architecture RTL of COSMIC_VIDEO is

signal col_cs			: std_logic;
signal col_pix			: std_logic_vector(7 downto 0);
signal char_pix_n		: std_logic_vector(7 downto 0);
signal char_pix_l		: std_logic_vector(7 downto 0);
signal col_addr		: std_logic_vector(10 downto 0);
signal col_ad			: std_logic_vector(10 downto 0);
signal op_pix			: std_logic_vector(7 downto 0);
signal op_pix2			: std_logic_vector(7 downto 0);
signal op_ad2			: std_logic_vector(10 downto 0);
signal charcolour    : std_logic_vector(3 downto 0);
signal I_Flip        : std_logic := '0';

begin
	-- Load rom signals
	col_cs      <= '1' when dn_addr(15 downto 13) = "001" else '0';  		-- 2000-23FF

	-- Address multiplex
	col_ad <= dn_addr(10 downto 0) when dn_ld='1' else col_addr;
	
	col_rom : entity work.spram
	generic map (
	  addr_width => 11
	)
	port map (
	  q        => col_pix,
	  data     => dn_data(7 downto 0),
	  address  => col_ad,
	  wren     => dn_wr and col_cs,
	  clock    => clk
   );

-- if both software and hardware flip the same, then don't flip background
I_FLIP <= '0' when (I_S_FLIP = I_H_FLIP) else '1';	

-- Video is bitmap using paged colour rom for colour map
vid_address : process
variable HADD : std_logic_vector(8 downto 0);
begin
	wait until rising_edge(CLK);

	 if (PIX_CLK = '1') then	

		if ((I_HCNT(8)='1' and I_VCNT(8)='1' and I_HCNT(2 downto 0)="101") or (I_HCNT="011111101")) then
			-- set address for video ram and colour ram
			HADD := I_HCNT + 3;   -- we want data for next character
			
			-- need to allow for screen flip (hardware and software)
			if I_FLIP='0' then
				O_VADDR  <= I_VCNT(7 downto 0) & HADD(7 downto 3);
				col_addr <= '0' & I_COL(1) & I_COL(0) & I_VCNT(7 downto 4) & HADD(7 downto 4);
			else
				O_VADDR  <= not I_VCNT(7 downto 0) & not HADD(7 downto 3);
				col_addr <= '0' & I_COL(1) & I_COL(0) & not I_VCNT(7 downto 4) & not HADD(7 downto 4);
		   end if;
		end if;

		if ((I_HCNT(8)='1' and I_VCNT(8)='1' and I_HCNT(2 downto 0)="111") or (I_HCNT="011111111")) then

			charcolour <= col_pix(3 downto 0);
			
			if I_FLIP='1' then
				char_pix_n <= I_BITMAP;
			else
				char_pix_n <= I_BITMAP(0) & I_BITMAP(1) & I_BITMAP(2) & I_BITMAP(3) & I_BITMAP(4) & I_BITMAP(5) & I_BITMAP(6) & I_BITMAP(7);
			end if;
		end if;
	end if;
end process;

backround_draw : process
variable pixel  : std_logic;
begin
    wait until rising_edge(CLK);
	 
    if (PIX_CLK = '1') then	

		--  if in visible area 
		if I_HCNT(8)='1' and I_VCNT(8)='1' and I_VCNT(7 downto 5) /= "111" then -- skip rows one side and > 224 : and I_VCNT(7 downto 3) /= "00000"
			 case I_HCNT(2 downto 0) is
				when "000" => pixel := char_pix_n(0);
								  char_pix_l <= char_pix_n;
				when "001" => pixel := char_pix_l(1);
				when "010" => pixel := char_pix_l(2);
				when "011" => pixel := char_pix_l(3);
				when "100" => pixel := char_pix_l(4);
				when "101" => pixel := char_pix_l(5);
				when "110" => pixel := char_pix_l(6);
				when "111" => pixel := char_pix_l(7);
			 end case;

			if pixel='1' then		
				if I_BW='1' then
					-- Black and White only
					O_BLUE  <= "1111";
					O_GREEN <= "1111";
					O_RED   <= "1111";
				else
					-- Colour Rom
					O_BLUE  <= charcolour(2) & '0' & charcolour(2) & '0';
					O_GREEN <= charcolour(1) & '0' & charcolour(1) & '0';
					O_RED   <= charcolour(0) & charcolour(3) & charcolour(0) & charcolour(3);
				end if;
			else
				if I_BW='1' then
					-- Black and White only
					O_BLUE  <= "0000";
					O_GREEN <= "0000";
					O_RED   <= "0000";
				else
					-- Optional blue background
					O_BLUE  <= "00" & I_BLUE & I_BLUE;
					O_GREEN <= "0000";
					O_RED   <= "0000";
				end if;
			 end if;			 
		else
			O_BLUE  <= "0000";
			O_GREEN <= "0000";
			O_RED   <= "0000";
		end if;
	end if;
end process;

end architecture;
