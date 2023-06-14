--
-- A simulation of Universal Cosmic Guerilla
--
-- Mike Coates
--
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

entity cosmic is
port
(
	O_VIDEO_R  : out std_logic_vector(3 downto 0);
	O_VIDEO_G  : out std_logic_vector(3 downto 0);
	O_VIDEO_B  : out std_logic_vector(3 downto 0);
	O_HSYNC    : out std_logic;
	O_VSYNC    : out std_logic;
	O_HBLANK   : out std_logic;
	O_VBLANK   : out std_logic;
	O_VCOUNT   : out std_logic_vector(8 downto 0);
	O_HCOUNT   : out std_logic_vector(8 downto 0);
	I_H_OFFSET : in  std_logic_vector(3 downto 0);
	I_V_OFFSET : in  std_logic_vector(3 downto 0);
	I_FLIP     : in  std_logic;
	I_MONO     : in  std_logic;
	I_BLUE	  : in  std_logic;
	O_FLIP_C   : out std_logic;
	--
	O_SoundPort : out std_logic_vector(15 downto 0);
	O_SoundStop : out std_logic_vector(15 downto 0);
	O_Sound_EN : out std_logic;
	O_AUDIO    : out std_logic_vector(15 downto 0);
	--
	dipsw1     : in  std_logic_vector(7 downto 0);
	in0        : in  std_logic_vector(7 downto 0);
	in1        : in  std_logic_vector(7 downto 0);
	coin       : in  std_logic;
	cabinet    : in  std_logic;
	--
	dn_addr    : in  std_logic_vector(15 downto 0);
	dn_data    : in  std_logic_vector(7 downto 0);
	dn_wr      : in  std_logic;
	dn_ld	     : in  std_logic;
	--
	RESET      : in  std_logic;
	PIX_CLK    : in  std_logic;
	CPU_ENA    : in  std_logic;
	CLK        : in  std_logic;
	GAME       : in  std_logic_vector(7 downto 0);
	PAUSED     : in  std_logic;
	-- Debug 
	Line1					: inout std_logic_vector(149 downto 0);
	Line2					: inout std_logic_vector(149 downto 0)	
);
end;

architecture RTL of cosmic is
	-- timing
	signal hcnt             : std_logic_vector(8 downto 0) := "000000000";
	signal vcnt             : std_logic_vector(8 downto 0) := "000000000";
	signal hsync            : std_logic;
	signal vsync            : std_logic;
	signal hblank           : std_logic;
	signal vblank           : std_logic := '1';
   signal do_hsync         : boolean;
   signal set_vblank       : boolean;
	
	signal hsync_start		: std_logic_vector(8 downto 0);
	signal hsync_end			: std_logic_vector(8 downto 0);
	signal vsync_start		: std_logic_vector(8 downto 0);
	signal vsync_end			: std_logic_vector(8 downto 0);
	
	-- cpu
	signal cpu_m1_l         : std_logic;
	signal cpu_mreq_l       : std_logic;
	signal cpu_rd_l         : std_logic;
	signal cpu_wr_l         : std_logic;
	signal cpu_rfsh_l       : std_logic;
	signal cpu_int          : std_logic := '1';
	signal cpu_nmi_l        : std_logic := '1';
	signal cpu_addr         : std_logic_vector(15 downto 0);
	signal cpu_data_out     : std_logic_vector(15 downto 0);
	signal cpu_data_in      : std_logic_vector(15 downto 0) := "0000000000000000";
	
	signal cpu_ready        : std_logic := '1';
	signal cpu_iaq          : std_logic;
	signal cpu_as           : std_logic;
	signal cpu_int_req      : std_logic;
	signal cpu_int_ack      : std_logic;
	signal cpu_cruin        : std_logic;
	signal cpu_cruout       : std_logic;
	signal cpu_cruclk       : std_logic;
	signal cpu_hold         : std_logic;
	signal cpu_holda        : std_logic;
	signal cpu_stuck        : std_logic;
	signal waits            : std_logic_vector(7 downto 0) := "00000000";
	
	-- Memory mapping
	signal dn_low           : std_logic_vector(7 downto 0);
	signal rom_in           : std_logic_vector(15 downto 0);
	signal rom_ld           : std_logic := '0';
	signal rom_rd           : std_logic := '0';
	signal ram_rd           : std_logic := '0';
	signal vid_rd           : std_logic := '0';
	signal col_rd           : std_logic := '0';	
	signal vector_rd			: std_logic := '0';	
	
	signal ram_wr           : std_logic := '0';
	signal vid_wr           : std_logic := '0';
   signal mmr_rd           : std_logic := '0';
	signal mmr_wr           : std_logic := '0';
	signal spr_wr           : std_logic := '0';
	signal snd_wr           : std_logic := '0';
	
	signal rom_data         : std_logic_vector(15 downto 0);
	signal vid_data         : std_logic_vector(15 downto 0);
	
	signal int_vector   	   : std_logic_vector(7 downto 0);
	signal reg_data   	   : std_logic_vector(7 downto 0);
	
	signal bus_ad           : std_logic_vector(15 downto 0);
	
	signal Global_Reset     : std_logic;
	signal irq_cnt          : std_logic_vector(4 downto 0);
	signal lastcoin   		: std_logic;

	-- watchdog
	signal watchdog_cnt     : std_logic_vector(2 downto 0);
	signal watchdog_clear   : std_logic;
	signal watchdog_reset   : std_logic;
	signal watchdog_cnt2    : std_logic_vector(4 downto 0);
	
	-- Video
	signal vid_addr         : std_logic_vector(12 downto 0);
	signal v_bitmap_data    : std_logic_vector(7 downto 0);
	signal v_colour_page	   : std_logic_vector(2 downto 0) := "000";
	signal v_background     : std_logic := '0';
	signal Sprite_Collision : std_logic_vector(11 downto 0);
	signal ClearCollision   : std_logic := '0';
	signal Sprite_H			: std_logic_vector(7 downto 0) := x"00";
	signal Sprite_V			: std_logic_vector(7 downto 0) := x"00";
	signal Sprite_C	      : std_logic_vector(3 downto 0);
	signal Sprite_I	      : std_logic_vector(3 downto 0);
	signal Screen_Flip      : std_logic := '0';

	-- Sound
	signal Sound_EN		   : std_logic := '0';
	signal March_Select	   : std_logic_vector(2 downto 0);
	signal Gun_Die          : std_logic := '0';
	
	-- Hiscore system
	signal vid_a_addr			: std_logic_vector(12 downto 0);
	signal vid_a_q				: std_logic_vector(7 downto 0);
	signal vid_a_data			: std_logic_vector(7 downto 0);
	signal vid_a_wren			: std_logic;
	signal vid_a_en			: std_logic;

	-- Debug
	signal FirstWord        : std_logic_vector(15 downto 0);
	signal CRUread          : std_logic_vector(15 downto 0);
	
	-- TMS9980 CPU
	COMPONENT tms9900
	 GENERIC (cycle_clks_g : integer);
    PORT(
         clk      : IN  std_logic;
			enable   : in  STD_LOGIC;		-- CPU Enable
         reset    : IN  std_logic;
         addr_out : OUT  std_logic_vector(15 downto 0);
         data_in  : IN  std_logic_vector(15 downto 0);
         data_out : OUT  std_logic_vector(15 downto 0);
         rd       : OUT  std_logic;
         wr       : OUT  std_logic;
         ready    : IN  std_logic := '1';
         iaq      : OUT  std_logic;
         as       : OUT  std_logic;
			alu_debug_arg1 : OUT  std_logic_vector(15 downto 0);
			alu_debug_arg2 : OUT  std_logic_vector(15 downto 0);
			int_req	: in STD_LOGIC;									-- interrupt request, active high
			ic03     : in STD_LOGIC_VECTOR(3 downto 0);			-- interrupt priority for the request, 0001 is the highest (0000 is reset)
			int_ack	: out STD_LOGIC;									-- does not exist on the TMS9900, when high CPU vectors to interrupt ?
			cpu_debug_out : out STD_LOGIC_VECTOR (95 downto 0);	
			cruin		: in STD_LOGIC;
			cruout   : out STD_LOGIC;
			cruclk   : out STD_LOGIC;
			hold     : in STD_LOGIC;
			holda    : out STD_LOGIC;
			waits    : in STD_LOGIC_VECTOR(7 downto 0);
			scratch_en : in STD_LOGIC;		-- when 1 in-core scratchpad RAM is enabled
         stuck    : OUT  std_logic;
         turbo    : in STD_LOGIC
        );
    END COMPONENT;

begin

  O_HBLANK <= hblank;
  O_VBLANK <= vblank;
  O_VCOUNT <= vcnt;
  O_HCOUNT <= hcnt;
  O_FLIP_C <= Screen_Flip;
  
  --Global_Reset <= watchdog_reset or reset;
  Global_Reset <= reset;
  
  waits <= "00000000";
  cpu_ready <= '1';

  --
  -- video timing
  --
  
  sync_stop : process(RESET,I_H_OFFSET,I_V_OFFSET)
  begin
		-- work out locations for sync pulses
		hsync_start <= std_logic_vector(to_unsigned(200 + to_integer(signed(I_H_OFFSET)),9));
		hsync_end   <= std_logic_vector(to_unsigned(214 + to_integer(signed(I_H_OFFSET)),9));
		vsync_start <= std_logic_vector(to_unsigned(252 + to_integer(signed(I_V_OFFSET)),9));
		vsync_end   <= std_logic_vector(to_unsigned(255 + to_integer(signed(I_V_OFFSET)),9));
  end process;
  
  p_hvcnt : process
    variable hcarry,vcarry : boolean;
  begin
    wait until rising_edge(CLK);
    if (PIX_CLK = '1') then
      hcarry := (hcnt = "111111111");
      if hcarry then
        --hcnt <= "011000000"; -- 0C0
		  hcnt <= "010101011"; -- 0AB
      else
        hcnt <= hcnt +"1";
      end if;
      
      vcarry := (vcnt = "111111111");
      if do_hsync then
        if vcarry then
          vcnt <= "011111000"; -- 0F8
        else
          vcnt <= vcnt +"1";
        end if;
      end if;	

		-- Debug data
		
		-- rom word
		Line1(4 downto 0) <= '0' & FirstWord(15 downto 12);
		Line1(9 downto 5) <= '0' & FirstWord(11 downto 8);
		Line1(14 downto 10) <= '0' & FirstWord(7 downto 4);
		Line1(19 downto 15) <= '0' & FirstWord(3 downto 0);
		
		-- PC address
		Line1(29 downto 25) <= '0' & bus_ad(15 downto 12);
		Line1(34 downto 30) <= '0' & bus_ad(11 downto 8);
		Line1(39 downto 35) <= '0' & bus_ad(7 downto 4);
		Line1(44 downto 40) <= '0' & bus_ad(3 downto 0);

		-- rom data
		Line1(54 downto 50) <= '0' & rom_data(15 downto 12);
		Line1(59 downto 55) <= '0' & rom_data(11 downto 8);
		Line1(64 downto 60) <= '0' & rom_data(7 downto 4);
		Line1(69 downto 65) <= '0' & rom_data(3 downto 0);
		
		-- cru address
		Line1(79 downto 75) <= '0' & CRUread(15 downto 12);	
		Line1(84 downto 80) <= '0' & CRUread(11 downto 8);
		Line1(89 downto 85) <= '0' & CRUread(7 downto 4);
		Line1(94 downto 90) <= '0' & CRUread(3 downto 0);
		
		-- Colour page selected
		Line1(104 downto 100) <= "0000" & watchdog_reset;

		-- Spaces
		Line1(24 downto 20) <= "10000";
		Line1(49 downto 45) <= "10000";
		Line1(74 downto 70) <= "10000";
		Line1(99 downto 95) <= "10000";
		Line1(109 downto 105) <= "10000";	  
	  
		end if;
  end process;

  p_sync_comb : process(hcnt, vcnt, hsync_start)
  begin
    do_hsync <= (hcnt = hsync_start);
    set_vblank <= (vcnt = "111100000"); -- 1E0
  end process;

  p_sync : process
  begin
    wait until rising_edge(CLK);
    -- Timing hardware is coded differently to the real hw
    if (PIX_CLK = '1') then
      if (hcnt = "010101100") then 
        hblank <= '1';
      elsif (hcnt = "011111111") then
        hblank <= '0';
      end if;

      if do_hsync then
        hsync <= '1';
      elsif (hcnt = hsync_end) then 
        hsync <= '0';
      end if;

      if do_hsync then
        if set_vblank then -- 1EF
          vblank <= '1';
        elsif (vcnt = "100011111") then
          vblank <= '0';
        end if;
		  
		  if (vcnt = vsync_start) then
			  vsync <= '0';
		  elsif (vcnt = vsync_end) then
			  vsync <= '1';
		  end if;	
      end if;
    end if;
  end process;

  p_video_timing_reg : process
  begin
    wait until rising_edge(CLK);
    -- match output delay in video module
    if (PIX_CLK = '1') then
      O_HSYNC     <= hsync;
      O_VSYNC     <= vsync;
    end if;
  end process;
  
  p_cpu_int : process
  begin
   wait until rising_edge(CLK);
	
	if Global_Reset='1' then
	    cpu_int <= '0';
	else
		-- use int 4 for coin - real hardware uses NE556 timer, but we'll use the Int-Ack flag instead
		lastcoin <= coin;
		if (lastcoin = '0' and coin = '1') then
			cpu_int <= '1';
		else
			if cpu_int = '1' then
				if cpu_int_ack = '1' then
					cpu_int <= '0';
				end if ;
			end if ;
		end if;
	end if;		
	
 end process;

 -- Watchdog. Real system uses NE556 set to trigger after about 8 video frames
-- process 
-- begin
--   wait until rising_edge(CLK);
--
--	if reset='1' then	-- External reset
--		watchdog_cnt <= "000";
--		watchdog_cnt2 <= "00000";
--		watchdog_reset <= '0';
--	else
--		if watchdog_reset='1' then
--			if watchdog_cnt2="00000" then
--				watchdog_reset <= '0';
--			else
--				watchdog_cnt2 <= watchdog_cnt2 - 1;
--			end if;
--		end if;
--		
--		-- Frame count done on PIX_CLK timing
--		if PIX_CLK = '1' then
--			if PAUSED='0' then
--				if watchdog_clear = '1' then
--					watchdog_cnt <= (others => '0');
--				else
--					if vcnt = "111111111" and hcnt = "111111111" then
--						watchdog_cnt <= watchdog_cnt + 1;
--					end if;
--					
--					if watchdog_cnt = "111" then
--						watchdog_reset <= '1'; 	-- Reboot!
--						watchdog_cnt2 <= "11111";
--					end if;
--					end if;
--			end if;
--		end if;
--	end if;
--	
-- end process;
	
--
-- cpu
--
	cpu : tms9900
		generic map (
			cycle_clks_g => 3					   -- used to scale wait states around various commands for memory access etc. (was 4)
		)
	PORT MAP (
          clk => CLK,
			 enable => CPU_ENA,
          reset => Global_Reset,				-- external reset or watchdog
          addr_out => cpu_addr,
          data_in => cpu_data_in,
          data_out => cpu_data_out,
          rd => cpu_rd_l,
          wr => cpu_wr_l,
          ready => cpu_ready,
          iaq => cpu_iaq,
          as => cpu_as,							-- address strobe
			 alu_debug_arg1 => open,
			 alu_debug_arg2 => open,
			 int_req => cpu_int,
			 ic03 => "0100",						-- interrupt priority (= 4 ?)
			 int_ack => cpu_int_ack,
		    cpu_debug_out => open,
			 cruin => cpu_cruin,
			 cruout => cpu_cruout,
			 cruclk => cpu_cruclk,
			 hold => PAUSED,
			 holda => cpu_holda,
			 waits => waits,
			 scratch_en => '0',					-- Don't use scratchram
          stuck => cpu_stuck,
			 turbo => '0'							-- Don't use Turbo mode
   );
		  
--
-- Cosmic Guerilla mappings - Memory access
-- All done in 16 bit, although real hardware is 8 bit access using TMS9980
--
--	map(0x0000, 0x1fff).rom();
--	map(0x2000, 0x3fff).ram().share("videoram");

-- when MEMEN is low, the CPU wants to access memory and has placed a valid address on the bus
-- when DBIN is high, the CPU is ready to accept data 
-- when WE is low, the CPU has placed valid data on the bus 

rom_ld <= '1' when dn_addr(15 downto 13)  = "000" and dn_ld='1' else '0';
bus_ad <= dn_addr(15 downto 0) when dn_ld='1' else cpu_addr;

-- Need to decrypt and patch on loading rom data
--         UINT8 normal = (scrambled >> 3 & 0x11)
--                      | (scrambled >> 1 & 0x22)
--                      | (scrambled << 1 & 0x44)
--                      | (scrambled << 3 & 0x88);
-- Also fix for bitrot
rom_in <= x"04C0" when dn_addr(15 downto 0) = x"1e9f" else dn_low & dn_data(4) & dn_data(5) & dn_data(6) & dn_data(7) & dn_data(0) & dn_data(1) & dn_data(2) & dn_data(3);

 -- Save low byte for loading 16 bit
 ROM_LOAD : process
 begin
	wait until rising_edge(CLK);
	if rom_ld = '1' then
		if dn_addr(0)='0' then
			dn_low <= dn_data(4) & dn_data(5) & dn_data(6) & dn_data(7) & dn_data(0) & dn_data(1) & dn_data(2) & dn_data(3);
		end if;
		
		-- save first word loaded
		if dn_addr(15 downto 0) = x"0021" then
			FirstWord <= dn_low & dn_data(4) & dn_data(5) & dn_data(6) & dn_data(7) & dn_data(0) & dn_data(1) & dn_data(2) & dn_data(3);
		end if;
	end if;
 end process;
  
 program_rom : entity work.spram
	generic map (
	  addr_width => 12,
	  data_width => 16
	)
	port map (
	  q        => rom_data,
	  data     => rom_in,
	  address  => bus_ad(12 downto 1),
	  wren     => dn_wr and rom_ld and bus_ad(0),
	  clock    => CLK
   );

  -- program and video ram 
 video_ram : entity work.dpram_dif
	 generic map (
	  addr_width_a => 12,
	  data_width_a => 16,
	  addr_width_b => 13,
	  data_width_b => 8
	 )
	 port map (
	  q_a        => vid_data,
	  data_a     => cpu_data_out,
	  address_a  => cpu_addr(12 downto 1),
	  wren_a     => vid_wr,
	  enable_a   => (vid_wr or vid_rd),
	  clock      => CLK,
	  
	  address_b  => vid_addr(12 downto 1) & not vid_addr(0),
	  q_b        => v_bitmap_data
	 );

 -- Memory decode
	 
p_mem_decode : process(cpu_addr,cpu_rd_l,cpu_wr_l,cpu_as)
variable address : natural range 0 to 2**15 - 1;
begin
	rom_rd <= '0';
	vid_rd <= '0';
	vid_wr <= '0';

	address := to_integer(unsigned(cpu_addr));
	
	-- Ram/Rom read or write
	--if cpu_as='1' then
		if cpu_rd_l='1' then
				case address is
					when 16#0000# to 16#1FFF# => rom_rd <= '1';
					when 16#2000# to 16#3FFF# => vid_rd <= '1';
					when others => null;
				end case;
		elsif cpu_wr_l='1' then
				case address is
					when 16#2000# to 16#3FFF# => vid_wr <= '1';
					when others => null;
				end case;
		end if;
	--end if;

end process;

 -- Mux back to CPU
 
 p_cpu_src_data_mux : process(rom_data,vid_data,rom_rd,vid_rd)
 begin
	 if rom_rd = '1' then
		cpu_data_in <= rom_data;
	 elsif vid_rd = '1' then
		cpu_data_in <= vid_data;
	 else 
	   cpu_data_in <= x"FFFF";
 	 end if;
 end process;


-- Memory mapped registers
--
-- when CRUCLK is high, CPU performing a CRU write operation
-- CRUOUT is bit for writes

-- CRU write
--  0 - 43 = mostly sound
-- 44 - 47 = colour register

MMR_Write : process (CLK)
variable address : natural range 0 to 2**15 - 1;
variable SoundBit : std_logic;

begin
	if rising_edge(CLK) then

		if Global_Reset='1' then
			Screen_Flip <= '0';
			O_SoundPort <= "0000000000000000";
			O_AUDIO     <= "0000000000000000";  -- DAC not used for this one it seems
			O_SoundStop <= "1111111111111111";
			Sound_EN    <= '0';
			March_Select <= "000";
			Gun_Die <= '0';
		else
			if CPU_ENA='1' then
			
				O_SoundStop <= "0000000000000000";
				
				-- reset flag if counter has been cleared
				if watchdog_cnt = "000" then 
					watchdog_clear <= '0';
				end if;

				if cpu_cruclk='1' then
				
					address := to_integer(unsigned(cpu_addr(12 downto 1)));
					SoundBit := cpu_cruout and Sound_EN;
					
					case address is
						--   16#0001# => DAC - Not used by this game
						when 16#0002# => case March_Select is
												 when "000" => O_SoundStop(0) <= SoundBit;
																	O_SoundPort(0) <= SoundBit;
												 when "001" => O_SoundStop(1) <= SoundBit;
																	O_SoundPort(1) <= SoundBit;
												 when "010" => O_SoundStop(2) <= SoundBit;
																	O_SoundPort(2) <= SoundBit;
												 when "011" => O_SoundStop(3) <= SoundBit;
																	O_SoundPort(3) <= SoundBit;
												 when "100" => O_SoundStop(4) <= SoundBit;
																	O_SoundPort(4) <= SoundBit;
												 when "101" => O_SoundStop(5) <= SoundBit;
																	O_SoundPort(5) <= SoundBit;
												 when "110" => O_SoundStop(6) <= SoundBit;
																	O_SoundPort(6) <= SoundBit;
												 when "111" => O_SoundStop(7) <= SoundBit;
																	O_SoundPort(7) <= SoundBit;
											 end case;

						when 16#0003# => March_Select(0) <= cpu_cruout;
						when 16#0004# => March_Select(1) <= cpu_cruout;
						when 16#0005# => March_Select(2) <= cpu_cruout;
						when 16#0006# => O_SoundPort(8) <= SoundBit;
						when 16#0007# => O_SoundStop(10) <= SoundBit;  -- restart if playing
											  O_SoundPort(10) <= SoundBit;
						when 16#0008# => O_SoundPort(9) <= SoundBit;
											  if (SoundBit='0') then		  -- stop 10 as well as 9
												O_SoundStop(10) <= '1';
											  end if;										  
						when 16#0009# => O_SoundPort(11) <= SoundBit;
											  O_SoundStop(9) <= '1';		  -- stop saucer noise if you hit it.
						when 16#000A# => Screen_Flip <= cpu_cruout and cabinet; -- screen flip - is ignored if cabinet set to upright, but P2 uses own controls in either. (unless done in wiring!)
						when 16#000B# => watchdog_clear <= '1'; -- Watchdog - write to this resets counter
						when 16#000C# => Sound_EN <= cpu_cruout;
											  if (cpu_cruout='0') then
													-- Stop all sounds as well
													O_SoundPort <= "0000000000000000";
													O_SoundStop <= "1111111111111111";
											  end if;
						when 16#000D# => if (Gun_Die='1') then
												 O_SoundPort(12) <= SoundBit;
											  else
												 O_SoundPort(13) <= SoundBit;
											  end if;
						when 16#000E# => Gun_Die <= cpu_cruout;
						when 16#000F# => O_SoundPort(14) <= SoundBit;
						when 16#0016# => v_colour_page(0) <= cpu_cruout;
						when 16#0017# => v_colour_page(1) <= cpu_cruout;
						when others => null;
					end case;
				end if;
			end if;
		end if;
	end if;
end process;

O_Sound_EN  <= Sound_EN;

-- CRU Read

-- we can just set CRUIN based on address, since it will be ignored by CPU if not required

MMR_read : process (CLK)
variable address : natural range 0 to 2**15 - 1;
variable ki : integer range 0 to 7;
variable VertPos : std_logic_vector(3 downto 0);
begin
	if rising_edge(CLK) then
		if (CPU_ENA='1') then

			address := to_integer(unsigned(cpu_addr(15 downto 4)));  -- main select 
			ki := to_integer(unsigned(cpu_addr(3 downto 1))); 	
			
			-- Modified vertical counter, to only include 0 - 15
			if vcnt(8) = '0' then
				VertPos := "0000";
			else
				VertPos := vcnt(7 downto 4);
			end if;
			
			case address is
				when 0 =>
					case ki is
						-- Vertical counter
						when  0 => cpu_cruin <= VertPos(0);
						when  1 => cpu_cruin <= VertPos(1);
						when  2 => cpu_cruin <= VertPos(2);
						when  3 => cpu_cruin <= VertPos(3);
						-- Controls
						when  4 => cpu_cruin <= in0(0);  -- start 1
						when  5 => cpu_cruin <= in0(1);  -- start 2
						when  6 => cpu_cruin <= in0(2);  -- P1 button 1
						when  7 => cpu_cruin <= in0(3);  -- P1 right
					end case;
				when 1 =>
					case ki is
						when 0 => cpu_cruin <= in1(0); -- P1 Left
						when 1 => cpu_cruin <= in1(1); -- P2 Button
						when 2 => cpu_cruin <= in1(2); -- P2 Right
						when 3 => cpu_cruin <= in1(3); -- P2 Left
						-- DIPS
						when 4 => cpu_cruin <= dipsw1(0);
						when 5 => cpu_cruin <= dipsw1(1);
						when 6 => cpu_cruin <= dipsw1(2);
						when 7 => cpu_cruin <= dipsw1(3);
					end case;
				-- Default (shouldn't be any other reads, but who knows!)
				when others => cpu_cruin <= '0';
			end case;
			
		end if;
	end if;
end process;

--
-- video subsystem
--
-- Bitmap graphics using colour prom
--
-- needs 
-- in  - x,y,flip,vid_data
-- out - vidaddr,R,G,B
-- colour rom to load internally, needs to track colourmap change writes (4 bits)
-- rom load 

video : work.COSMIC_VIDEO
port map (
	I_HCNT    => hcnt,
	I_VCNT    => vcnt,
	--
	I_S_FLIP  => Screen_Flip,
	I_H_FLIP  => I_FLIP,
	I_BITMAP  => v_bitmap_data,
	I_COL     => v_colour_page,
	I_BW      => I_MONO,
	I_BLUE	 => I_BLUE,
	O_VADDR   => vid_addr,
	--
	dn_addr   => dn_addr,
	dn_data   => dn_data,
	dn_wr     => dn_wr,
	dn_ld     => dn_ld,
	--
	O_RED     => O_VIDEO_R,
	O_GREEN   => O_VIDEO_G,
	O_BLUE    => O_VIDEO_B,
	--
	PIX_CLK	 => PIX_CLK,
	CLK       => CLK,
	CPU_ENA   => CPU_ENA,
	GAME      => GAME,
   PAUSED    => PAUSED
);

end RTL;
