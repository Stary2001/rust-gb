use core::mem::MemBlock;

pub struct PPU
{
	pub lcdc: u8,
	pub stat: u8,
	pub scanline: u8,
	pub scroll_x: u8,
	pub scroll_y: u8,
	pub lyc: u8,
	pub wx: u8,
	pub wy: u8,
	pub bg_palette: u8,
	pub obj0_palette: u8,
	pub obj1_palette: u8
}

impl PPU
{
	pub fn start_frame(&mut self)
	{
		self.scanline = 0;
	}

	pub fn step(&mut self)
	{
		if self.scanline != 153
		{
			self.scanline += 1;
		}
	}
}

impl MemBlock for PPU
{
	fn read8(&self, loc: u16) -> u8
	{
		//println!("PPU read at 0x{:x}!", loc);

		match loc - 0xff40
		{
			0 => self.lcdc,
			1 => self.stat,
			2 => self.scroll_y,
			3 => self.scroll_x,
			4 => self.scanline,
			5 => self.lyc,

			7 => self.bg_palette,
			8 => self.obj0_palette,
			9 => self.obj1_palette,
			0xa => self.wy,
			0xb => self.wx - 7,
			_ => 0
		}
	}

	fn write8(&mut self, loc: u16, v: u8)
	{
		println!("PPU write at 0x{:x}!", loc);
		match loc - 0xff40
		{
			0 => self.lcdc = v,
			1 => self.stat = v,
			2 => self.scroll_y = v,
			3 => self.scroll_x = v,
			4 => panic!("write to y-coord reg"),
			7 => self.bg_palette = v,
			8 => self.obj0_palette = v,
			9 => self.obj1_palette = v,
			0xa => self.wy = v,
			0xb => self.wx = v + 7,
			_ => panic!("unimplemented ppu write {:x}", loc)
		};
	}
}
