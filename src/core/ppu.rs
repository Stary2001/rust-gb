use core::mem::MemBlock;

pub struct PPU
{
	pub lcdc: u8,
	pub scanline: u8,
	pub scroll_x: u8,
	pub scroll_y: u8,
	pub palette: u8
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
			2 => self.scroll_y,
			3 => self.scroll_x,
			4 => self.scanline,
			7 => self.palette,
			_ => 0
		}
	}

	fn write8(&mut self, loc: u16, v: u8)
	{
		println!("PPU write at 0x{:x}!", loc);
		match loc - 0xff40
		{
			0 => self.lcdc = v,
			2 => self.scroll_y = v,
			3 => self.scroll_x = v,
			4 => panic!("write to y-coord reg"),
			7 => self.palette = v,
			_ => panic!("unimplemented ppu write")
		};
	}
}
