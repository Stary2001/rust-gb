use core::mem::MemBlock;
use core::cpu::CPU;
use core::mem::RAMBlock;

pub struct Display
{
	pub width: usize,
	pub height: usize,
	pub bpp: usize,
	pub data: Vec<u8>
}

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

	pub bg_palette_reg: u8,
	pub bg_palette: Vec<u8>,
	pub obj0_palette_reg: u8,
	pub obj0_palette: Vec<u8>,
	pub obj1_palette_reg: u8,
	pub obj1_palette: Vec<u8>,

	pub vram: RAMBlock,
	pub oam: RAMBlock,
	pub display: Display
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

	pub fn end_frame(&mut self)
	{
		// Blit stuff here. Yay.

		if self.lcdc & 1<<0 == 1<<0
		{
			println!("BG enabled!");
			let bg_tilemap_off = if self.lcdc & 1 << 3 == 1 << 3 { 0x9c00 } else { 0x9800 };

			let xoff = self.scroll_x as isize;
			let yoff = self.scroll_y as isize;

			let tx0 = self.scroll_x as isize / 8;
			let tx1 = tx0 + 20;
			let ty0 = self.scroll_y as isize / 8;
			let ty1 = ty0 + 18;

			for x in tx0 .. tx1
			{
				for y in ty0 .. ty1
				{
					let tile_no = self.vram.read8(bg_tilemap_off + (x%32 + (y%32)*32) as u16);

					let scr_x = (x%32)*8 - xoff;
					let scr_y = (y%32)*8 - yoff;
					let clipx = if scr_x < 0 { -scr_x } else { 0 };
					let clipy = if scr_y < 0 { -scr_y } else { 0 };

					self.render_tile(scr_x, scr_y, tile_no as usize, clipx, clipy);
				}
			}
		}
	}

	fn render_tile(&mut self, x: isize, y: isize, tile_no: usize, clip_x: isize, clip_y: isize)
	{
		let tiledata_off = if self.lcdc & 1 << 4 == 1 << 4 { 0x8000 } else { 0x8800 };
		let mut tile_x = 0;
		let mut tile_y = 0;

		for i in clip_y..8
		{
			let tile_upper = self.vram.read8(tiledata_off + (tile_no as u16) * 16 + (i*2) as u16);
			let tile_lower = self.vram.read8(tiledata_off + (tile_no as u16) * 16 + (i*2) as u16 + 1);

			for j in clip_x..8
			{
				let jj = 7 - j;
				let off = (x + j + (y + i) * self.display.width as isize) as usize;
				let palette_index = (tile_upper & (1<<jj)) >> jj << 1 | (tile_lower & (1<<jj)) >> jj;
				self.display.data[off] = self.bg_palette[palette_index as usize];
			}
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

			7 => self.bg_palette_reg,
			8 => self.obj0_palette_reg,
			9 => self.obj1_palette_reg,
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
			7 => {
					self.bg_palette_reg = v;
					self.bg_palette[0] = v & 3 << 0;
					self.bg_palette[1] = (v & 3 << 2) >> 2;
					self.bg_palette[2] = (v & 3 << 4) >> 4;
					self.bg_palette[3] = (v & 3 << 6) >> 6;
				 },

			8 => self.obj0_palette_reg = v,
			9 => self.obj1_palette_reg = v,
			0xa => self.wy = v,
			0xb => self.wx = v + 7,
			_ => panic!("unimplemented ppu write {:x}", loc)
		};
	}
}
