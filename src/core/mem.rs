pub struct IOHandler
{}

pub struct MemMapper
{
	pub vram: MemRegion,
	pub wram: MemRegion,
	pub hram: MemRegion,
	pub cart: MemRegion,
	pub bios: MemRegion, // BIOS will shadow the first part of cart when enabled
	pub bios_enabled: bool,
	pub io: MemRegion
}

impl IOHandler
{
	fn read8(&self, loc: u16) -> u8
	{
		println!("IO read at 0x{:x}!", loc);

		0
	}

	fn write8(&self, loc: u16, v: u8)
	{
		println!("IO write at 0x{:x}!", loc);
	}
}

pub enum MemRegion
{
	Empty,
	Read(usize, Vec<u8>),
	ReadWrite(usize, Vec<u8>),
	IO(usize, IOHandler)
}

impl MemMapper
{
	fn map(&self, loc: u16) -> &MemRegion
	{
		match loc
		{
			0...0xff => { if self.bios_enabled { &self.bios } else { &self.cart } }
			0x100 ... 0x3fff => &self.cart,
			0x8000 ... 0x9fff => &self.vram,
			0xff00...0xff7f => &self.io,
			0xff80...0xfffe => &self.hram,
			_ => panic!("unmapped mem access at 0x{:x}", loc)
		}
	}

	fn map_mut(&mut self, loc: u16) -> &mut MemRegion
	{
		match loc
		{
			0...0xff => { if self.bios_enabled { &mut self.bios } else { &mut self.cart } }
			0x100 ... 0x3fff => &mut self.cart,
			0x8000 ... 0x9fff => &mut self.vram,
			0xff00...0xff7f => &mut self.io,
			0xff80...0xfffe => &mut self.hram,
			_ => panic!("unmapped mem access at 0x{:x}", loc)
		}
	}

	pub fn read8(&self, loc: u16) -> u8
	{
		println!("Read8 at 0x{:x}", loc);
		match *self.map(loc)
		{
			MemRegion::Read(base, ref b) | MemRegion::ReadWrite(base, ref b) => b[loc as usize - base],
			MemRegion::IO(base, ref io) => io.read8(loc - base as u16),
			MemRegion::Empty => panic!("empty memblock read at 0x{:x}", loc)
		}
	}

	pub fn write8(&mut self, loc: u16, v: u8)
	{
		println!("Write to 0x{:x}: 0x{:x}", loc, v);
		match *self.map_mut(loc)
		{
			MemRegion::ReadWrite(base, ref mut b) => { println!("{:x} {:x}", base, loc); b[loc as usize - base] = v; },
			MemRegion::IO(base, ref io) => io.write8(loc - base as u16, v),
			_ => panic!("wrong memblock write at 0x{:x}", loc)
		}
	}

	pub fn read16(&self, loc: u16) -> u16
	{
		println!("Read16 at 0x{:x}", loc);
		match *self.map(loc)
		{
			MemRegion::Read(base, ref b) | MemRegion::ReadWrite(base, ref b) => ((b[(loc + 1) as usize - base] as u16) << 8) | (b[loc as usize - base] as u16),
			MemRegion::IO(base, ref io) => panic!("16-bit memblock read?"),
			MemRegion::Empty => panic!("empty memblock read at 0x{:x}", loc)
		}
	}

	pub fn write16(&mut self, loc: u16, v: u16)
	{
		println!("Write to 0x{:x}: 0x{:x}", loc, v);
		match *self.map_mut(loc)
		{
			MemRegion::ReadWrite(base, ref mut b) => { b[(loc+1) as usize - base] = ((v&0xff00) >> 8) as u8; b[loc as usize - base] = (v&0xff) as u8; },
			MemRegion::IO(base, ref io) => panic!("16-bit memblock write?"),
			_ => panic!("wrong memblock write at 0x{:x}", loc)
		}
	}
}