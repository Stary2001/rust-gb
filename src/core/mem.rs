pub struct MemMapper
{
	pub vram: MemRegion,
	pub wram: MemRegion,
	pub hram: MemRegion,
	pub cart: MemRegion,
	pub bios: MemRegion, // BIOS will shadow the first part of cart when enabled
	pub bios_enabled: bool
}

pub enum MemRegion
{
	Empty,
	Block(Vec<u8>)
}

impl MemMapper
{
	pub fn read8(&self, loc: u16) -> u8
	{
		match loc
		{
			0...0xff => match self.bios
						{
							MemRegion::Block(ref b) => b[loc as usize],
							MemRegion::Empty => match self.cart
												{
													MemRegion::Block(ref b2) => b2[loc as usize],
													MemRegion::Empty => panic!("no")
												}
						},
			_ => panic!("unmapped mem read 0x{:x}", loc)
		}
	}

	pub fn write8(&self, loc: u16, v: u8)
	{
		println!("Write to 0x{:x}: 0x{:x}", loc, v);
	}

	pub fn read16(&self, loc: u16) -> u16
	{
		match loc
		{
			0...0xff => match self.bios
						{
							MemRegion::Block(ref b) => (b[(loc + 1) as usize] as u16) << 8 | (b[loc as usize] as u16),
							MemRegion::Empty => match self.cart
												{
													MemRegion::Block(ref b2) => (b2[(loc + 1) as usize] as u16) << 8 | (b2[loc as usize] as u16),
													MemRegion::Empty => panic!("no")
												}
						},
			_ => panic!("unmapped mem read of 0x{:x}", loc)
		}
	}
}