pub trait MemBlock
{
	fn read8(&self, loc: u16) -> u8
	{
		panic!("unhandled 8-bit read at 0x{:x}!", loc);
	}

	fn write8(&mut self, loc: u16, v: u8)
	{
		panic!("unhandled 8-bit write at 0x{:x}!", loc);
	}
}

pub struct RAMBlock
{
	pub base: usize,
	pub v: Vec<u8>
}

pub struct ROMBlock
{
	pub base: usize,
	pub v: Vec<u8>
}

pub struct EmptyBlock
{}

impl MemBlock for RAMBlock
{
	fn read8(&self, loc: u16) -> u8
	{
		self.v[loc as usize - self.base]
	}

	fn write8(&mut self, loc: u16, v: u8)
	{
		self.v[loc as usize - self.base] = v;
	}
}

impl MemBlock for ROMBlock
{
	fn read8(&self, loc: u16) -> u8
	{
		self.v[loc as usize - self.base]
	}
}

impl MemBlock for Option<ROMBlock>
{
	fn read8(&self, loc: u16) -> u8
	{
		match *self
		{
			Some(ref v) => v.v[loc as usize - v.base],
			None => panic!("No ROM!")
		}
		
	}
}

impl MemBlock for EmptyBlock
{}