use core::mem::MemBlock;

pub struct PPU
{}

impl MemBlock for PPU
{
	fn read8(&self, loc: u16) -> u8
	{
		println!("IO read at 0x{:x}!", loc);

		0
	}

	fn write8(&mut self, loc: u16, v: u8)
	{
		println!("IO write at 0x{:x}!", loc);
	}
}
