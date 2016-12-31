mod core;
use core::cpu::CPU;

fn main()
{
	let mut c: CPU = CPU::new();
	c.mmu.bios = core::loader::make_region_from_file("./bios.bin").unwrap();
	c.mmu.cart = core::loader::make_region_from_file("./cart.bin").unwrap();

	for n in 1..0x10000
	{
		let i = c.decode();
		c.exec(i);
	}
}
