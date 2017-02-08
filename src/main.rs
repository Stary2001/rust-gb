mod core;
mod sdl;
use sdl::window::SDLWindow;
use core::window::Window;
use core::cpu::CPU;

fn main()
{
	let mut c: CPU = CPU::new();
	c.bios = core::loader::load_from_file("./bios.bin");
	c.cart = core::loader::load_from_file("./cart.bin");

	/*for n in 1..0x10000
	{
		let i = c.decode();
		c.exec(i);
	}*/
	let mut w = SDLWindow::new();
	loop
	{
		c.ppu.start_frame();
		for i in 0..10000
		{
			if (i % (10000 / 153)) == 0
			{
				c.ppu.step();
			}

			let instr = c.decode();
			c.exec(instr);
		}

		w.do_frame();
		w.process_events();
		if w.should_quit()
		{
			break
		}
	}
}
