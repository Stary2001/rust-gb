extern crate sdl2;
use self::sdl2::*;
use self::event::Event;
use self::render::Renderer;
use self::render::Texture;
use sdl::window::sdl2::pixels::PixelFormatEnum::*;
use core::window::Window;
use core::ppu::PPU;
use self::pixels::Color;

pub struct SDLWindow<'a>
{
	sdl_context: Sdl,
	events: EventPump,
	renderer: Renderer<'a>,
	screen_texture: Texture,
	should_quit_bool: bool
}

impl<'a> SDLWindow<'a>
{
	pub fn new() -> SDLWindow<'a>
	{
		let sdl = sdl2::init().unwrap();
		let video = sdl.video().unwrap();
		let window = video.window("Test", 160, 144).build().unwrap();
		let render = window.renderer().build().unwrap();
		let event_pump = sdl.event_pump().unwrap();
		let tex = render.create_texture_streaming(RGBA8888, 160, 144).unwrap();

		SDLWindow {sdl_context: sdl, events: event_pump, renderer: render, screen_texture: tex, should_quit_bool: false}
	}

	fn blit(p: &PPU, pixels: &mut [u8], _: usize)
	{
		let palette: Vec<u8> = vec!{0xff, 0xaa, 0x88, 0x00};

		for y in 0..p.display.height
		{
			for x in 0..p.display.width
			{
				let off = y * p.display.width + x;
				let rgba_off = off * 4;

				pixels[rgba_off+3] = palette[(p.display.data[off] & 3) as usize];
				pixels[rgba_off+2] = palette[(p.display.data[off] & 3) as usize];
				pixels[rgba_off+1] = palette[(p.display.data[off] & 3) as usize];
				pixels[rgba_off] = 0xff;
			}
		};
	}
}

impl<'a> Window for SDLWindow<'a>
{
	fn process_events(&mut self)
	{
		for ev in self.events.poll_event()
		{
			println!("{:?}", ev);
			match ev
			{
				Event::Quit{..} => { self.should_quit_bool = true },
				_ => ()
			}
		}
	}

	fn do_frame(&mut self, p: &PPU)
	{
		self.renderer.set_draw_color(Color::RGB(255, 0, 0));
		self.renderer.clear();

		let ref mut scr = self.screen_texture;
		scr.with_lock(None, |pixels: &mut [u8], len: usize| { SDLWindow::blit(p, pixels, len) }).expect("Texture lock failed!");
		self.renderer.copy(scr, None, None).expect("Texture copy failed!");
		self.renderer.present();
	}

	fn should_quit(&self) -> bool
	{
		self.should_quit_bool
	}
}