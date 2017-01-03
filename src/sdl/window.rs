extern crate sdl2;
use self::sdl2::*;
use self::event::Event;
use self::render::Renderer;
use core::window::Window;
use self::surface::Surface;
use self::pixels::Color;

pub struct SDLWindow<'a>
{
	sdl_context: Sdl,
	events: EventPump,
	renderer: Renderer<'a>,
	should_quit_bool: bool
}

impl<'a> SDLWindow<'a>
{
	pub fn new() -> SDLWindow<'a>
	{
		let sdl = sdl2::init().unwrap();
		let video = sdl.video().unwrap();
		let window = video.window("Test", 160, 144).build().unwrap();
		let mut render = window.renderer().build().unwrap();
		let mut event_pump = sdl.event_pump().unwrap();
		SDLWindow {sdl_context: sdl, events: event_pump, renderer: render, should_quit_bool: false}
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

	fn do_frame(&mut self)
	{
		self.renderer.set_draw_color(Color::RGB(255, 0, 0));
    	self.renderer.clear();
    	self.renderer.present();
	}

	fn should_quit(&self) -> bool
	{
		self.should_quit_bool
	}
}