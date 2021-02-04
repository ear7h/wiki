#![feature(or_patterns)]

use termion::{
    raw:: IntoRawMode,
    input::{
        TermReadEventsAndRaw,
        MouseTerminal,
    },
    event::{
        Event,
        Key,
    },
};

use std::io::{
    stdout,
    stdin,
    Write,
};

fn main() {

    let mut stdout = MouseTerminal::from(stdout().into_raw_mode().unwrap());

    let stdin = stdin();

    for (evt, bytes) in stdin.events_and_raw().map(Result::unwrap) {
        write!(stdout, "{:?}{:x?}\r\n", evt, bytes).unwrap();
        stdout.flush().unwrap();

        match evt {
            Event::Key(Key::Ctrl('c' | 'd')) => break,
            _ => {},
        }
    }
}
