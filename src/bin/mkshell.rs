#![allow(unused_imports)]
#![feature(vec_into_raw_parts)]
#![feature(str_internals)]
#![feature(shrink_to)]

use nix::fcntl::{
    fcntl,
    FcntlArg,
    OFlag,
};

use termion::{
    self,
    event::Key,
    input::TermRead,
    raw::{
        RawTerminal,
        IntoRawMode,
    },
    clear,
    cursor,
};

use unicode_width::{
    UnicodeWidthStr,
    UnicodeWidthChar,
};

use mio::{
    Events, Interest, Poll, Registry, Token,
    event::Event,
    unix::SourceFd,
};

use aho_corasick::{
    self as ac,
    AhoCorasick,
    AhoCorasickBuilder,
};

use core::str::utf8_char_width;

use std::{
    str::{
        from_utf8_unchecked,
    },
    thread,
    process::{
        Command,
        Stdio,
        ChildStdout,
    },
    os::unix::io::{
        RawFd,
        AsRawFd,
    },
    io::{
        ErrorKind,
        Write,
        Read,
        stdout,
        stdin,
        Stdin,
        Error,
    },
};

use wiki::tst::{
    Tst,
    TstScanner,
    TstToken,
};

type Result<T> = std::result::Result<T, Error>;


#[derive(Debug, Clone, Default)]
struct Line {
    s : String,
}


impl Line {

    fn len(&self) -> usize {
        self.s.len()
    }

    fn as_str(&self) -> &str {
        self.s.as_str()
    }

    fn reset(&mut self) {
        self.s.truncate(0);
        if self.s.capacity() > 256 {
            self.s.shrink_to(256);
        }
    }

    fn push(&mut self, c : char) {
        self.s.push(c);
    }
}

struct LineBuf {
    lines : Vec<Line>,

    // oh yea that sweet 3 byte savings on padding
    overflow : [u8; 3],
    overflow_len : u8,

    start : usize,
    len : usize,
    chunk_size : usize,
}

impl std::fmt::Debug for LineBuf {
    fn fmt(&self, f : &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.iter())
            .finish()
    }
}

#[allow(dead_code)]
impl LineBuf {
    fn new(chunk_size : usize, nchunk : usize ) -> LineBuf {

        let mut lines = Vec::new();
        lines.resize_with(nchunk, Default::default);

        LineBuf {
            lines : lines,
            overflow : [0; 3],
            overflow_len: 0,
            start : 0,
            len : 1,
            chunk_size : chunk_size,
        }
    }


    fn get(&self, i : usize) -> Option<&Line> {
        if i >= self.len {
            None
        } else {
            let idx = (self.start + i) % self.lines.len();
            Some(&self.lines[idx])
        }
    }

    fn current_line(&mut self) -> &mut Line {
        let idx = (self.start + self.len - 1) % self.lines.len();
        &mut self.lines[idx]
    }

    fn new_line(&mut self) {

        if self.len < self.lines.len() {
            self.len += 1;
        } else {
            self.start = (self.start + 1) % self.lines.len();
            self.current_line().reset();
        }
    }

    fn push_str(&mut self, s : &str) {
        let chunk_size = self.chunk_size;
        let mut line = self.current_line();

        for c in s.chars() {
            line.push(c);

            if c == '\n' || c == '\r' || line.len() > chunk_size {
                self.new_line();
                line = self.current_line();
            }
        }
    }

    fn push_invalid_bytes(&mut self, b : &[u8]) {
        self.push_str("\u{fffd}".repeat(b.len()).as_str())
    }

    fn push(&mut self, c : char) {
        let mut b : [u8; 4] = [0; 4];
        self.push_str(
            c.encode_utf8(&mut b))
    }


    fn push_bytes(&mut self, data : &[u8]) {

        let mut buf = data;

        let overflow_len = self.overflow_len as usize;

        if overflow_len > 0 {

            let mut overflow : [u8; 4] = [0;4];

            overflow[..overflow_len].copy_from_slice(&self.overflow[..overflow_len]);


            let needs = (4 - overflow_len).min(data.len());

            overflow[overflow_len..(overflow_len+needs)]
                .copy_from_slice(&data[..needs]);


            let token = utf8_scan(&overflow[..(overflow_len+needs)]);
            //println!("{:?}", token);
            match token {
                Utf8Token::Empty => unreachable!(),
                Utf8Token::Incomplete(_) => {
                    assert!(data.len() < needs);
                    return;
                },
                Utf8Token::Normal(s) => {
                    self.push_str(s);

                    assert!(s.len() > overflow_len);

                    self.overflow_len = 0;
                    buf = &buf[needs..];
                },
                Utf8Token::Invalid(s) => {
                    self.push_invalid_bytes(s);

                    assert!(s.len() > overflow_len);

                    self.overflow_len = 0;
                    buf = &buf[needs..];
                }
            }
        }

        loop {

            let token = utf8_scan(buf);

            //println!("{:?}", token);

            match token {
                Utf8Token::Empty => break,
                Utf8Token::Incomplete(s) => {
                    assert!(buf.len() == s.len() && s.len() < 4);
                    self.overflow[..s.len()].copy_from_slice(s);
                    self.overflow_len = s.len() as u8;
                    break;
                },
                Utf8Token::Normal(s)  => {
                    self.push_str(s);
                    buf = &buf[s.len()..];
                },
                Utf8Token::Invalid(b) => {
                    self.push_invalid_bytes(b);
                    buf = &buf[b.len()..];
                },
            }
        }
    }

    fn iter(&self) -> LineBufIter {
        LineBufIter{
            inner : self,
            start : 0,
            end : self.len,
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Utf8Token<'a> {
    Empty,
    Normal(&'a str),
    Invalid(&'a [u8]),
    Incomplete(&'a [u8]),
}

fn utf8_scan<'a> (src : &'a [u8]) -> Utf8Token<'a> {
    if src.len() == 0 {
        return Utf8Token::Empty
    }

    let mut n = 0;
    let mut it = src.iter();
    let mut last = 0;

    macro_rules! next {
        () => {
            {
                n += 1;
                it.next()
            }
        };
    }

    macro_rules! must_next {
        () => {
            if let Some(b) = next!() {
                *b
            } else {
                return if last > 0 {
                    Utf8Token::Normal(unsafe { from_utf8_unchecked(&src[..last]) })
                } else {
                    Utf8Token::Incomplete(&src[..n-1])
                }
            }
        };
    }

    macro_rules! last_or_next_bound {
        () => {
            if last > 0 {
                Utf8Token::Normal(unsafe { from_utf8_unchecked(&src[..last]) })
            } else {
                while let Some(b) = next!() {
                    if is_char_boundary(*b) {
                        break;
                    }
                }
                Utf8Token::Invalid(&src[..n-1])
            }
        };
    }

    loop {

        let b = if let Some(b) = next!() {
            *b
        } else {
            return Utf8Token::Normal(unsafe { from_utf8_unchecked(&src[..n-1]) });
        };


        if b < 128 {
            last = n;
            continue;
        }

        let w = utf8_char_width(b);

        match w {
            0 => {
                // too long
                return last_or_next_bound!();
            },
            1 => unreachable!(), // handled earlier
            2 => {
                if !is_cont_byte(must_next!()) {
                    return last_or_next_bound!();
                }
            },
            3 => {
                match (b, must_next!()) {
                    (0xE0, 0xA0..=0xBF) => (),
                    (0xE1..=0xEC, 0x80..=0xBF) => (),
                    (0xED, 0x80..=0x9F) => (),
                    (0xEE..=0xEF, 0x80..=0xBF) => (),
                    _ => {
                        return last_or_next_bound!();
                    }
                }

                if !is_cont_byte(must_next!()) {
                    return last_or_next_bound!();
                }
            },
            4 => {
                match (b, must_next!()) {
                    (0xF0, 0x90..=0xBF) => (),
                    (0xF1..=0xF3, 0x80..=0xBF) => (),
                    (0xF4, 0x80..=0x8F) => (),
                    _ => {
                        return last_or_next_bound!();
                    }
                }

                if !is_cont_byte(must_next!()) {
                    return last_or_next_bound!();
                }

                if !is_cont_byte(must_next!()) {
                    return last_or_next_bound!();
                }
            }
            _ => unreachable!(),
        }
        last = n;
    }
}


fn is_char_boundary(b : u8) -> bool {
    (b as i8) >= -0x40
}

fn is_cont_byte(b : u8) -> bool {
    b >> 6 == 0b10
}

#[derive(Debug)]
enum LineToken {
    Empty,
    Wrap,
    Newline,
    Incomplete,
}

/// Tokenize the line from right to left, returns the token type, the index of the token
/// and the display width of the input between tokens
fn rline_scan(src : &str, line_width : usize) -> (LineToken, usize,  usize)  {
    if src.len() == 0 {
        return (LineToken::Empty, 0, 0);
    }

    let mut w = 0;

    for (idx, c) in src.char_indices().rev() {
        match c {
            '\n' | '\r' => return (LineToken::Newline, idx, w),
            c => {
                let cw = c.width().unwrap_or(0);

                if w + cw > line_width {
                    return (LineToken::Wrap, idx, w);
                }

                w += cw;
            },
        }
    }

    (LineToken::Incomplete, 0, w)
}

/// AnsiToken and associated functions are not a full ansi parser. The intention
/// is for them to capture only the relevant KEY PRESSES to our functionality.
#[derive(Clone, Copy, Debug)]
enum AnsiToken<'a> {
    Up,
    Down,
    Right,
    Left,

    WordLeft,
    WordRight,

    Backspace,
    Delete,

    CtrlC,
    CtrlD,

    Other(&'a [u8]) // everything else
}

struct AnsiScanner<R> {
    i : ac::StreamChunkIter<R, u16>,
}

impl<R : Read > AnsiScanner<R> {
    const PATTERNS : [(AnsiToken<'static> , &'static [u8]); 10] = [
        (AnsiToken::Up,         &[0x1b, 0x5b, 0x42]),
        (AnsiToken::Down,       &[0x1b, 0x5b, 0x43]),
        (AnsiToken::Right,      &[0x1b, 0x5b, 0x44]),
        (AnsiToken::Left,       &[0x1b, 0x5b, 0x45]),

        (AnsiToken::WordLeft,   &[0x1b, 0x62]),
        (AnsiToken::WordRight,  &[0x1b, 0x66]),

        (AnsiToken::Backspace,  &[0x08]),
        (AnsiToken::Delete,     &[0x1b, 0x5b, 0x33, 0x7e]),

        (AnsiToken::CtrlC,      &[0x03]),
        (AnsiToken::CtrlD,      &[0x04]),
    ];

    fn new(r : R) -> Self {

        let a = AhoCorasickBuilder::new()
            .dfa(true)
            .prefilter(true)
            .premultiply(true)
            .build_with_size(Self::PATTERNS.iter().map(|x| x.1))
            .unwrap();

        AnsiScanner {
            i : ac::StreamChunkIter::new(a, r)
        }
    }

    fn next<'a> (&'a mut self) -> Option<Result<AnsiToken<'a>>> {
        let next = self.i.next();

        let chunk = match next {
            None => return None,
            Some(Err(e)) => return Some(Err(e)),
            Some(Ok(x)) => x,
        };

        return match chunk {
            ac::StreamChunk::NonMatch{ bytes, .. } => {
                Some(Ok(AnsiToken::Other(bytes)))
            },
            ac::StreamChunk::Match{ mat, .. } => {
                Some(Ok(Self::PATTERNS[mat.pattern()].0))
            }
        };
    }
}


struct LineBufIter<'a> {
    inner : &'a LineBuf,
    start : usize,
    end : usize,
}

impl<'a> Iterator for LineBufIter<'a> {
    type Item = &'a Line;

    fn next(&mut self) -> Option<&'a Line> {
        if self.start >= self.end {
            None
        } else {
            let start = self.start;
            self.start += 1;
            self.inner.get(start)
        }
    }
}

impl<'a> DoubleEndedIterator for LineBufIter<'a> {
    fn next_back(&mut self) -> Option<&'a Line> {
        if self.start >= self.end {
            None
        } else {
            self.end -= 1;
            self.inner.get(self.end)
        }
    }
}



#[derive(Debug)]
struct Shell {
    o : LineBuf,
    i : String,
    can_send : bool,
    cursor : usize, // distance from the end
}

impl Shell {
    /// history_lines is a rough estimate of the number of
    /// lines we can go back
    fn new(cols : usize, rows : usize) -> Shell {
        Shell{
            o : LineBuf::new(cols, rows),
            i : String::new(),
            can_send: false,
            cursor : 0,
        }
    }

    fn push_in_str(&mut self, s : &str) {
        for c in s.chars() {
            self.push_in(c);
        }
    }

    fn push_in_slice(&mut self, b : &[u8]) {
        let s = String::from_utf8_lossy(b);
        self.push_in_str(s.as_ref());
    }

    fn push_in(&mut self, c : char) {
        if c == '\n' || c == '\r' {
            if self.i.ends_with('\\') {
                self.i.pop();
                self.i.push(c);
            } else {
                self.i.push('\n');
                self.can_send = true;
            }
        } else {
            self.i.push(c);
        }
    }

    fn get_in(&self) -> &str {
        self.i.as_str()
    }

    fn pop_in(&mut self) {
        while self.i.len() > 0 && !self.i.is_char_boundary(self.i.len() - 1) {
            self.i.truncate(self.i.len() - 1);
        }

        if self.i.len() > 0 {
            self.i.truncate(self.i.len() - 1);
        }
    }

    fn reset_in(&mut self) {
        self.i.truncate(0);
        self.can_send = false;
        self.cursor = 0;
    }

    fn poll(&self) -> Option<&str> {
        if self.can_send {
            Some(self.i.as_str())
        } else {
            None
        }
    }

    fn in_to_out(&mut self) {
        self.o.push_str(self.i.as_str())
    }

    fn push_out_slice(&mut self, b : &[u8]) {
        self.o.push_bytes(b);
    }

    fn push_out_str(&mut self, s : &str) {
        self.o.push_str(s);
    }

    fn push_out(&mut self, c : char) {
        self.o.push(c)
    }

    fn render<T : Term>(&self, term : &mut T) -> Result<()> {
        term.clear()?;

        let (cols, rows)  = term.size()?;

        if cols == 0 || rows == 0 {
            return Ok(())
        }


        // this may be confusing but it is the rendering cursor
        let mut render_row = rows - 1;


        let mut cursor_col = 0;
        let mut cursor_row = rows - 1;
        let mut cursor_cs = 0;
        let mut cursor_latch = false;

        macro_rules! next_row {
            () => {
                {
                    if render_row == 0 {
                        term.goto(cursor_col, cursor_row)?;
                        return Ok(());
                    }
                    render_row -= 1;
                    let _ = render_row;
                }
            };
        }

        for (row_idx, line) in self.i.rsplit("\n").enumerate() {
            term.write_at(0, render_row, line)?;

            if !cursor_latch {
                let it = line.chars().rev().chain(if row_idx == 0 { "\n" } else { "" }.chars());

                for (col_idx, _) in it.enumerate() {
                    if cursor_cs == self.cursor {
                        cursor_latch = true;
                        cursor_row = rows - row_idx - 1;
                        cursor_col = UnicodeWidthStr::width(
                            line.get(..(line.len() - col_idx)).unwrap());
                        break;
                    }

                    cursor_cs += 1;
                }
            }

            next_row!();
        }

        term.write_at(0, render_row, format!("{}", "-".repeat(cols)).as_str())?;
        next_row!();



        let mut incompletes : Vec<(&str, usize)> = Vec::new();
        let mut incompletes_w = 0;
        let mut first_newline = true;

        for line in self.o.iter().rev().skip(1) {
            let mut line_str = line.as_str();

            loop {
                let token = rline_scan(&line_str, cols - incompletes_w);
                //println!("{:?}", token);
                match token {
                    (LineToken::Empty, _, _) => break,
                    (LineToken::Wrap, idx, w) => {
                        term.write_at(0, render_row, &line_str[idx+1..])?;

                        let mut ww = w;
                        for (s, sw) in incompletes.iter().rev() {
                            term.write_at(ww, render_row, s)?;
                            ww += sw;
                        }

                        next_row!();
                        incompletes.truncate(0);
                        incompletes_w = 0;

                        line_str = &line_str[..=idx];
                    },
                    (LineToken::Newline, idx, w) => {
                        if first_newline {
                            line_str = &line_str[..idx];
                            first_newline = false;
                            continue;
                        }

                        term.write_at(0, render_row, &line_str[idx+1..])?;

                        let mut ww = w;
                        for (s, sw) in incompletes.iter().rev() {
                            term.write_at(ww, render_row, s)?;
                            ww += sw;
                        }

                        next_row!();
                        incompletes.truncate(0);
                        incompletes_w = 0;

                        line_str = &line_str[..idx];
                    },
                    (LineToken::Incomplete, _, w) => {
                        incompletes.push((&line_str, w));
                        incompletes_w += w;
                        break;
                    }
                }
            }
        }

        if incompletes.len() > 0 {
            let mut ww = 0;
            for (s, sw) in incompletes.iter().rev() {
                term.write_at(ww, render_row, s)?;
                ww += sw;
            }

            next_row!();
        }


        term.goto(cursor_col, cursor_row)?;
        Ok(())
    }
}




trait Term {
    // (cols, rows)
    fn size(&self) -> Result<(usize, usize)>;

    // 0-indexed
    fn write_at(&mut self, x : usize, y : usize, s : &str) -> Result<()>;
    fn goto(&mut self, x : usize, y : usize) -> Result<()>;

    fn clear(&mut self) -> Result<()>;

    fn flush(&mut self) -> Result<()>;
}

struct TestTerm{
    size : (usize, usize),
}

impl Term for TestTerm {
    // (cols, rows)
    fn size(&self) -> Result<(usize, usize)> {
        print!("size() -> {:?}\r\n", self.size);
        Ok(self.size)
    }

    // 0-indexed
    fn write_at(&mut self, x : usize, y : usize, s : &str) -> Result<()> {
        print!("write_at(x : {}, y : {}, s : {:?}) -> Ok(())\r\n", x, y, s);
        Ok(())
    }

    fn goto(&mut self, x : usize, y : usize) -> Result<()> {
        print!("goto(x : {}, y : {}) -> Ok(())\r\n", x, y);
        Ok(())
    }

    fn clear(&mut self) -> Result<()> {
        print!("clear() -> Ok(())\r\n");
        Ok(())
    }

    fn flush(&mut self) -> Result<()> {
        print!("flush() -> Ok(())\r\n");
        Ok(())
    }

}

impl<W : Write> Term for RawTerminal<W> {
    fn size(&self) -> Result<(usize, usize)> {
        match termion::terminal_size() {
            Ok((c, r)) => Ok((c as usize, r as usize)),
            Err(e) => Err(e),
        }
    }

    fn write_at(&mut self, x : usize, y : usize, s : &str) -> Result<()> {
        write!(self, "{}{}", cursor::Goto((x+1) as u16, (y+1) as u16), s)
    }


    fn goto(&mut self, x : usize, y : usize) -> Result<()> {
        write!(self, "{}", cursor::Goto((x+1) as u16, (y+1) as u16))
    }

    fn clear(&mut self) -> Result<()> {
        write!(self, "{}", clear::All)
    }

    fn flush(&mut self) -> Result<()> {
        std::io::Write::flush(self)
    }
}




fn main_loop<T, W>(stdin : Stdin,
             mut term : T,
             mut child_stdin : W,
             mut child_stdout : ChildStdout) -> Result<()>
where T : Term, W : Write {

    const TOKEN_STDIN : mio::Token = Token(0);
    const TOKEN_CHILD_STDOUT : mio::Token = Token(1);

    let mut poll  = Poll::new()?;

    let stdin_fd = stdin.as_raw_fd();
    let flag = fcntl(stdin_fd, FcntlArg::F_GETFL).unwrap();
    let ret = fcntl(stdin_fd,
                    FcntlArg::F_SETFL(
                        OFlag::from_bits(flag).unwrap() | OFlag::O_NONBLOCK
                    )
                ).unwrap();
    assert!(ret != -1);

    poll.registry().register(
        &mut SourceFd(&stdin_fd),
        TOKEN_STDIN,
        Interest::READABLE)?;

    let child_stdout_fd = child_stdout.as_raw_fd();
    let flag = fcntl(child_stdout_fd, FcntlArg::F_GETFL).unwrap();
    let ret = fcntl(child_stdout_fd,
                    FcntlArg::F_SETFL(
                        OFlag::from_bits(flag).unwrap() | OFlag::O_NONBLOCK
                    )
                ).unwrap();
    assert!(ret != -1);

    poll.registry().register(
        &mut SourceFd(&child_stdout_fd),
        TOKEN_CHILD_STDOUT,
        Interest::READABLE)?;

    let mut events = Events::with_capacity(256);

    let mut shell = {
        let (cols, rows) = term.size()?;
        Shell::new(cols, rows)
    };

    let mut ansi_scanner = AnsiScanner::new(stdin);

    let mut buf = [0; 4 * (1 << 10)];

    loop {
        poll.poll(&mut events, None)?;

        for event in events.iter() {
            // print!("event {:?}\r\n", event);
            match event.token() {
                TOKEN_STDIN => {
                    // read from stdin and push to terminal event parser
                    loop {
                        match ansi_scanner.next() {
                            None => return Ok(()), // eof
                            Some(Ok(token)) => {
                                match token {
                                    AnsiToken::Other(b) => {
                                        shell.push_in_slice(b);
                                        if let Some(s) = shell.poll() {
                                            child_stdin.write(s.as_bytes())?;
                                            shell.reset_in();
                                        }
                                    },
                                    AnsiToken::CtrlD |
                                    AnsiToken::CtrlC => return Ok(()),
                                    _ => { /* move accordingly */ },
                                }

                                shell.render(&mut term)?;
                                term.flush()?;
                            },
                            Some(Err(err)) => {
                                match err.kind() {
                                    ErrorKind::WouldBlock |
                                    ErrorKind::Interrupted => {
                                        break;
                                    }
                                    _ => return Err(err)
                                }
                            }
                        }
                    }
                },
                TOKEN_CHILD_STDOUT => {
                    // read from child stdout and push to shell out
                    match child_stdout.read(&mut buf) {
                        Ok(0) => return Ok(()),
                        Ok(n) => {
                            print!("CHILD READ{:?}\r\n", n);
                            print!("CHILD READ{:?}\r\n", &buf.as_ref()[..n]);
                            shell.push_out_slice(&buf[..n]);
                            shell.render(&mut term)?;
                            term.flush()?;
                        },
                        Err(err) => {
                            match err.kind() {
                                ErrorKind::WouldBlock |
                                ErrorKind::Interrupted => continue,
                                _ => return Err(err)
                            }
                        }

                    }
                },
                _ => {},
            }
        }
    }
}


fn main() {
    let mut child = Command::new("bash")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let _keep_raw = stdout().into_raw_mode().unwrap();

    main_loop(
        stdin(),
        stdout().into_raw_mode().unwrap(),
        // TestTerm{ size : (80, 24) },
        child.stdin.take().unwrap(),
        child.stdout.take().unwrap(),
        ).unwrap();
}

/*
fn main() {
    let stdin = stdin();
    //let mut term = TestTerm{ size  : (100, 100), };
    let mut term = stdout().into_raw_mode().unwrap();
    let mut shell = Shell::new(term.size().unwrap().0, 4 * 1 << 10);

    for key in stdin.keys() {
        match key.unwrap() {
            Key::Char(c) => shell.push_in(c),
            Key::Backspace | Key::Ctrl('h') => shell.pop_in(),
            Key::Ctrl('d') => break,
            x => {
                term.write_at(0, 0, format!("{:?}", x).as_str()).unwrap();
                Term::flush(&mut term).unwrap();
                continue;
            },
            //_ => Ok(()),
        };

        if let Some(_) = shell.poll() {
            shell.in_to_out();
            shell.reset_in();
        }

        shell.render(&mut term).unwrap();
        Term::flush(&mut term).unwrap();
    }

    println!("{:?}", shell);
}
*/
