
#[path = "../tst.rs"]
mod tst;
use crate::tst::*;

use std::{
    io::{
        //BufRead,
        stdin,
    },
};

fn main() {


    let mut tst = Tst{ nodes : Vec::new(), has_empty : false };
    tst.add_pat(b"abcde", ());
    tst.add_pat(b"bce", ());


    let mut scanner = TstScanner::new(tst);
    //let mut scanner = TstScannerValue::new(tst);

    let stdin = stdin();
    let mut s = String::new();

    while let Ok(_) = stdin.read_line(&mut s) {
        if s.len() == 0 {
            // EOF
            break;
        }
        s.truncate(s.len() - 1);
        println!("{:?}", s);

        let x = scanner.scan(s.as_ref());
        println!("{:?}", x);
        s.truncate(0);
        /*
        match x.0 {
            TstToken::MatchObj(x) |
            TstToken::MatchArg(x) => {
                println!("{:?}", x);
            },
            x => println!("{:?}", x),
        }
        */
    }

}
