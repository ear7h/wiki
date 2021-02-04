// broken tst based state machine implementation

use std::{
    borrow::Borrow,
    cmp::Ordering,
};



#[derive(Debug)]
pub(crate) struct Node<K, V> {
    /// TODO: make the index generic
    left : usize,
    right : usize,
    down : usize,
    /// data value
    k : K,
    /// is match
    v : Option<V>,
}

impl<K, V> Node<K, V> {
    fn new(k : K) -> Node<K, V> {
        Node {
            left : 0,
            right : 0,
            down : 0,
            k : k,
            v : None,
        }
    }

    fn new_match(k : K, v : V) -> Node <K, V>  {
        Node {
            left : 0,
            right : 0,
            down : 0,
            k : k,
            v : Some(v),
        }
    }

    fn is_match(&self) -> bool {
        self.v.is_some()
    }
}

/// TstScanner is capable of scanning chunked [K] maintaining state and an
/// internal buffer of the match.
///
/// This should only be used if you'd like to keep track of all the inputs
/// into the scanner. If you only want the Vs for matches then use TstScannerValue.
pub struct TstScanner<K, V> {
    tst : Tst<K, V>,
    state : usize,
    buf : Vec<K>,
    clear_buf : bool,
    has_match : Option<usize>,
}

/// A token given back from TstScanner::scan()
///
/// The fundamentally different tokens are Match, NonMatch, and Incomplete:
/// * Match means the scanner found a matching string, it is returned in
///     the token
/// * NonMatch means the scanner has completely ruled out the contained
///     string.  Multiple NonMatches may be returned in a row; in particular,
///     across calls to scan()
/// * Incomplete means the scanner consumed all the bytes and is in the
///     process of matching some pattern; scanning more data is required
///     to get a match/non-match
///
/// Also, for each of the string-returning tokens (Match and NonMatch), there
/// are also variants for the lifetimes of the string. This is to minimize the
/// amount of copying from a source slice to buffer.
#[derive(Debug)]
pub enum TstToken<'obj, 'arg, K, V> {
    MatchObj(&'obj [K], V),
    MatchArg(&'arg [K], V),

    NonMatchObj(&'obj  [K]),
    NonMatchArg(&'arg [K]),

    Incomplete,
}

impl<K : Clone + Ord, V> TstScanner<K, V> {

    pub fn new(tst : Tst<K, V>) -> Self {
        TstScanner{
            tst,
            state : 0,
            buf : Vec::new(),
            clear_buf : false,
            has_match : None,
        }
    }

    /// NOTE: due to the halting problem, this method will never return a match
    /// to the empty string (even if the underlying tst might).
    ///
    /// NOTE: in a streaming context, the TST can be used to continue matching a longer
    /// prefix after matching a shorter prefix, but this behavior is uncessarily close to
    /// (or a poor attempt at) an Aho-Corasick atomaton. So, this only matches the shortest
    /// prefix. We could also return the longest prefix, the main problem overlapping results...
    ///
    /// NOTE: one assumption made in this code is that running the tst on a K is more expensive than
    /// Cloning the K. This may not be always true across the board (who knows how someone
    /// implemented clone()), but the most common case of string keys this is true. So, in the
    /// cases where the buffer contains a partial match and continuing the match on
    /// the arg fails we chose to copy the failed match from the arg into the buffer
    /// and returning a NonMatchObj. The other option would be returning the NonMatchObj
    /// without adding the arg match, and having the caller call scan again with the same
    /// arg and state; but since the assumption is that cloning is faster than the tst
    /// this is not done.
    pub fn scan<'obj, 'arg>(&'obj mut self, s : &'arg [K]) -> (TstToken<'obj, 'arg, K, &'obj V>, &'arg [K]) {
        println!("scan state {}", self. state);

        if let Some(idx) = self.has_match {
            // we have a full match in the buffer, give it back
            self.has_match = None;
            self.clear_buf = true;
            return (
                TstToken::MatchObj(
                    self.buf.as_slice(),
                    self.tst.node(idx).v.as_ref().unwrap()
                ),
                s
            );
        }

        if self.clear_buf {
            self.buf.truncate(0);
            self.clear_buf = false;
        }

        if s.len() == 0 {
            return (TstToken::Incomplete, &s[0..0]);
        }

        println!("state : {:?}, buf.len() {:?}", self.state, self.buf.len());
        assert!((self.state == 0)  == (self.buf.len() == 0));

        // branch on whether we have data in the buffer, this tells us
        // whether we should return an 'obj or 'arg token
        if self.buf.len() == 0 {
            // return 'arg tokens


            // the index of the last byte that transitioned from the 0 state
            // aka the first byte of a match
            let mut last_from_zero = 0;

            for (idx, c) in s.iter().enumerate() {
                let mat = self.tst.next_state(&mut self.state, c);
                if mat && last_from_zero == 0 {
                    // match started at the beginning
                    let matched_node = self.tst.node(self.state).v.as_ref().unwrap();
                    self.state = 0;

                    return (
                        TstToken::MatchArg(
                            &s[..=idx],
                            matched_node,
                        ),
                        &s[idx+1..]
                    );
                } else if mat {
                    // match did not start at the beginning so
                    // return non-match up to the start of the match
                    // and put the match in the buffer to retrieve it asap in
                    // the next call
                    self.buf.extend_from_slice(&s[last_from_zero..=idx]);
                    self.has_match = Some(self.state);
                    self.state = 0;

                    return (TstToken::NonMatchArg(&s[..last_from_zero]), &s[idx+1..]);
                }


                if self.state == 0 {
                    if last_from_zero != idx {
                        // if the match went from non-zero to zero we need to restart
                        // the the search with the current char. It is not necessary
                        // to restart the match if the previous state was zero
                        // because restarting means re-trying the current char match
                        // from zero, meaning it has already been done
                        //
                        // if a match happens here it is only a single char
                        let mat = self.tst.next_state(&mut self.state, c);
                        if mat {
                            self.buf.extend_from_slice(&s[idx..=idx]);
                            self.has_match = Some(self.state);
                            self.state = 0;

                            return (TstToken::NonMatchArg(&s[..idx]), &s[idx..]);
                        }
                        last_from_zero = idx;
                    } else {
                        last_from_zero = idx + 1;
                    }
                }
            }


            if last_from_zero == 0 {
                // we read all bytes and they all contributed to the match
                self.buf.extend_from_slice(s);
                return (TstToken::Incomplete, &s[0..0])
            }

            // we read all bytes and some might have conntributed to the match
            // add them to the buffer, then return the ones that defnitely didn't match
            self.buf.extend_from_slice(&s[last_from_zero..]);
            return (TstToken::NonMatchArg(&s[..last_from_zero]) , &s[0..0]);
        } else {
            // return 'obj tokens
            // the previous call returned TstToken::Incomplete
            for (idx, c) in s.iter().enumerate() {
                let mat = self.tst.next_state(&mut self.state, c);
                if mat {
                    // got a match, add it to the obj buffer and return
                    // the buf results
                    let matched_node = self.tst.node(self.state).v.as_ref().unwrap();
                    self.buf.extend_from_slice(&s[..=idx]);
                    self.state = 0;
                    self.clear_buf = true;

                    return (
                        TstToken::MatchObj(
                            self.buf.as_slice(),
                            matched_node,
                        ),
                        &s[idx+1..]
                    );
                }

                if self.state == 0 {
                    // didn't match and we're back to the initial state so
                    // we can get rid of the buffered bytes as a non-match
                    //
                    // since the state is at 0 we need to restart the match
                    // with the current character. In this case it is easier
                    // to have it done in the next scan call so we don't include
                    // s[idx] in the buffer that gets returned (it is the first
                    // char for the next scan call with state still at 0)
                    self.buf.extend_from_slice(&s[..idx]);
                    self.clear_buf = true;

                    return (TstToken::NonMatchObj(self.buf.as_slice()), &s[idx..]);
                }
            }

            // consumed all the arg bytes without a match or being able to dump the
            // obj buffer
            self.buf.extend_from_slice(s);
            return (TstToken::Incomplete, &s[0..0]);
        }
    }
}

/// Like TstScanner but it does not keep track of the any input
pub struct TstScannerValue<K, V> {
    tst : Tst<K, V>,
    state : usize,
}


impl<K : Clone + Ord, V> TstScannerValue<K, V> {
    pub fn new(tst : Tst<K, V>) -> Self {
        TstScannerValue {
            tst: tst,
            state : 0,
        }
    }

    pub fn scan<'a>(&mut self, s : &'a [K]) -> (Option<&V>, &'a [K]) {
        for (idx, c) in s.iter().enumerate() {
            println!("state: {}", self.state);
            let mat = self.tst.next_state(&mut self.state, c);
            if mat {
                let ret = self.tst.node(self.state).v.as_ref().unwrap();
                self.state = 0;
                return (Some(ret), &s[idx+1..]);
            }

            if self.state == 0 {
                if self.tst.next_state(&mut self.state, c) {
                    let ret = self.tst.node(self.state).v.as_ref().unwrap();
                    self.state = 0;
                    return (Some(ret), &s[idx+1..]);
                }
            }
        }

        return (None, &s[0..0]);
    }
}

#[derive(Debug)]
pub struct Tst<K, V> {
    pub(crate) nodes : Vec<Node<K, V>>,
    pub(crate) has_empty : bool,
}

impl<K : Clone + Ord, V> Tst<K, V> {
    pub fn new<I, Ks>(pats : I) -> Tst<K, V>
    where
        Ks : AsRef<[K]>,
        I : Iterator<Item = (Ks, V)>
    {
        let mut ret = Tst{
            nodes: Vec::new(),
            has_empty : false,
        };

        for (ks, v)  in pats {
            ret.add_pat(ks.as_ref(), v)
        }

        ret
    }


    #[inline]
    fn new_node(&mut self, k : K) -> usize {
        let ret = self.nodes.len();
        self.nodes.push(Node::new(k));

        ret
    }

    #[inline(always)]
    fn node(&self, i : usize) -> &Node<K, V> {
        &self.nodes[i]
    }

    #[inline(always)]
    fn node_mut(&mut self, i : usize) -> &mut Node<K, V> {
        &mut self.nodes[i]
    }

    #[inline(always)]
    fn next_state<Kb>(&self, ret_idx : &mut usize, c : &Kb) -> bool
    where
        K : Borrow<Kb>,
        Kb : Ord + ?Sized {

        let mut idx = *ret_idx;

        loop {
            let node = self.node(idx);

            idx = match node.k.borrow().cmp(c) {
                Ordering::Equal => {
                    *ret_idx = node.down;
                    return node.is_match();
                }
                Ordering::Less => node.left,
                Ordering::Greater => node.right,
            };

            if idx == 0 {
                *ret_idx = 0;
                return false
            }
        }
    }

    /// Finds the provided string. If the string is not found the last node and
    /// the remaining string is returned
    pub(crate) fn find_insert_point<'a>(&self, mut s : &'a [K]) -> (usize, &'a [K]) {
        if s.len() == 0 || self.nodes.len() == 0 {
            return (0, s)
        }

        let mut node_idx = 0;
        let mut node = self.node(node_idx);
        let mut c = &s[0];

        macro_rules! set_node_idx {
            ($e:expr) => {
                if $e == 0 {
                    return (node_idx, s);
                } else {
                    node_idx = $e;
                    node = self.node(node_idx);
                }
            };
        }


        loop {

            match node.k.cmp(c) {
                Ordering::Equal => {
                    if s.len() == 1 {
                        return (node_idx, s);
                    }

                    set_node_idx!(node.down);
                    s = &s[1..];
                    c = &s[0];
                },
                Ordering::Less => set_node_idx!(node.left),
                Ordering::Greater => set_node_idx!(node.right),
            }
        }
    }

    pub(crate) fn add_pat(&mut self, s : &[K], v : V) {
        if s.len() == 0 {
            self.has_empty = true;
            return;
        }

        let (idx, rest) = self.find_insert_point(s);
        /*
        if self.nodes.len() > 0 && self.node(idx).m {
            return;
        }
        */

        let next_idx = self.new_node(rest[0].clone());
        let node = self.node_mut(idx);

        // add first new
        let dir = match node.k.cmp(&rest[0]) {
            Ordering::Equal => &mut node.down,
            Ordering::Less => &mut node.left,
            Ordering::Greater => &mut node.right,
        };

        assert!(*dir == 0);
        *dir = next_idx;

        let mut idx = next_idx;

        for c in &rest[1..] {
            let next_idx = self.new_node(c.clone());
            self.node_mut(idx).down = next_idx;
            idx = next_idx;
        }

        self.node_mut(idx).v = Some(v);
    }
}
