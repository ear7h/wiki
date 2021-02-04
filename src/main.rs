

use {
    std::io::prelude::*,
};


type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
enum Error {
    Unimplemented,
    UnknownCommand(String),
    Parse(String),
    InvalidArgs(String),
    VertexNotFound(String),
    Io(std::io::Error),
}


impl From<std::num::ParseIntError> for Error {
    fn from(err : std::num::ParseIntError) -> Error {
        Error::Parse(format!("{}", err))
    }
}

impl From<std::io::Error> for Error {
    fn from(err : std::io::Error) -> Error {
        Error::Io(err)
    }
}

struct Vertex {
    name : String,
    adjacent : Vec<usize>,
}

struct Graph {
    vertices : Vec<Vertex>,
}

impl Graph {
    fn load<R : BufRead>(mut r : R) -> Result<Self> {
        let mut s : String = Default::default();
        let mut ret = Graph { vertices : Vec::new() };
        let mut line = 0;

        while s.len() == 0 {
            line += 1;
            if 0 == r.read_line(&mut s)? {
                return Ok(ret)
            }
        }
        s.truncate(s.len() - 1);

        loop {
            println!("new vertex |{}|", s);
            let name = s.clone();

            line += 1;
            s.clear();
            if 0 == r.read_line(&mut s)? {
                return Err(
                    Error::Parse(
                        format!("line {}: unexpected eof", line)))
            }
            s.truncate(s.len() - 1);

            let adjacent = s.split(" ")
                            .filter(|s| s.len() > 0)
                            .map(|s| {
                                s.parse()
                                 .map_err(|err| Error::Parse(
                                    format!("line {}: {:?}", line, err)))
                            })
                            .collect::<std::result::Result<Vec<_>, _>>()?;

            ret.vertices.push(Vertex{ name, adjacent });

            line += 1;
            s.clear();
            if 0 == r.read_line(&mut s)? {
                return Ok(ret)
            }
            s.truncate(s.len() - 1);
            while s.len() == 0 {
                line += 1;
                if 0 == r.read_line(&mut s)? {
                    return Ok(ret)
                }
                s.truncate(s.len() - 1);
            }
        }
    }

    fn save<W : Write>(&self, mut w : W) -> Result<()> {
        for vert in self.vertices.iter() {
            write!(w, "{}\n", vert.name)?;
            let adj = vert.adjacent
                          .iter()
                          .map(ToString::to_string)
                          .collect::<Vec<_>>()
                          .join(" ");
            write!(w, "{}\n\n", adj)?;
        }
        Ok(())
    }

    fn add_vertex(&mut self, name : String) -> usize {
        self.vertices.push(Vertex{
            name : name,
            adjacent: Vec::new(),
        });

        self.vertices.len() - 1
    }

    fn add_edge(&mut self, from : usize, to : usize) {
        self.get_vertex_mut(from).map(|v| v.adjacent.push(to));
    }

    fn get_vertex_mut(&mut self, id : usize) -> Option<&mut Vertex> {
        self.vertices.get_mut(id)
    }

    fn get_vertex(&self, id : usize) -> Option<&Vertex> {
        self.vertices.get(id)
    }

    fn get_vertices_by_name<'a, 'b>(&'a self, name : &'b str) ->
        Vec<(usize, &'a Vertex)> {

        self.vertices.iter().enumerate().filter(|v| v.1.name == name).collect()
    }

    fn format_vertex(&self, idx : usize) -> String {
        let vert = &self.vertices[idx];
        let mut ret = vert.name.clone();
        for adj in vert.adjacent.iter() {
            ret.push_str(format!("\n- {}", self.vertices[*adj].name).as_str());
        }

        return ret
    }
}

struct GraphShell {
    graph : Graph,
    history : Vec<usize>,
    history_idx : usize,
    display : Vec<(usize, Vec<usize>)>,
}

impl GraphShell {
    fn get_relative(&self, name : &str) -> Option<usize>{
        self.graph
            .get_vertex(self.current_vertex())
            .and_then(|v| {
                v.adjacent
                    .iter()
                    .filter(|id| {
                        self.graph
                            .get_vertex(**id)
                            .map(|v| v.name.as_str() == name)
                            .unwrap_or(false)
                    })
                    .nth(0)
            })
            .map(|id| *id)
    }

    fn current_vertex(&self) -> usize {
        self.history[self.history_idx]
    }

    fn change_vertex(&mut self, id : usize) {
        // this isn't exactly a "history"
        // but a buffer for the forwarad and backward
        // history navigation.
        //
        // moving to a new directory clears any forward
        // history created from going back
        //
        // if the user is at the most forward point in history:
        //      ih: idx = len - 1
        // let idx' = idx + 1
        // => truncate(idx') => no op
        // => push() => idx' = len - 1 + 1
        //           => idx' = len - 1 (from ih)
        //  => idx' is the new idx at the most forward point in history
        // (base case is idx = 0 and len = 1)
        //
        // if the user is not at the most forward point in
        // history:
        //      idx < len - 1
        // idx' = idx + 1
        // => truncate(idx') => len' = idx'
        // => push() => idx' = len + 1
        //           => idx' - 1 = len'
        //=> idx' is the new idx at the most forward point in history

        assert!(self.history.len() > 0);
        self.history_idx += 1;
        self.history.truncate(self.history_idx);
        self.history.push(id);
    }

    fn exec(&mut self, cmd : Command) -> Result<()> {
        match cmd {
            Command::Vertex(name) => {
                let new_id = self.graph.add_vertex(name);
                self.graph.add_edge(self.current_vertex(), new_id);
                Ok(())
            },
            Command::List(name_opt) => {
                if let Some(name) = name_opt {
                    self.get_relative(name.as_str())
                        .and_then(|id| {
                            self.graph.get_vertex(id)
                        })
                } else {
                    self.graph.get_vertex(self.current_vertex())
                }.map(|v| {
                    println!("({}) {}", self.current_vertex(), v.name);
                    v.adjacent
                        .iter()
                        .for_each(|id| {
                            self.graph
                                .get_vertex(*id)
                                .map(|v| {
                                    println!("\t({}) {}", id, v.name)
                                });
                        })
                });

                Ok(())
            },
            Command::ChangeVertex(to) => {
                if let Some(id) = self.get_relative(to.as_str()) {
                    self.change_vertex(id);
                    Ok(())
                } else {
                    Err(Error::VertexNotFound(to))
                }
            },
            Command::HistoryBack(n) => {
                if self.history_idx > 0 {
                    self.history_idx -= 1;
                }
                Ok(())
            },
            Command::HistoryForward(n) => {
                if self.history_idx + 1 < self.history.len() {
                    self.history_idx += 1;
                }
                Ok(())

            },
            Command::Nop => Ok(()),
            _ => unimplemented!(),
        }

    }
}



#[derive(Debug)]
enum Command {
    Nop,
    Vertex(String),
    Edge((String, String)),
    ChangeVertex(String),
    HistoryBack(usize),
    HistoryForward(usize),
    List(Option<String>),
}

impl Command {
    fn help(s : &str) -> &'static str {
        match s {
            "print" => "print - print the current vertex",
            "vertex" => "vertex name - add a vertex with the name",
            "edge" => "edge from to - add an edge",
            "cv" => "change vertex",
            _ => "not a command",
        }
    }

}


impl std::str::FromStr for Command {
    type Err = Error;

    fn from_str(s : &str) -> Result<Self> {
        let mut args = s.split_whitespace().collect::<Vec<_>>();

        match args.as_slice() {
            [] => Ok(Command::Nop),
            ["ls"] => {
                Ok(Command::List(None))
            },
            ["ls", name] => {
                Ok(Command::List(Some(name.to_string())))
            },
            ["vertex", name] => { // create a vertex
                Ok(Command::Vertex(name.to_string()))
            },
            ["edge", from, to] => { // create an edge
                Ok(Command::Edge((from.to_string(), to.to_string())))
            },
            ["hb"] => {
                Ok(Command::HistoryBack(1))
            },
            ["hb", n] => {
                Ok(Command::HistoryBack(n.parse()?))
            },
            ["hf"] => {
                Ok(Command::HistoryForward(1))
            },
            ["hf", n] => {
                Ok(Command::HistoryForward(n.parse()?))
            },
            ["cv", to] => {
                Ok(Command::ChangeVertex(to.to_string()))
            },
            x => Err(Error::UnknownCommand(x.join(" ").to_string())),
        }
    }
}

fn main() {
    let mut file = std::fs::OpenOptions::new().read(true)
                                          .open("me.graphdb").unwrap();

    let mut g = Graph::load(std::io::BufReader::new(&mut file)).unwrap();

    let mut gs = GraphShell{
        graph: g,
        history: vec![0],
        history_idx: 0,
        display: Vec::new(),
    };


    for line in std::io::BufReader::new(std::io::stdin()).lines() {

        line
            .map_err(Into::into)
            .and_then(|s| s.parse::<Command>())
            .and_then(|cmd| gs.exec(cmd))
            .unwrap_or_else(|err| println!("error: {:?}", err));

        //println!("{:?}", gs)

    }


    let mut file = std::fs::OpenOptions::new().write(true)
                                              .truncate(true)
                                              .create(true)
                                              .open("me.graphdb").unwrap();
    gs.graph.save(&mut file).unwrap();

}
