import gleam/io

const is_enabled = False

pub fn log(x: a) -> a {
  case is_enabled {
    True -> io.debug(x)
    False -> x
  }
}
