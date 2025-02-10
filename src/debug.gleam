import gleam/io

const is_enabled = True

pub fn log(x: a) -> a {
  case is_enabled {
    True -> io.debug(x)
    False -> x
  }
}

pub fn enabled() -> Bool {
  is_enabled
}
