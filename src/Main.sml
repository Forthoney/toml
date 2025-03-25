structure toml:
sig
  val greeting: string
end =
struct
  val hello = "Hello, "
  val world = "World!"
  val greeting = hello ^ world ^ "\n"
end
val () = print toml.greeting
