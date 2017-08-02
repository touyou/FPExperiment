import Commander
import Foundation

let main = command(
    Option("H", "https://localhost", description: "host name of a server"),
    Option("p", 8080, description: "port number of a server"),
    Option("n", "Player1", description: "player name")
) { host, port, name in
}

main.run()
