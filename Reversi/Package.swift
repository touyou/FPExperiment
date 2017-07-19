// swift-tools-version:3.1

import PackageDescription

let package = Package(
    name: "Reversi",
    dependencies: [
        .Package(url: "git@github.com:kylef/Commander.git",
                 majorVersion: 0),
        ]
)
