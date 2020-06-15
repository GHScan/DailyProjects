cmake_minimum_required(VERSION 2.4)

project(Codec)

file(GLOB incs "src/*.h")
file(GLOB srcs "src/*.cpp")

add_executable(codec ${srcs} ${incs})
