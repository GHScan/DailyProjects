project(UnitTests C)

# target
include_directories(${Byzantine_SOURCE_DIR})
include_directories(${Byzantine_BINARY_DIR})

file(GLOB srcs "${PROJECT_SOURCE_DIR}/*.c")
add_executable(UnitTests ${srcs})

target_link_libraries(UnitTests Byzantine)

# workspace
generateWorkspace()
