project(Byzantine C)

# generate config
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(by_DEBUG "1")
endif()

if (CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(by_64BIT "1")
endif()

if (MSVC)
    set(by_MSVC "1")
elseif (CMAKE_COMPILER_IS_GNUCC)
    set(by_GCC "1")
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(by_LINUX "1")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set(by_WINDOWS "1")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set(by_DARWIN "1")
endif()

configure_file(
    "${PROJECT_SOURCE_DIR}/Config.h.in"
    "${PROJECT_BINARY_DIR}/Config.h")

# target
include_directories(${PROJECT_BINARY_DIR})

file(GLOB srcs "${PROJECT_SOURCE_DIR}/*.c")
add_library(Byzantine SHARED ${srcs})

# workspace
generateWorkspace()

# install
file(GLOB headers "${PROJECT_SOURCE_DIR}/*.h")
file(GLOB internalHeaders "${PROJECT_SOURCE_DIR}/Internal_*.h")
foreach(h ${internalHeaders})
    list(REMOVE_ITEM headers ${h})
endforeach()

file(GLOB dyheaders "${PROJECT_BINARY_DIR}/*.h")

set_target_properties(Byzantine 
    PROPERTIES PUBLIC_HEADER "${headers};${dyheaders}")

install(TARGETS Byzantine
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include
    )
