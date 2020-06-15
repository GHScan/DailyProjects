cmake_minimum_required(VERSION 3.9)


set(CMAKE_CXX_STANDARD 14)



project(CPUSoftwareRenderer)


if (MSVC)
    set(OPT_OPTIONS "/fp:fast /arch:AVX2 /Oi /Ox /Ot")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${OPT_OPTIONS}")
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} ${OPT_OPTIONS}")
else()
    set(OPT_OPTIONS "-mfma -mavx2")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${OPT_OPTIONS}")
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} ${OPT_OPTIONS}")
endif()


set(DEP_INC "${PROJECT_SOURCE_DIR}/3rdparty")
if (MSVC)
    set(DEP_LIB "winmm.lib")
else()
    set(DEP_LIB "")
endif()


# SDL
set(SDL2_ROOT "" CACHE PATH "location of SDL2 root")
if (NOT SDL2_ROOT)
    message(FATAL_ERROR "cannot find SDL2")
endif()
list(APPEND DEP_INC ${SDL2_ROOT}/include)
if (MSVC)
    list(APPEND DEP_LIB ${SDL2_ROOT}/lib/SDL2.lib ${SDL2_ROOT}/lib/SDL2-static.lib)
elseif(APPLE)
    list(APPEND DEP_LIB ${SDL2_ROOT}/lib/libSDL2.dylib)
endif()
# assimp
set(assimp_ROOT "" CACHE PATH "location of assimp root")
if (NOT assimp_ROOT)
    message(FATAL_ERROR "cannot find assimp")
endif()
list(APPEND DEP_INC ${assimp_ROOT}/include)
if (MSVC)
    list(APPEND DEP_LIB debug ${assimp_ROOT}/lib/zlibstaticd.lib debug ${assimp_ROOT}/lib/IrrXMLd.lib debug ${assimp_ROOT}/lib/assimp-vc141-mtd.lib)
    list(APPEND DEP_LIB optimized ${assimp_ROOT}/lib/zlibstatic.lib optimized ${assimp_ROOT}/lib/IrrXML.lib optimized ${assimp_ROOT}/lib/assimp-vc141-mt.lib)
elseif(APPLE)
    list(APPEND DEP_LIB ${assimp_ROOT}/lib/libassimp.dylib)
endif()


file(GLOB incs "src/*.h")
file(GLOB srcs "src/*.cpp")
add_executable(CPUSoftwareRenderer WIN32 ${srcs} ${incs})
target_include_directories(CPUSoftwareRenderer PRIVATE ${DEP_INC})
target_link_libraries(CPUSoftwareRenderer PRIVATE ${DEP_LIB})
