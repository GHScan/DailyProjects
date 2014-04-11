# version
cmake_minimum_required(VERSION 2.8.0)

# options
option(OPT_BUILD_64BIT "build 64 bit object file" "0")

# config
# set(CMAKE_VERBOSE_MAKEFILE ON)

# compiler options
if (UNIX)
    set(compilerOptions "-std=c99 -Wall")
    if (OPT_BUILD_64BIT)
        set(compilerOptions "${compilerOptions} -m64")
    else()
        set(compilerOptions "${compilerOptions} -m32")
    endif()
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} ${compilerOptions}")
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} ${compilerOptions}")
elseif (MSVC)
    set(compilerOptions "/TC /W4 /analyze /DWIN32 /D_CONSOLE")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} ${compilerOptions} /MDd")
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} ${compilerOptions} /MD")

    if (OPT_BUILD_64BIT)
        if (NOT CMAKE_CL_64)
            message(FATAL_ERROR "Should choose 64bit generator")
        endif()
    else()
        if(CMAKE_CL_64)
            message(FATAL_ERROR "Should choose 32bit generator")
        endif()
    endif()
endif()

# help functions
function(generateWorkspace)
    get_directory_property(headerDirs INCLUDE_DIRECTORIES)
    list(APPEND headerDirs ${PROJECT_SOURCE_DIR})

    set(vimrcLines "cd ${PROJECT_SOURCE_DIR}\n")
    foreach(dir ${headerDirs})
        list(APPEND vimrcLines "set path+=" ${dir} "\n")
    endforeach()
    file(WRITE "${PROJECT_BINARY_DIR}/workspace_vimrc" ${vimrcLines})

    set(clangCompleteLines ${CMAKE_C_FLAGS_DEBUG})
    foreach(dir ${headerDirs})
        list(APPEND clangCompleteLines "\n-I" ${dir})
    endforeach()
    file(WRITE "${PROJECT_BINARY_DIR}/.clang_complete" ${clangCompleteLines})
endfunction()

# subdirectory
add_subdirectory(3rdLib)
add_subdirectory(Byzantine)
add_subdirectory(App)
