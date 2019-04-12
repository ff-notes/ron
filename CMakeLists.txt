cmake_minimum_required(VERSION 3.0)
include(ExternalProject)

set(RON_CXX_INSTALL_DIR "${CMAKE_SOURCE_DIR}/swarm/_artifacts/")
ExternalProject_Add(
    ron-cxx
    # GIT_REPOSITORY /Users/cblp/dev/ron-cxx
    GIT_REPOSITORY https://github.com/gritzko/ron-cxx.git
    GIT_TAG master
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${RON_CXX_INSTALL_DIR}
)
