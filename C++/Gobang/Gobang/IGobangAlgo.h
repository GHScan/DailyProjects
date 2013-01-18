#pragma once

#include <map>
#include <string>

struct IGobangAlgo
{
    virtual ~IGobangAlgo() = 0 {}

    virtual void init(int sz) = 0;
    virtual void reset() = 0;
    virtual void peerGo(int x, int y) = 0;
    virtual bool thisGo(int &x, int &y) = 0;    
    virtual bool canPeerWin() const = 0;
    virtual bool canWin() const = 0;
};

typedef IGobangAlgo* (*algoCreator)();

std::map<std::string, algoCreator>& getAlgoMap();