#pragma once

class IDllComponent
{
public:
    IDllComponent();
    virtual ~IDllComponent();

    virtual bool isEnable() const = 0;
    virtual bool setup() = 0;
    virtual bool hasSetuped() const = 0;
    virtual void cleanup() = 0;

    virtual IDllComponent* getNext()
    {
        return m_next;
    }

    static IDllComponent* getHeader();

private:
    IDllComponent   *m_next;
};