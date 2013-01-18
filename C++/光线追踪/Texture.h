// vim: fileencoding=gbk

#ifndef TEXTURE_H
#define TEXTURE_H

#include <map>
#include <string>
#include <vector>

class Texture
{
public:
    Texture();
    Texture(const char *fname);
    ~Texture();

    bool load(const char *fname);
    bool save(const char *fname) const;
    void clear();
    bool valid() const;
    bool hasMipmap() const;
    int maxMipmapLevel() const;

    int width(int level) const;
    int height(int level) const;
    const float* data(int level) const;
private:
    void genMipmap();

private:
    std::vector<float*> m_bufs;
    int m_w, m_h;
};

class TextureManager
{
public:
    ~TextureManager();

    static TextureManager* instance();

    const Texture* find(const char *fname);
    void clear();

private:
    typedef std::map<std::string, Texture*> TextureMap;
private:
    TextureMap m_map;
};

#endif // #ifndef TEXTURE_H
