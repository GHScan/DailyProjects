#ifndef RENDERSTATEENUMS_H
#define RENDERSTATEENUMS_H

enum E_ZBufferType
{
    EZBT_null = 0,
    EZBT_zbuf,
    EZBT_1zbuf,
};
enum E_ShadeMode
{
    ESM_frame = 1,
    ESM_const,
    ESM_flat,
    ESM_gouraud,
    ESM_phong,
};
enum E_TextureFilterType
{
    ETFT_null = 0,
    ETFT_point,
    ETFT_bilinear,
    ETFT_trilinear,
    ETFT_mipmapDbg,
};
enum E_TextureAddressingMode
{
    ETAM_clamp,
    ETAM_repeat,
    ETAM_mirror,
};
enum E_CullFace
{
    ECF_null = 0,
    ECF_back,
    ECF_front,
};
enum E_ZSortType
{
    EZST_null,
    EZST_near2Far,
    EZST_far2Near,
};
enum E_SpecularLight
{
    ESL_disable,
    ESL_enbale,
};

const char* enum2Str(E_ZBufferType t);
const char* enum2Str(E_ShadeMode t);
const char* enum2Str(E_TextureFilterType t);
const char* enum2Str(E_TextureAddressingMode t);
const char* enum2Str(E_CullFace t);
const char* enum2Str(E_ZSortType t);
const char* enum2Str(E_SpecularLight t);

#endif // #ifndef RENDERSTATEENUMS_H
