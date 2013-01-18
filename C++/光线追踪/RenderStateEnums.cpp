#include "pch.h"

#include <cassert>

#include <string>

#include "RenderStateEnums.h"

const char* enum2Str(E_ZBufferType t)
{
    switch (t) {
        case EZBT_null: return "null";
        case EZBT_zbuf: return "zbuf";
        case EZBT_1zbuf: return "invzbuf";
        default: break;
    }
    return "";
}
const char* enum2Str(E_ShadeMode t)
{
    switch (t) {
        case ESM_frame: return "frame";
        case ESM_const: return "const";
        case ESM_flat: return "flat";
        case ESM_gouraud: return "gouraud";
        case ESM_phong: return "phong";
        default: break;
    }
    return "";
}
const char* enum2Str(E_TextureFilterType t)
{
    switch (t) {
        case ETFT_null: return "null";
        case ETFT_point: return "point";
        case ETFT_bilinear: return "bilinear";
        case ETFT_trilinear: return "trilinear";
        case ETFT_mipmapDbg: return "mipmapDbg";
        default: break;
    }
    return "";
}
const char* enum2Str(E_TextureAddressingMode t)
{
    switch (t) {
        case ETAM_clamp: return "clamp";
        case ETAM_repeat: return "repeat";
        case ETAM_mirror: return "mirror";
        default: break;
    }
    return "";
}
const char* enum2Str(E_CullFace t)
{
    switch (t) {
        case ECF_null: return "null";
        case ECF_back: return "back";
        case ECF_front: return "front";
        default: break;
    }
    return "";
}

const char* enum2Str(E_ZSortType t)
{
    switch (t) {
        case EZST_null: return "null";
        case EZST_near2Far: return "near2Far";
        case EZST_far2Near: return "far2Near";
        default: break;
    }
    return "";
}

const char* enum2Str(E_SpecularLight t)
{
    switch (t) {
        case ESL_disable: return "disable";
        case ESL_enbale: return "enable";
        default: break;
    }
    return "";
}

void str2Enum(E_TextureFilterType& t, const char *_s)
{
    std::string s = _s;
    if (s == "null") t = ETFT_null; 
    else if (s == "point") t = ETFT_point; 
    else if (s == "bilinear") t = ETFT_bilinear; 
    else if (s == "trilinear") t = ETFT_trilinear; 
    else if (s == "mipmapDbg") t = ETFT_mipmapDbg; 
    else assert(0);
}
void str2Enum(E_TextureAddressingMode &t, const char *_s)
{
    std::string s = _s;
    if (s == "clamp") t = ETAM_clamp; 
    else if (s == "repeat") t = ETAM_repeat; 
    else if (s == "mirror") t = ETAM_mirror; 
    else assert(0);
}
