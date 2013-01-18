#include "pch.h"

#include "RenderStateEnums.h"

const char* enum2Str(E_ZBufferType t)
{
    switch (t) {
        case EZBT_null: return "ZBuffer_null";
        case EZBT_zbuf: return "ZBuffer_zbuf";
        case EZBT_1zbuf: return "ZBuffer_invzbuf";
        default: break;
    }
    return "";
}
const char* enum2Str(E_ShadeMode t)
{
    switch (t) {
        case ESM_frame: return "ShadeMode_frame";
        case ESM_const: return "ShadeMode_const";
        case ESM_flat: return "ShadeMode_flat";
        case ESM_gouraud: return "ShadeMode_gouraud";
        case ESM_phong: return "ShadeMode_phong";
        default: break;
    }
    return "";
}
const char* enum2Str(E_TextureFilterType t)
{
    switch (t) {
        case ETFT_null: return "TextureFilterType_null";
        case ETFT_point: return "TextureFilterType_point";
        case ETFT_bilinear: return "TextureFilterType_bilinear";
        case ETFT_trilinear: return "TextureFilterType_trilinear";
        case ETFT_mipmapDbg: return "TextureFilterType_mipmapDbg";
        default: break;
    }
    return "";
}
const char* enum2Str(E_TextureAddressingMode t)
{
    switch (t) {
        case ETAM_clamp: return "TextureAddressingMode_Clamp";
        case ETAM_repeat: return "TextureAddressingMode_Repeat";
        case ETAM_mirror: return "TextureAddressingMode_Mirror";
        default: break;
    }
    return "";
}
const char* enum2Str(E_CullFace t)
{
    switch (t) {
        case ECF_null: return "CullFace_null";
        case ECF_back: return "CullFace_back";
        case ECF_front: return "CullFace_front";
        default: break;
    }
    return "";
}

const char* enum2Str(E_ZSortType t)
{
    switch (t) {
        case EZST_null: return "ZSortType_null";
        case EZST_near2Far: return "ZSortType_near2Far";
        case EZST_far2Near: return "ZSortType_far2Near";
        default: break;
    }
    return "";
}

const char* enum2Str(E_SpecularLight t)
{
    switch (t) {
        case ESL_disable: return "SpecularLight_disable";
        case ESL_enbale: return "SpecularLight_enable";
        default: break;
    }
    return "";
}
