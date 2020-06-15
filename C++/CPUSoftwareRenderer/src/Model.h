#ifndef MODEL_H
#define MODEL_H


#include <vector>
#include <map>

#include <assimp/Importer.hpp>
#include <assimp/scene.h>
#include <assimp/postprocess.h>
#include <stb/stb_image.h>

#include "Vector.h"
#include "BoundingBox.h"
#include "Texture.h"


struct Mesh
{
    std::vector<Vector3> Pos;
    std::vector<Vector3> Norm;
    std::vector<Vector2> UV;
    std::vector<int> Index;
    TexturePtr DiffMap; // diff(RGB) + spec(A)
    TexturePtr NormMap; // norm(RGB) + refl(A)
    Vector4 AmbientColor = Vector4(1);
    Vector4 DiffColor = Vector4(1);
    Vector4 SpecColor = Vector4(1);
    float Shininess = 64;
    AABB3 Bounds;
};

struct Model
{
    std::vector<Mesh> Meshes;
    AABB3 Bounds;

    bool Load(std::string const &path, float normGamma)
    {
        auto dir = path.substr(0, path.find_last_of('/'));


        Assimp::Importer importer;

        aiScene const * scene = importer.ReadFile(
            path, 
            aiProcess_Triangulate | aiProcess_FlipUVs | aiProcess_CalcTangentSpace | aiProcess_GenNormals);
        if (!scene || scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene->mRootNode)
        {
            fprintf(stderr, "%s\n", importer.GetErrorString());
            return false;
        }

        std::map<std::string, TexturePtr> textureCache;
        ProcessNode(
            normGamma,
            Meshes,
            scene->mRootNode, scene,
            dir, textureCache);

        Bounds = AABB3();
        for (auto &mesh : Meshes)
        {
            Bounds.AddPoint(mesh.Bounds.Min);
            Bounds.AddPoint(mesh.Bounds.Max);
        }

        return true;
    }

    void AddGroundMesh()
    {
        Mesh mesh;

        float scale = 1;
        Vector3 size = (Bounds.Max - Bounds.Min);
        float halfSize = std::max(size.Val[0], size.Val[2]);
        float y = Bounds.Min.Val[1];


        mesh.Pos = 
        {
            { -halfSize * scale, y, -halfSize * scale }, 
            { halfSize * scale, y, -halfSize * scale }, 
            { -halfSize * scale, y, halfSize * scale }, 
            { halfSize * scale, y, halfSize * scale }, 
        };
        mesh.Norm.assign(4, Vector3(0, 1, 0));
        mesh.UV.assign(4, Vector2(0));

        mesh.Index = std::vector<int>{ 3, 1, 0, 3, 0, 2 };

        Meshes.insert(Meshes.begin(), std::move(mesh));
    }

private:

    static void ProcessNode(
        float normGamma,
        std::vector<Mesh> &meshes,
        aiNode *node, const aiScene *scene, 
        std::string const &dir, std::map<std::string, TexturePtr> &textureCache)
    {
        for (unsigned int i = 0; i < node->mNumMeshes; i++)
        {
            meshes.push_back(
                ProcessMesh(
                    normGamma,
                    scene->mMeshes[node->mMeshes[i]], scene,
                    dir, textureCache));
        }
        for (unsigned int i = 0; i < node->mNumChildren; i++)
        {
            ProcessNode(
                normGamma,
                meshes, node->mChildren[i], scene, 
                dir, textureCache);
        }
    }

    static Mesh ProcessMesh(
        float normGamma,
        aiMesh *mesh, aiScene const *scene,
        std::string const &dir, std::map<std::string, TexturePtr> &textureCache)
    {
        Mesh res;

        for (unsigned int i = 0; i < mesh->mNumVertices; i++)
        {
            res.Pos.push_back({ mesh->mVertices[i].x , mesh->mVertices[i].y , mesh->mVertices[i].z });
            res.Norm.push_back(Vector3({ mesh->mNormals[i].x , mesh->mNormals[i].y , mesh->mNormals[i].z }).Normalize());
            Vector2 uv;
            if (mesh->mTextureCoords[0]) 
            {
                res.UV.push_back({ mesh->mTextureCoords[0][i].x , mesh->mTextureCoords[0][i].y });
            }
            else
            {
                res.UV.push_back(Vector2{ 0 });
            }
        }
        for (unsigned int i = 0; i < mesh->mNumFaces; i++)
        {
            aiFace face = mesh->mFaces[i];
            for (unsigned int j = 0; j < face.mNumIndices; j++)
                res.Index.push_back(face.mIndices[j]);
        }

        res.Bounds = AABB3();
        for (auto &pt : res.Pos)
            res.Bounds.AddPoint(pt);
        

        aiMaterial* material = scene->mMaterials[mesh->mMaterialIndex];

        // ambient is actually reflection map
        res.DiffMap = LoadMaterialTextures(material, aiTextureType_DIFFUSE, COMMON_GAMMA, dir, textureCache);
        res.NormMap = LoadMaterialTextures(material, aiTextureType_NORMALS, normGamma, dir, textureCache);
        TexturePtr specMap = LoadMaterialTextures(material, aiTextureType_SPECULAR, 1, dir, textureCache);
        TexturePtr reflMap = LoadMaterialTextures(material, aiTextureType_AMBIENT, 1, dir, textureCache);
        if (res.DiffMap != nullptr)
        {
            CombineMap(res.DiffMap, specMap, 0xff);
        }
        if (res.NormMap != nullptr)
        {
            CombineMap(res.NormMap, reflMap, 0x00);
        }

        aiColor3D clr;
        float f;
        if (aiReturn_SUCCESS == material->Get(AI_MATKEY_COLOR_AMBIENT, clr))
            res.AmbientColor = Vector4(clr.r, clr.g, clr.b, 1);
        if (aiReturn_SUCCESS == material->Get(AI_MATKEY_COLOR_DIFFUSE, clr))
            res.DiffColor = Vector4(clr.r, clr.g, clr.b, 1);
        if (aiReturn_SUCCESS == material->Get(AI_MATKEY_COLOR_SPECULAR, clr))
            res.SpecColor = Vector4(clr.r, clr.g, clr.b, 1);
        if (aiReturn_SUCCESS == material->Get(AI_MATKEY_SHININESS, f))
            res.Shininess = f;

        return res;
    }

    static TexturePtr LoadMaterialTextures(
        aiMaterial *mat, aiTextureType type, float gamma,
        std::string const &dir, std::map<std::string, TexturePtr> &textureCache)
    {
        for (unsigned int i = 0; i < mat->GetTextureCount(type); i++)
        {
            std::string fname;
            {
                aiString path;
                mat->GetTexture(type, i, &path);
                fname = path.C_Str();
                fname = fname.substr(fname.find_last_of('/') + 1);
                fname = fname.substr(fname.find_last_of('\\') + 1);
            }

            std::string path = dir + "/" + fname;

            TexturePtr texture;
            if (textureCache.count(path) > 0)
            {
                texture = textureCache[path];
            }
            else
            {
                int w, h;
                uint8_t *data = stbi_load(path.c_str(), &w, &h, nullptr, 4);
                if (data != nullptr)
                {
                    texture = std::make_shared<Texture>(w, h, (uint32_t*)data, gamma);
                    stbi_image_free(data);

                    textureCache[path] = texture;
                }
                else
                {
                    fprintf(stderr, "failed to load : %s\n", path.c_str());
                }
            }

            // TODO: return more than 1 texture
            return texture;
        }
        
        return nullptr;
    }

    static void CombineMap(TexturePtr const &rgbMap, TexturePtr const &aMap, uint32_t defAlpha)
    {
        RAssert(defAlpha >= 0 && defAlpha <= 255);

        int n = rgbMap->h * rgbMap->w;
        if (aMap != nullptr)
        {
            RAssert(aMap->h * aMap->w == n);
        }

        for (int i = 0; i < n; ++i)
        {
            uint32_t rgb = rgbMap->Data[i];
            uint32_t alpha = aMap == nullptr ? defAlpha : ((aMap->Data[i] >> 24) & 0xff);
            rgbMap->Data[i] = (rgb & 0xffffff00) | alpha;
        }
    }
};


#endif
