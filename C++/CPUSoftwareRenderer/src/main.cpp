#include <cstdio>

#include <vector>
#include <chrono>
#include <fstream>

#if _WIN32
#include <Windows.h>
#undef min
#undef max
#endif

#include "Matrix.h"
#include "Rasterizer.h"
#include "Texture.h"
#include "Model.h"
#include "MainLoop.h"


struct Config
{
    int ThreadCount;
    int WindowW;
    int WindowH;
    int SampleCount;
    bool PerSampleShading;
    int ShadowMapScale;
    std::string AssetRoot;

    bool ParseFile(std::string const &path)
    {
        std::ifstream file(path);
        std::string key, value;
        for (;;)
        {
            if (!(file >> key))
                break;
            if (key[0] == '#')
                continue;
            if (!(file >> value))
                break;

            if (key == "ThreadCount")
                ThreadCount = std::stoi(value);
            else if (key == "WindowWidth")
                WindowW = std::stoi(value);
            else if (key == "WindowHeight")
                WindowH = std::stoi(value);
            else if (key == "SampleCount")
                SampleCount = std::stoi(value);
            else if (key == "PerSampleShading")
                PerSampleShading = std::stoi(value) != 0;
            else if (key == "ShadowMapScale")
                ShadowMapScale = std::stoi(value);
            else if (key == "AssetRoot")
                AssetRoot = value;
            else
            {
                fprintf(stderr, "Unknown config : %s\n", key.c_str());
                return false;
            } 
        }
        return true;
    }

    std::string GetModelPath(std::string const &relatPath)
    {
        return AssetRoot + "/Model/" + relatPath;
    }

    std::string GetCubemapPath(std::string const &relatPath)
    {
        return AssetRoot + "/Cubemap/" + relatPath;
    }


} gCfg;


static int TestColoredTriangle()
{
    float rotate = 0;

    std::vector<float> zBuf;

    return MainLoop(
        gCfg.WindowW, gCfg.WindowH,
        [&](uint32_t *fBuf, float elapse)
    {
        std::fill(fBuf, fBuf + gCfg.WindowW * gCfg.WindowH, 0xa0a0a0ff);
        zBuf.assign(gCfg.WindowW * gCfg.WindowH, 0.f);

        rotate += elapse * 30;

        Vector3 pts[] = 
        { 
            {-0.8f, 0.8f, 0.f},
            {0.8f, 0.8f, 0.f},
            {0.f, -0.8f, 0.f},
        };;
        int indices[] = {0, 1, 2, };

        Matrix<3, 4> clrs =
        {
            1, 0, 0, 1,
            0, 1, 0, 1,
            0, 0, 1, 1,
        };

        Vector4 clipPts[3];

        auto trans =
            Matrix4::Rotate(Degree{ rotate }, Vector3(0, 1, 0).Normalize()) *
            Matrix4::Translate(Vector3{ 0, 0, -5 }) *
            Matrix4::Orthographic(-1, 1, 1, -1, 1, 10);
        
        DrawMesh(
            1, 
            1, false, 
            false,
            gCfg.WindowW, gCfg.WindowH, fBuf, zBuf.data(),
            { 1 },
            [&](Vector4 clipPts[3], int meshId, int triId)
            {
                for (int i = 0; i < 3; ++i)
                {
                    clipPts[i] = Vector4(pts[i], 1) * trans;
                }
                return 0;
            },
            [&](Vector3 const &bc, int attr)
            {
                return EncodeRGBA32(bc * clrs);
            });

    }) ? 0 : 1;
}

static int TestPerspectiveInterpolation()
{
    std::vector<float> zBuf;

    return MainLoop(
        gCfg.WindowW, gCfg.WindowH,
        [&](uint32_t *fBuf, float elapse)
    {
        std::fill(fBuf, fBuf + gCfg.WindowW * gCfg.WindowH, 0xa0a0a0ff);
        zBuf.assign(gCfg.WindowW * gCfg.WindowH, 0.f);

        int constexpr kFace = 2;

        Vector3 pts[][3] =
        {
            {
                {-0.8f, 0.8f, 0.f},
                {-0.8f, -0.8f, 0.f},
                {0.8f, 0.8f, 0.f},
            },
            {
                {0.8f, 0.8f, 0.f},
                {-0.8f, -0.8f, 0.f},
                {0.8f, -0.8f, 0.f},
            }
        };
        Matrix<3, 2> uv[] =
        {
            {
               0, 0,
               0, 1,
               1, 0,
            },
            {
               1, 0,
               0, 1,
               1, 1,
            }
        };
        int indices[] = 
        {
            0, 1, 2, 3, 4, 5,
        };

        auto trans =
            Matrix4::Rotate(Degree{ -75 }, Vector3(1, 0, 0).Normalize()) *
            Matrix4::Translate(Vector3{ 0, 0, -2 }) *
            Matrix4::Perspective(Degree{ 60.f }, float(gCfg.WindowW) / gCfg.WindowH, 0.1f, 5.f);

        DrawMesh(
            1,
            1, false,
            false,
            gCfg.WindowW, gCfg.WindowH, fBuf, zBuf.data(),
            { kFace },
            [&](Vector4 clipPts[3], int meshId, int triId)
            {
                for (int i = 0; i < 3; ++i)
                {
                    clipPts[i] = Vector4(pts[triId][i], 1) * trans;
                }

                return uv[triId];
            },
            [&](Vector3 const &bc, Matrix<3, 2> const &uvMat)
            {
                auto uv = bc * uvMat;
                int color =
                    int(std::floor(uv.Val[0] * 30)) % 2 ^
                    int(std::floor(uv.Val[1] * 30)) % 2;

                return color == 1 ? -1 : 0xff;
            });

    }) ? 0 : 1;
}

static int TestSimpleModel()
{
    Model model;
    model.Load(gCfg.GetModelPath("nanosuit/nanosuit.obj"), COMMON_GAMMA);
    // model.Load(gCfg.GetModelPath("cyborg/cyborg.obj"), 1);
    // model.Load(gCfg.GetModelPath("elizabeth-from-bioshock-infinite-burial-at-sea/0.obj"), 1);
    // model.Load(gCfg.GetModelPath("cyberdemon-from-doom-2016/0.obj"), 1);
    // model.Load(gCfg.GetModelPath("dragon/dragon.obj"), 1);
    model.AddGroundMesh();

    std::vector<int> triCounts;
    {
        int totalTriCount = 0, totalVertCount = 0;
        for (auto &mesh : model.Meshes)
        {
            int triCount = (int)mesh.Index.size() / 3;
            triCounts.push_back(triCount);
            totalTriCount += triCount;
            totalVertCount += (int)mesh.Pos.size();
        }
        printf("triangle count: %d, vertex count : %d\n", totalTriCount, totalVertCount);
    }

    std::vector<float> zBuf(gCfg.WindowW * gCfg.WindowH * gCfg.SampleCount);
    std::vector<uint32_t> ssFbuf(gCfg.WindowW * gCfg.WindowH * gCfg.SampleCount);

    int shadowMapW = gCfg.WindowW * gCfg.ShadowMapScale;
    int shadowMapH = gCfg.WindowH * gCfg.ShadowMapScale;
    std::vector<float> shadowMap(shadowMapH * shadowMapW);

    struct TriangleAttr
    {
        Matrix<3, 4> ViewPtMat;
        Matrix<3, 4> NormMat;
    };

    float rotateSpeed = 20;
    float rotateAngle = 0;
    float totalElapse = 0;
    int totalFrame = 0;

    return MainLoop(
        gCfg.WindowW, gCfg.WindowH,
        [&](uint32_t *finalFBuf, float elapse)
    {
        totalFrame++;
        totalElapse += elapse;
        if (totalElapse > 1)
        {
            printf("FPS = %.1f\n", totalFrame / totalElapse);
            totalFrame = 0;
            totalElapse = 0;
        }


        double t0 = 0, t1 = 0;
        uint32_t *fBuf;
        if (gCfg.SampleCount == 1)
        {
            ClearBuffer(gCfg.ThreadCount, finalFBuf, gCfg.WindowW * gCfg.WindowH, 0xa0a0a0ff);
            fBuf = finalFBuf;
        }
        else
        {
            ClearBuffer(gCfg.ThreadCount, ssFbuf.data(), (int)ssFbuf.size(), 0xa0a0a0ff);
            fBuf = ssFbuf.data();
        }
        ClearBuffer(gCfg.ThreadCount, zBuf.data(), (int)zBuf.size(), 0.f);


        rotateAngle += elapse * rotateSpeed;

        auto lightPos = Vector3(20, 20, 10);
        auto lightPos4 = Vector4(lightPos, 1);

        auto modelRotate = Degree{ rotateAngle };
        auto modelScale = Vector3(10 / (model.Bounds.Max.Val[1] - model.Bounds.Min.Val[1]));
        auto modelPos = Vector3(0, -6, -10);
        auto rotMat = Matrix4::Rotate(Degree{ rotateAngle }, Vector3(0, 1, 0));
        auto modMat =
            rotMat * 
            Matrix4::Scale(modelScale) *
            Matrix4::Translate(modelPos);
        Matrix4 lightMvpMat;

        // render to shadow map
        {
            auto lightViewMat = Matrix4::LookAt(lightPos, modelPos, Vector3(0, 1, 0));
            auto newBounds = Transform(model.Bounds, modMat * lightViewMat);
            auto pMat = Matrix4::Perspective(
                Degree{ 30 }, float(shadowMapW) / shadowMapH, -newBounds.Max.Val[2] * 0.9f, -newBounds.Min.Val[2] * 1.1f);
            lightMvpMat =  modMat * lightViewMat * pMat;

            ClearBuffer(gCfg.ThreadCount, shadowMap.data(), (int)shadowMap.size(), 0.f);

            DrawMesh(
                gCfg.ThreadCount,
                1, false,
                true,
                shadowMapW, shadowMapH,
                nullptr, shadowMap.data(),
                triCounts,
                [&](Vector4 clipPts[3], int meshId, int triId)
                {
                    auto &mesh = model.Meshes[meshId];
                    for (int i = 0; i < 3; ++i)
                    {
                        int idx = mesh.Index[triId * 3 + i];
                        clipPts[i] = Vector4(mesh.Pos[idx], 1) * lightMvpMat;
                    }
                    return 0;
                },
                [](Vector3 const &bc, int attr)
                {
                    return 0;
                });
        }

        // render to frame buffer
        {
            auto mvMat = Matrix4::Identity() * modMat;
            auto pMat = Matrix4::Perspective(Degree{ 60 }, float(gCfg.WindowW) / gCfg.WindowH, 0.1f, 40);
            auto mvpMat = mvMat * pMat;
            auto v2LightPMat = mvMat.Inverse() * lightMvpMat;

            DrawMesh(
                gCfg.ThreadCount,
                gCfg.SampleCount, gCfg.PerSampleShading,
                true,
                gCfg.WindowW, gCfg.WindowH,
                fBuf, zBuf.data(),
                triCounts,
                [&](Vector4 clipPts[3], int meshId, int triId)
                {
                    auto &mesh = model.Meshes[meshId];

                    Vector4 viewPts[3];
                    Vector4 viewNorms[3];
                    for (int i = 0; i < 3; ++i)
                    {
                        int idx = mesh.Index[triId * 3 + i];
                        viewPts[i] = Vector4(mesh.Pos[idx], 1) * mvMat;
                        clipPts[i] = viewPts[i] * pMat;
                        viewNorms[i] = (Vector4(mesh.Norm[idx], 0) * rotMat).Normalize();
                    }

                    return TriangleAttr
                    {
                        *(Matrix<3, 4>*)viewPts, *(Matrix<3, 4>*)viewNorms,
                    };
                },
                [&](Vector3 const &bc, TriangleAttr const &attr)
                {
                    Vector4 viewPos = bc * attr.ViewPtMat;
                    Vector4 lightDir = (lightPos4 - viewPos).Normalize();

                    Vector4 tex(1);
                    Vector4 norm = bc * attr.NormMat;

                    Vector4 ambient = tex * 0.05f;
                    float nDL = norm.Dot(lightDir);
                    Vector4 diffuse = tex * std::max(0.f, nDL);

                    float shadow;
                    {
                        Vector4 clipPt = viewPos * v2LightPMat;
                        Vector4 ndcPt = clipPt * (1 / clipPt.Val[3]);
                        // NEAREST 
                        int x = Clamp(int((ndcPt.Val[0] * 0.5f + 0.5f) * shadowMapW), 0, shadowMapW - 1);
                        int y = Clamp(int((ndcPt.Val[1] * -0.5f + 0.5f) * shadowMapH), 0, shadowMapH - 1);
                        float z = ndcPt.Val[2];
                        float bias = 1.5e-2f;

                        shadow = 0;
                        int sampleCount = 0;
                        for (int yOff = -1; yOff <= 1; ++yOff)
                        {
                            for (int xOff = -1; xOff <= 1; ++xOff)
                            {
                                int x0 = Clamp(x + xOff, 0, shadowMapW - 1);
                                int y0 = Clamp(y + yOff, 0, shadowMapH - 1);
                                float frontZ = shadowMap[y0 * shadowMapW + x0];
                                shadow += frontZ - z > bias ? 1 : 0;
                                ++sampleCount;
                            }
                        }
                        shadow /= sampleCount;
                    }

                    Vector4 finalClr = ambient + diffuse * (1 - shadow);
                    finalClr = GammaEncode(ExposureToneMapping(finalClr, 4), COMMON_GAMMA);
                    return EncodeRGBA32(finalClr);
                });

            if (gCfg.SampleCount > 1)
            {
                ResolveMultiSampleBuffer(
                    gCfg.ThreadCount, gCfg.SampleCount,
                    gCfg.WindowW, gCfg.WindowH, finalFBuf, fBuf);
            }
        }

    }) ? 0 : 1;
}

static int TestTexturedModel()
{
    Model model;
    model.Load(gCfg.GetModelPath("nanosuit/nanosuit.obj"), COMMON_GAMMA);
    // model.Load(gCfg.GetModelPath("cyborg/cyborg.obj"), 1);
    // model.Load(gCfg.GetModelPath("elizabeth-from-bioshock-infinite-burial-at-sea/0.obj"), 1);
    // model.Load(gCfg.GetModelPath("cyberdemon-from-doom-2016/0.obj"), 1);
    model.AddGroundMesh();

    std::vector<int> triCounts;
    {
        int totalTriCount = 0, totalVertCount = 0;
        for (auto &mesh : model.Meshes)
        {
            int triCount = (int)mesh.Index.size() / 3;
            triCounts.push_back(triCount);
            totalTriCount += triCount;
            totalVertCount += (int)mesh.Pos.size();
        }
        printf("triangle count: %d, vertex count : %d\n", totalTriCount, totalVertCount);
    }


    Cubemap cubeMap;
    cubeMap.Load(gCfg.GetCubemapPath("lake"), COMMON_GAMMA);

    std::vector<float> zBuf(gCfg.WindowW * gCfg.WindowH * gCfg.SampleCount);
    std::vector<uint32_t> ssFbuf(gCfg.WindowW * gCfg.WindowH * gCfg.SampleCount);

    int shadowMapW = gCfg.WindowW * gCfg.ShadowMapScale;
    int shadowMapH = gCfg.WindowH * gCfg.ShadowMapScale;
    std::vector<float> shadowMap(shadowMapH * shadowMapW);

    struct TriangleAttr
    {
        Vector4 Tangent;
        Matrix<3, 2> UVMat;
        Matrix<3, 4> ViewPtMat;
        Matrix<3, 4> NormMat;
        int MeshId;
    };

    float rotateSpeed = 20;
    float rotateAngle = 0;
    float totalElapse = 0;
    int totalFrame = 0;

    return MainLoop(
        gCfg.WindowW, gCfg.WindowH,
        [&](uint32_t *finalFBuf, float elapse)
    {
        totalFrame++;
        totalElapse += elapse;
        if (totalElapse > 1)
        {
            printf("FPS = %.1f\n", totalFrame / totalElapse);
            totalFrame = 0;
            totalElapse = 0;
        }


        double t0 = 0, t1 = 0;
        uint32_t *fBuf;
        if (gCfg.SampleCount == 1)
        {
            ClearBuffer(gCfg.ThreadCount, finalFBuf, gCfg.WindowW * gCfg.WindowH, 0xa0a0a0ff);
            fBuf = finalFBuf;
        }
        else
        {
            ClearBuffer(gCfg.ThreadCount, ssFbuf.data(), (int)ssFbuf.size(), 0xa0a0a0ff);
            fBuf = ssFbuf.data();
        }
        ClearBuffer(gCfg.ThreadCount, zBuf.data(), (int)zBuf.size(), 0.f);


        rotateAngle += elapse * rotateSpeed;

        auto lightPos = Vector3(20, 20, 10);
        auto lightPos4 = Vector4(lightPos, 1);

        auto modelRotate = Degree{ rotateAngle };
        auto modelScale = Vector3(10 / (model.Bounds.Max.Val[1] - model.Bounds.Min.Val[1]));
        auto modelPos = Vector3(0, -6, -10);
        auto rotMat = Matrix4::Rotate(Degree{ rotateAngle }, Vector3(0, 1, 0));
        auto modMat =
            rotMat *
            Matrix4::Scale(modelScale) *
            Matrix4::Translate(modelPos);
        Matrix4 lightMvpMat;

        // render to shadow map
        {
            auto lightViewMat = Matrix4::LookAt(lightPos, modelPos, Vector3(0, 1, 0));
            auto newBounds = Transform(model.Bounds, modMat * lightViewMat);
            auto pMat = Matrix4::Perspective(
                Degree{ 30 }, float(shadowMapW) / shadowMapH, -newBounds.Max.Val[2] * 0.9f, -newBounds.Min.Val[2] * 1.1f);
            lightMvpMat = modMat * lightViewMat * pMat;

            ClearBuffer(gCfg.ThreadCount, shadowMap.data(), (int)shadowMap.size(), 0.f);

            DrawMesh(
                gCfg.ThreadCount,
                1, false,
                true,
                shadowMapW, shadowMapH,
                nullptr, shadowMap.data(), 
                triCounts,
                [&](Vector4 clipPts[3], int meshId, int triId)
                {
                    auto &mesh = model.Meshes[meshId];
                    for (int i = 0; i < 3; ++i)
                    {
                        int idx = mesh.Index[triId * 3 + i];
                        clipPts[i] = Vector4(mesh.Pos[idx], 1) * lightMvpMat;
                    }
                    return 0;
                },
                [](Vector3 const &bc, int attr)
                {
                    return 0;
                });
        }

        // render to frame buffer
        {
            auto mvMat = Matrix4::Identity() * modMat;
            auto pMat = Matrix4::Perspective(Degree{ 60 }, float(gCfg.WindowW) / gCfg.WindowH, 0.1f, 40);
            auto mvpMat = mvMat * pMat;
            auto v2LightPMat = mvMat.Inverse() * lightMvpMat;

            DrawMesh(
                gCfg.ThreadCount,
                gCfg.SampleCount, gCfg.PerSampleShading,
                true,
                gCfg.WindowW, gCfg.WindowH,
                fBuf, zBuf.data(), 
                triCounts,
                [&](Vector4 clipPts[3], int meshId, int triId)
                {
                    auto &mesh = model.Meshes[meshId];

                    Vector2 uv[3];
                    Vector4 viewPts[3];
                    Vector4 viewNorms[3];
                    for (int i = 0; i < 3; ++i)
                    {
                        int idx = mesh.Index[triId * 3 + i];
                        uv[i] = mesh.UV[idx];
                        viewPts[i] = Vector4(mesh.Pos[idx], 1) * mvMat;
                        clipPts[i] = viewPts[i] * pMat;
                        viewNorms[i] = (Vector4(mesh.Norm[idx], 0) * rotMat).Normalize();
                    }

                    Vector4 trTangent;
                    {
                        Vector2 uvMat[2] = { uv[1] - uv[0], uv[2] - uv[0], };
                        Vector2 invUVRow0 = (*(Matrix<2, 2>*)&uvMat).Inverse().Row(0);
                        Vector4 dirMat[2] = { viewPts[1] - viewPts[0], viewPts[2] - viewPts[0], };
                        trTangent = 
                            (dirMat[0] * invUVRow0.Val[0] + dirMat[1] * invUVRow0.Val[1])
                            .Normalize();
                    }

                    return TriangleAttr
                    {
                        trTangent,
                        *(Matrix<3, 2>*)&uv, *(Matrix<3, 4>*)viewPts, *(Matrix<3, 4>*)viewNorms,
                        meshId,
                    };
                },
                [&](Vector3 const &bc, TriangleAttr const &attr)
                {
                    auto &mesh = model.Meshes[attr.MeshId];

                    auto uv = bc * attr.UVMat;
                    Vector4 viewPos = bc * attr.ViewPtMat;
                    Vector4 eyeDir = (Vector4(0) - viewPos).Normalize();
                    Vector4 lightDir = (lightPos4 - viewPos).Normalize();

                    Vector4 tex(1);
                    float specFactor = 0;
                    if (mesh.DiffMap != nullptr)
                    {
                        auto clr = mesh.DiffMap->Sample(uv);
                        tex = clr * mesh.DiffColor;
                        specFactor = clr.Val[3];
                    }
                    Vector4 norm = bc * attr.NormMat;
                    float reflFactor = 0;
                    if (mesh.NormMap != nullptr)
                    {
                        auto tangent = attr.Tangent;
                        tangent = (tangent - norm * (norm.Dot(tangent))).Normalize();
                        auto biTangent = tangent.Cross(norm);

                        Vector4 clr = mesh.NormMap->Sample(uv);
                        auto texNorm = (clr * 2.f + (-1));
                        norm = (tangent * texNorm.Val[0] + 
                                biTangent * texNorm.Val[1] + 
                                norm * texNorm.Val[2])
                            .Normalize();

                        reflFactor = clr.Val[3];
                    }
                    if (reflFactor > 0)
                    {
                        float eyeDn = eyeDir.Dot(norm);
                        reflFactor *= Pow3(1 - std::max(0.f, eyeDn));
                        if (reflFactor > 0.1)
                        {
                            auto reflEye = Reflect(eyeDir, norm);
                            auto clr = cubeMap.Sample(reflEye);

                            tex = Lerp(tex, clr, reflFactor);
                        }
                    }

                    Vector4 ambient = tex * 0.05f;
                    float nDL = norm.Dot(lightDir);
                    Vector4 diffuse = tex * std::max(0.f, nDL);
                    Vector4 halfWay = (eyeDir + lightDir).Normalize();
                    Vector4 spec =
                        nDL < 0
                        ? Vector4(0)
                        : Vector4(Pow(std::max(0.f, halfWay.Dot(norm)), mesh.Shininess) * specFactor);

                    float shadow;
                    {
                        Vector4 clipPt = viewPos * v2LightPMat;
                        Vector4 ndcPt = clipPt * (1 / clipPt.Val[3]);
                        // NEAREST 
                        int x = Clamp(int((ndcPt.Val[0] * 0.5f + 0.5f) * shadowMapW), 0, shadowMapW - 1);
                        int y = Clamp(int((ndcPt.Val[1] * -0.5f + 0.5f) * shadowMapH), 0, shadowMapH - 1);
                        float z = ndcPt.Val[2];
                        float bias = 1.5e-2f;

                        shadow = 0;
                        int sampleCount = 0;
                        for (int yOff = -1; yOff <= 1; ++yOff)
                        {
                            for (int xOff = -1; xOff <= 1; ++xOff)
                            {
                                int x0 = Clamp(x + xOff, 0, shadowMapW - 1);
                                int y0 = Clamp(y + yOff, 0, shadowMapH - 1);
                                float frontZ = shadowMap[y0 * shadowMapW + x0];
                                shadow += frontZ - z > bias ? 1 : 0;
                                ++sampleCount;
                            }
                        }
                        shadow /= sampleCount;
                    }

                    Vector4 finalClr = ambient + (diffuse + spec) * (1 - shadow);
                    finalClr = GammaEncode(ExposureToneMapping(finalClr, 4), COMMON_GAMMA);
                    return EncodeRGBA32(finalClr);
                });

            if (gCfg.SampleCount > 1)
            {
                ResolveMultiSampleBuffer(
                    gCfg.ThreadCount, gCfg.SampleCount, 
                    gCfg.WindowW, gCfg.WindowH, finalFBuf, fBuf);
            }
        }

    }) ? 0 : 1;
}

#if _WIN32
int WinMain(HINSTANCE, HINSTANCE, char *argv, int argc)
{
    if (!CreateConsole())
        return 1;
#else
int main()
{
#endif

    if (!gCfg.ParseFile("config.txt"))
        return 1;

    // return TestColoredTriangle();
    // return TestPerspectiveInterpolation();
    return TestSimpleModel();
    // return TestTexturedModel();
}
