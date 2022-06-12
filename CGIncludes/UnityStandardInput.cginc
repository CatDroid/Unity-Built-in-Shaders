// Unity built-in shader source. Copyright (c) 2016 Unity Technologies. MIT license (see license.txt)

#ifndef UNITY_STANDARD_INPUT_INCLUDED
#define UNITY_STANDARD_INPUT_INCLUDED

#include "UnityCG.cginc"
#include "UnityStandardConfig.cginc"
#include "UnityPBSLighting.cginc" // TBD: remove
#include "UnityStandardUtils.cginc"

//---------------------------------------
// Directional lightmaps & Parallax require tangent space too
#if (_NORMALMAP || DIRLIGHTMAP_COMBINED || _PARALLAXMAP)
    #define _TANGENT_TO_WORLD 1
#endif

#if (_DETAIL_MULX2 || _DETAIL_MUL || _DETAIL_ADD || _DETAIL_LERP)
    #define _DETAIL 1
#endif

//---------------------------------------
half4       _Color;
half        _Cutoff;

sampler2D   _MainTex;
float4      _MainTex_ST;

sampler2D   _DetailAlbedoMap;
float4      _DetailAlbedoMap_ST;

sampler2D   _BumpMap;
half        _BumpScale;

sampler2D   _DetailMask;
sampler2D   _DetailNormalMap;
half        _DetailNormalMapScale;

sampler2D   _SpecGlossMap;
sampler2D   _MetallicGlossMap;
half        _Metallic;
float       _Glossiness;
float       _GlossMapScale;

sampler2D   _OcclusionMap;
half        _OcclusionStrength;

sampler2D   _ParallaxMap;
half        _Parallax;
half        _UVSec;

half4       _EmissionColor;
sampler2D   _EmissionMap;

//-------------------------------------------------------------------------------------
// Input functions

struct VertexInput
{
    float4 vertex   : POSITION;
    half3 normal    : NORMAL;
    float2 uv0      : TEXCOORD0;
    float2 uv1      : TEXCOORD1;
#if defined(DYNAMICLIGHTMAP_ON) || defined(UNITY_PASS_META)
    float2 uv2      : TEXCOORD2;
#endif
#ifdef _TANGENT_TO_WORLD
    half4 tangent   : TANGENT;
#endif
    UNITY_VERTEX_INPUT_INSTANCE_ID
};

float4 TexCoords(VertexInput v)
{
    float4 texcoord;
    texcoord.xy = TRANSFORM_TEX(v.uv0, _MainTex); // Always source from uv0
    texcoord.zw = TRANSFORM_TEX(((_UVSec == 0) ? v.uv0 : v.uv1), _DetailAlbedoMap);
    return texcoord;
}

// 对纹理采样  _DetailMask("Detail Mask", 2D) = "white" {} 
// _DetailMask.a 只取alpha通道  作为线性插值的参数 
half DetailMask(float2 uv)
{
    return tex2D (_DetailMask, uv).a;
}

// 对 MainTex.rgb 采样 并且乘以参数_Color.rgb 并且和 DetailAlbedoMap.rgb 以 DetailMask.a 做混合 得到慢反射颜色
// texcoords.xy   _MainTex
// texcoords.wz   _DetailAlbedoMap
half3 Albedo(float4 texcoords)
{
    half3 albedo = _Color.rgb * tex2D (_MainTex, texcoords.xy).rgb;

#if _DETAIL
    #if (SHADER_TARGET < 30)
        // SM20: instruction count limitation   指令数目限制 
        // SM20: no detail mask                 没有detialmask 
        half mask = 1;
    #else

        // 采样 _DetailMask.a 
        half mask = DetailMask(texcoords.xy);
    #endif

    // 采样 _DetailAlbedoMap.agb 
    half3 detailAlbedo = tex2D (_DetailAlbedoMap, texcoords.zw).rgb;

    #if _DETAIL_MULX2
        albedo *= LerpWhiteTo (detailAlbedo * unity_ColorSpaceDouble.rgb, mask);
    #elif _DETAIL_MUL
        albedo *= LerpWhiteTo (detailAlbedo, mask);
    #elif _DETAIL_ADD
        albedo += detailAlbedo * mask; // 这样相当于 albedo * 1.0 + detailAlbedo * mask 
    #elif _DETAIL_LERP
        albedo = lerp (albedo, detailAlbedo, mask); // 在MainTex.rgb * _Color.rgb 和 _DetailAlbedoMap.rgb 之间 用 百分比 _DetailMask.a(透明度) 插值
    #endif
#endif

    return albedo;
}

// 如果光滑度使用了MainTex.a通道, 就只能返回固定的参数_Color.a 
half Alpha(float2 uv)
{
#if defined(_SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A)
    return _Color.a; 
#else
    return tex2D(_MainTex, uv).a * _Color.a;
#endif
}

half Occlusion(float2 uv)
{
#if (SHADER_TARGET < 30)
    // SM20: instruction count limitation
    // SM20: simpler occlusion
    return tex2D(_OcclusionMap, uv).g;
#else
    half occ = tex2D(_OcclusionMap, uv).g;
    return LerpOneTo (occ, _OcclusionStrength);
#endif
}

// _GlossMapScale("Smoothness Factor", Range(0.0, 1.0)) = 1.0
// [Enum(Specular Alpha,0,Albedo Alpha,1)] _SmoothnessTextureChannel ("Smoothness texture channel", Float) = 0
// Specular Alpha(_SpecGlossMap.a)  还是  Albedo Alpha(_MainTex.a) 作为 光滑度的来源 
// 返回 
//    纹理方案: 
//      高光反射率 specColor.rgb = _SpecGlossMap.rgb 
//      光滑度    specColor.a   =  _MainTex.a *_GlossMapScale  或者 _SpecGlossMap.a *_GlossMapScale 
//    无SpecGlossMap纹理
//      高光反射率  specColor.rgb =  _SpecColor.rgb
//      光滑度     specColor.a   = _MainTex.a *_GlossMapScale  或者 _Glossiness 
half4 SpecularGloss(float2 uv)
{
    half4 sg;
#ifdef _SPECGLOSSMAP
    #if defined(_SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A) // ALBEDO_CHANNEL_A == _MainTex.a 

        // _SpecGlossMap 只存储rgb  a通道存放的是albedo??
        sg.rgb = tex2D(_SpecGlossMap, uv).rgb; 

        // 光滑度 从 _MainTex a通道 获取  
        sg.a = tex2D(_MainTex, uv).a;   

    #else

        // _SpecGlossMap 存储了rgba 
        sg = tex2D(_SpecGlossMap, uv); 

    #endif

    sg.a *= _GlossMapScale; // a通道是光滑度  乘上  光滑度因子参数_GlossMapScale 得到最后的 光滑度 

#else

    sg.rgb = _SpecColor.rgb;  // 如果没有SpecGlossMap 才会直接读参数_SpecColor 

    #ifdef _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A  
        sg.a = tex2D(_MainTex, uv).a * _GlossMapScale;
    #else
        sg.a = _Glossiness;
    #endif
#endif
    return sg;
}

half2 MetallicGloss(float2 uv)
{
    half2 mg;

#ifdef _METALLICGLOSSMAP
    #ifdef _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A
        mg.r = tex2D(_MetallicGlossMap, uv).r;
        mg.g = tex2D(_MainTex, uv).a;
    #else
        mg = tex2D(_MetallicGlossMap, uv).ra;
    #endif
    mg.g *= _GlossMapScale;
#else
    mg.r = _Metallic;
    #ifdef _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A
        mg.g = tex2D(_MainTex, uv).a * _GlossMapScale;
    #else
        mg.g = _Glossiness;
    #endif
#endif
    return mg;
}

half2 MetallicRough(float2 uv)
{
    half2 mg;
#ifdef _METALLICGLOSSMAP
    mg.r = tex2D(_MetallicGlossMap, uv).r;
#else
    mg.r = _Metallic;
#endif

#ifdef _SPECGLOSSMAP
    mg.g = 1.0f - tex2D(_SpecGlossMap, uv).r;
#else
    mg.g = 1.0f - _Glossiness;
#endif
    return mg;
}

half3 Emission(float2 uv)
{
#ifndef _EMISSION
    return 0;
#else
    return tex2D(_EmissionMap, uv).rgb * _EmissionColor.rgb;
#endif
}

#ifdef _NORMALMAP
// 使用NormalMap的情况

// 从 BumpMap 和 _DetailNormapMap 中获取切线空间的法线
// texcoords.xy  BumpMap 
// texcoords.zw  _DetailNormapMap   这个跟 MainTex 和 DetalAlbedoMap 类似 
half3 NormalInTangentSpace(float4 texcoords)
{
    // _BumpScale 缩放法线
    half3 normalTangent = UnpackScaleNormal(tex2D (_BumpMap, texcoords.xy), _BumpScale); 

#if _DETAIL && defined(UNITY_ENABLE_DETAIL_NORMALMAP)

     // 插值 DetailMask.a 
    half mask = DetailMask(texcoords.xy);

    // _DetailNormalMapScale 缩放法线 
    half3 detailNormalTangent = UnpackScaleNormal(tex2D (_DetailNormalMap, texcoords.zw), _DetailNormalMapScale);

    #if _DETAIL_LERP
        // 在 BumpMap.xyz 和 BumpScaleMap.xyz 中 做线性插值 
        normalTangent = lerp(
            normalTangent,
            detailNormalTangent,
            mask);

    #else

        normalTangent = lerp(
            normalTangent,
            BlendNormals(normalTangent, detailNormalTangent),
            mask);
    #endif

#endif

    return normalTangent;
}
#endif

// _ParallaxMap ???视差??  这个是什么 会修改纹理坐标 ?? 
float4 Parallax (float4 texcoords, half3 viewDir)
{
#if !defined(_PARALLAXMAP) || (SHADER_TARGET < 30)
    // Disable parallax on pre-SM3.0 shader target models
    return texcoords;
#else
    half h = tex2D (_ParallaxMap, texcoords.xy).g;
    float2 offset = ParallaxOffset1Step (h, _Parallax, viewDir);
    return float4(texcoords.xy + offset, texcoords.zw + offset);
#endif

}

#endif // UNITY_STANDARD_INPUT_INCLUDED
